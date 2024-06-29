;;; org-llm.el --- Integrate LLMs with org-mode -*- lexical-binding: t; -*-

;; Copyright © 2024 Hraban Luyat

;; Author: Hraban Luyat <hraban@0brg.net>
;; Keywords: lisp org html export
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (llm "0.12.1") (dash "20240103.1301"))
;; URL:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, version 3 of the License.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple and to-the-point

;;; Code:

(require 'org)
(require 'org-element)

(require 'dash)
(require 'llm)

(defcustom org-llm/provider nil
  "An LLM instance from the llm package")


;;;;; EXAMPLE BLOCKS

;;;###autoload
(defun org-llm/send-block ()
  "Send a single example block to the LLM"
  (interactive)
  (let ((block (org-element-context)))
    (unless (eq 'example-block (org-element-type block))
      (user-error "Can only send example blocks to gpt4"))
    (hly/with-org-props (value) block
                        (let ((results (org-llm//insert-results-block block)))
                          (org-llm//send value results)))))

(defun org-llm//insert-results-block (prompt-block)
  "Create a new results block after this block and return a mark
to it.

Start writing at the mark to add content to the block. Don’t
forget to let the mark garbage collect when you’re done.
"
  (save-excursion
    (goto-char (org-element-property :end prompt-block))
    ;; Org considers any stretch of following empty lines to be part of the
    ;; block. Rewind.
    (while (looking-at "^")
      (backward-char))
    (newline)
    (newline)
    (insert "#+results:\n#+begin_example\n\n#+end_example\n")
    (forward-line -2)
    ;; This is where you can insert new text.
    (copy-marker (point) t)))


;;;;; CONVERSATION TREES

(defun org-llm//send (msg results-mark)
  (let ((prompt (llm-make-simple-chat-prompt msg)))
    (llm-chat-streaming-to-point
     org-llm/provider
     prompt
     (current-buffer)
     results-mark
     (lambda (&rest _)))))

(defun org-llm//previous-heading ()
  "Like ‘org-previous-visible-heading’ but indiciates whether the point changed.

Returns T if you traveled, NIL if not."
  (let ((old (point)))
    (org-previous-visible-heading 1)
    (not (eq (point) old))))

(defun org-llm//find-header (f)
  "Find a header in my ancestry which satisfies this predicate.

Leave the point at the header and return its org-element-context
value."
  (let ((c (org-element-context)))
    (if (and (eq 'headline (org-element-type c))
             (funcall f c))
        c
      (when (org-llm//previous-heading)
        (org-llm//find-header f)))))

(defun org-llm//find-section-ancestor (section)
  (org-llm//find-header (lambda (h)
                          (equal section (org-element-property :LLM_SECTION h)))))

(defun org-llm//find-conversation-header ()
  (interactive)
  (org-llm//find-header (lambda (h) (eq 3 org-current-level))))


(defun org-llm/narrow-to-conversation ()
  (interactive)
  (when-let ((tree (org-llm//find-section-ancestor "conversation")))
    (org-narrow-to-subtree tree)
    tree))

(defun org-llm//summarize-buffer ()
  "Assume the buffer has been narrowed to a conversation

Returns a list representing the headers in the buffer and their role. E.g.:

((:interaction :content \"I am Jack, greet me\" :role user)
 (:context \"I am context\")
 (:interaction :content \"Hello Jack\" :role assistant))

The results are intended for make-llm-chat-prompt. Every element
is either an :interaction or a :context. The :interactions should
have their cdrs combined into a list, the :context should be
passed as-is.
"
  (let (top-h)
    (cl-remove
     #'null
     (org-element-map (org-element-parse-buffer) 'headline
       (lambda (h)
         (if top-h
             (when (eq top-h (org-element-property :parent h))
               (let ((content (string-trim (buffer-substring-no-properties
                                            (org-element-property :contents-begin h)
                                            (org-element-property :contents-end h)))))
                 (pcase (org-element-property :raw-value h)
                   ("Prompt"
                    `(:interaction :role user :content ,content))
                   ("Response"
                    `(:interaction :role assistant :content ,content))
                   ("Context"
                    `(:context ,content))
                   (`,x
                    (user-error "Unknown header: %s (accepted: Prompt, Response, Context)" x)))))
           (setf top-h h)
           nil))))))

;; Hacky--this only works with openai. https://github.com/ahyatt/llm/issues/43
(defun org-llm//summary->args (summ)
  "Prepare the result of ‘org-llm//summarize-buffer’ for ‘make-llm-chat-prompt’"
  (let (context interactions)
    (dolist (form summ)
      (pcase-exhaustive form
        (`(:interaction . ,rest)
         (setf interactions
               (append interactions
                       (list (apply #'make-llm-chat-prompt-interaction rest)))))
        (`(:context ,text)
         (setf context
               (make-llm-chat-prompt-interaction :role 'system :content text)))))
    (list :interactions (if context
                            (cons context interactions)
                          interactions))))

;;;###autoload
(defun org-llm/continue-conversation ()
  (interactive)
  (save-restriction
    (if-let (head (org-llm/narrow-to-conversation))
        (let ((prompt (->> (org-llm//summarize-buffer)
                           org-llm//summary->args
                           (apply #'make-llm-chat-prompt))))
          (org-insert-subheading '(4))
          (insert "Response\n\n")
          (save-excursion
            (insert "\n")
            (org-insert-heading '(4))
            (insert "Prompt\n"))
          (let ((start (point-marker))
                  (end (copy-marker (point-marker) t)))
              (llm-chat-streaming-to-point
               org-llm/provider
               prompt
               (current-buffer)
               end
               (lambda ())))
          (goto-char (point-max)))
      (user-error "Not currently in an conversation"))))



;;;;; CONVERT MARKDOWN <-> ORG-MODE

(defun org-llm//convert-subtree (from to)
  (save-excursion
    (org-back-to-heading)
    (let ((h (org-element-context)))
      (when (eq 'headline (org-element-type h))
        (save-restriction
          (narrow-to-region (org-element-property :contents-begin h)
                            (org-element-property :contents-end h))
          (call-process-region (point-min) (point-max) "pandoc" t t nil "-f" from "-t" to))))))

;;;###autoload
(defun org-llm/md->org ()
  "Convert a subtree from markdown to org-mode, in-place.

The region is from START to END. Requires pandoc.
"
  (interactive)
  (org-llm//convert-subtree "gfm" "org"))

;;;###autoload
(defun org-llm/org->md ()
  "Convert a subtree from org-mode to markdown, in-place.

The region is from START to END. Requires pandoc.
"
  (interactive)
  (org-llm//convert-subtree "org" "gfm"))


;;;;; CONVERSATION HISTORY

;;;###autoload
(defun org-llm/new-conversation (&optional msg)
  (interactive)
  (if-let (h (org-llm//find-section-ancestor "conversations"))
      (progn
        (org-insert-subheading '(4))
        (org-set-property "LLM_SECTION" "conversation")
        (insert (format-time-string "%Y-%m-%d"))
        (org-insert-subheading '(4))
        (insert "Prompt\n\n"))
    (user-error "Not in an LLM conversation history")))

;;;;; SUMMARIZE

(defcustom title-prompt "Generate a title for this conversation."
  "Prompt sent to the LLM to generate a title for this conversation")

;;;###autoload
(defun org-llm/summarize-conversation ()
  "Append a prompt to the conversation to generate a title. Send it to the llm. Grab the returned title and replace the conversation title with it"
  (interactive)
  (save-restriction
    (if-let (head (org-llm/narrow-to-conversation))
        (let ((prompt (->> `(:interaction :role user :content title-prompt)
			   list
			   (append (org-llm//summarize-buffer))
			   org-llm//summary->args
                           (apply #'make-llm-chat-prompt))))
	  (goto-char (+ 1 (org-element-property :begin head) (org-element-property :level head)))
	  (kill-line)
          (let ((start (point-marker))
                (end (copy-marker (point-marker) t)))
            (llm-chat-streaming-to-point
             org-llm/provider
             prompt
             (current-buffer)
             end
             (lambda ()
	       ;; Strip quotes which most of the responses seem to contain
	       (goto-char end)
	       (backward-char)
	       (when (looking-at "\"") (delete-char 1))
	       (goto-char start)
	       (when (looking-at "\"") (delete-char 1)))))
          (goto-char (point-max)))
      (user-error "Not currently in an conversation"))))


(define-prefix-command 'org-llm/map)
(keymap-set org-llm/map "b" 'org-llm/send-block)
(keymap-set org-llm/map "s" 'org-llm/continue-conversation)
(keymap-set org-llm/map "n" 'org-llm/new-conversation)
(keymap-set org-llm/map "m" 'org-llm/org->md)
(keymap-set org-llm/map "o" 'org-llm/md->org)
(keymap-set org-llm/map "t" 'org-llm/summarize-conversation)

(provide 'org-llm)
