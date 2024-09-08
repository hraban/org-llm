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
(require 'rx)

(require 'dash)
(require 'llm)

(defcustom org-llm/provider nil
  "An LLM instance from the llm package")


;;;;; EXAMPLE BLOCKS

(defmacro org-llm//with-org-props (props el &rest body)
  (declare (indent defun))
  (cl-with-gensyms (elval)
    `(let* ((,elval ,el)
            ,@(--map `(,it (org-element-property ,(intern (format ":%s" it)) ,elval)) props))
       ,@body)))

;;;###autoload
(defun org-llm/send-block ()
  "Send a single example block to the LLM"
  (interactive)
  (let ((block (org-element-context)))
    (unless (eq 'example-block (org-element-type block))
      (user-error "Can only send example blocks to gpt4"))
    (org-llm//with-org-props (value) block
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

(defun org-llm//headerp (o)
  (eq 'headline (org-element-type o)))

(defun org-llm//ancestry ()
  "Find the list of headers from point, up to root"
  (--> (org-element-at-point)
       (org-element-lineage it nil 'with-self)
       (-filter #'org-llm//headerp it)))

(defun org-llm//find-header (f)
  "Find a header in my ancestry which satisfies this predicate."
  (cl-find-if f (org-llm//ancestry)))

(defun org-llm//find-section-ancestor (section)
  (org-llm//find-header (lambda (h)
                          (equal section (org-element-property :LLM_SECTION h)))))

(defun org-llm/narrow-to-conversation ()
  (interactive)
  (when-let ((tree (org-llm//find-section-ancestor "conversation")))
    (org-narrow-to-subtree tree)
    tree))

(defun org-llm//map-direct-subheadings (callback)
  "Map over every direct child of the top heading in this buffer.

Assumes the entire buffer is narrowed to a single org subtree.
"
  (let (top-h)
    (cl-remove
     #'null
     (org-element-map (org-element-parse-buffer) 'headline
       (lambda (h)
         (if top-h
             (when (eq top-h (org-element-property :parent h))
               (funcall callback h))
           (setf top-h h)
           nil))))))

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
  (cl-remove
   nil
   (org-llm//map-direct-subheadings
    (lambda (h)
      (org-llm//with-org-props (contents-begin contents-end tags) h
        (unless (or (member nil (list contents-begin contents-end))
                    (cl-intersection tags '("ignore" "archive") :test #'cl-equalp))
          (let ((content (string-trim (buffer-substring-no-properties contents-begin contents-end))))
            (pcase (org-element-property :raw-value h)
              ("Prompt"
               `(:interaction :role user :content ,content))
              ("Response"
               `(:interaction :role assistant :content ,content))
              ("Context"
               `(:context ,content))
              ((rx bos "Ignore ")
               ;; Ignore me
               )
              (`,x
               (user-error "Unknown header: %s (accepted: Prompt, Response, Context)" x))))))))))

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
          (goto-char (org-element-property :begin head))
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

(defun org-llm//conversation-heading-p (o)
  (and (eq 'headline (org-element-type o)) (equal "conversation" (org-entry-get o "LLM_SECTION"))))

(defun org-llm//find-conversation-subheading ()
  "Find the heading for this entry in the conversation..

Meaning the \"Repsonse\" or \"Prompt\" subheading for this
conversation."
  ;; Can’t search on those actual titles because they might be different and
  ;; because there might be a nested subheading with the same title in our
  ;; ancestry.
  (when-let* ((ancestry (org-llm//ancestry))
              (conv (cl-position-if #'org-llm//conversation-heading-p ancestry)))
    (elt ancestry (1- conv))))

(defun org-llm//rebalance-headings (top-level)
  "Set the highest heading to TOP-LEVEL and correct all below appropriately."
  (save-excursion
    (let (old-top)
      (->>
       (org-element-map (org-element-parse-buffer) 'headline
         (lambda (h)
           (org-llm//with-org-props (true-level begin) h
             (setf old-top (or old-top true-level))
             (list begin true-level))))
       reverse
       (mapc (cl-function
              (lambda ((begin old-level))
                (goto-char begin)
                (delete-char old-top)
                (insert (make-string top-level ?*)))))))))

(defun org-llm//convert-conversation (from to)
  (if-let ((head (org-llm//find-conversation-subheading)))
      (org-llm//with-org-props (contents-begin contents-end true-level) head
        (save-restriction
          (save-excursion
            (narrow-to-region contents-begin contents-end)
            (call-process-region contents-begin contents-end "pandoc" t t nil "-f" from "-t" to)
            (org-llm//rebalance-headings (1+ true-level)))))
    (user-error "Not currently in an conversation")))

;;;###autoload
(defun org-llm/md->org ()
  "Convert a subtree from markdown to org-mode, in-place.

The region is from START to END. Requires pandoc.
"
  (interactive)
  (org-llm//convert-conversation "gfm" "org"))

;;;###autoload
(defun org-llm/org->md ()
  "Convert a subtree from org-mode to markdown, in-place.

The region is from START to END. Requires pandoc.
"
  (interactive)
  (org-llm//convert-conversation "org" "gfm"))


;;;;; CONVERSATION HISTORY

;;;###autoload
(defun org-llm/new-conversation (&optional msg)
  (interactive)
  (if-let (h (org-llm//find-section-ancestor "conversations"))
      (progn
        (goto-char (org-element-property :begin h))
        (org-insert-subheading '(4))
        (org-set-property "LLM_SECTION" "conversation")
        (insert (format-time-string "%Y-%m-%d"))
        (org-insert-subheading '(4))
        (insert "Prompt\n\n"))
    (user-error "Not in an LLM conversation history")))

;;;;; SUMMARIZE

(defcustom title-prompt "A best-effort title for this conversation, without talkback or questions, making stuff up if you have to, no matter what, is:"
  "Prompt sent to the LLM to generate a title for this conversation")

(defun org-llm//insert-or-skip-date ()
  "If point is at a date, skip past it, otherwise insert one.

Ensures the point is at exactly one space past a date notation.
"
  (save-match-data
    (if (looking-at (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)))
        (goto-char (match-end 0))
      (insert (format-time-string "%Y-%m-%d ")))
    (insert " ")))

;;;###autoload
(defun org-llm/summarize-conversation ()
  "Append a prompt to the conversation to generate a title. Send it to the llm. Grab the returned title and replace the conversation title with it"
  (interactive)
  (save-restriction
    (if-let (head (org-llm/narrow-to-conversation))
        ;; This doesn’t handle contexts properly but whatever.
        (let* ((parts (append '((:interaction :role user :content "Given this conversation:"))
                              (org-llm//summarize-buffer)
                              `((:interaction :role user :content ,title-prompt))))
               (prompt (apply #'make-llm-chat-prompt (org-llm//summary->args parts))))
	  (goto-char (+ 1 (org-element-property :begin head) (org-element-property :level head)))
          (org-llm//insert-or-skip-date)
	  (delete-region (point) (line-end-position))
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
