;;; org-llm.el --- Integrate LLMs with org-mode -*- lexical-binding: t; -*-

;; Copyright © 2024 Hraban Luyat

;; Author: Hraban Luyat <hraban@0brg.net>
;; Keywords: lisp org html export
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (llm "0.12.1") (s "20220902"))
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
(require 's)

(defcustom org-llm/provider nil
  "An LLM instance from the llm package")

(defun org-llm/send-block ()
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

(defun org-llm//conversation-header-p (h)
  (org-element-property :LLM_CONVERSATION h))

(defun org-llm/narrow-to-conversation ()
  (interactive)
  (when-let ((tree (org-llm//find-header #'org-llm//conversation-header-p)))
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
               (let ((content (s-trim (buffer-substring-no-properties
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

(defun org-llm//summary->args (summ)
  "Prepare the result of ‘org-llm//summarize-buffer’ for ‘make-llm-chat-prompt’"
  (cl-reduce
   (cl-function
    (lambda (agg form)
      (pcase-exhaustive form
        (`(:interaction . ,rest)
         (setf (plist-get agg :interactions)
               (append (plist-get agg :interactions)
                       (list (apply #'make-llm-chat-prompt-interaction rest)))))
        (`(:context ,context)
         (setf (plist-get agg :context)
               context)))
      agg))
   summ
   :initial-value '()))

(defun org-llm/continue-conversation ()
  (interactive)
  (save-restriction
    (when-let ((head (org-llm/narrow-to-conversation)))
      (let ((prompt (->> (org-llm//summarize-buffer)
                         org-llm//summary->args
                         (apply #'make-llm-chat-prompt)
                         ))
            (headstr (make-string (org-element-property :level head) ?*)))
        (goto-char (point-max))
        ;; An extra * because it’s a subheading
        (insert "\n" headstr "* Response\n\n\n\n" headstr "* Prompt\n\n")
        (save-excursion
          (forward-line -4)
          (llm-chat-streaming-to-point
           org-llm/provider
           prompt
           (current-buffer)
           (copy-marker (point-marker) t)
           (lambda (&rest _))))))))

(provide 'org-llm)
