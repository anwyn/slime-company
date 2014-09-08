;;; slime-company.el --- slime completion backend for company mode
;;
;; Copyright (C) 2009-2014  Ole Arndt
;;
;; Author: Ole Arndt <anwyn@sugarshark.com>
;; Keywords: convenience, lisp, abbrev
;; Version: 0.8
;; Package-Requires: ((slime "2.3.2") (company "0.7"))
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This is a backend implementation for the completion package
;; company-mode by Nikolaj Schumacher. More info about this package
;; is available at http://company-mode.github.io/
;;
;;; Installation:
;;
;;  Put this file somewhere into your load-path
;;  (or just into slime-path/contribs) and then call
;;
;;   (slime-setup '(slime-company))
;;
;; I also have the following, IMO more convenient key bindings for
;; company mode in my .emacs:
;;
;;   (define-key company-active-map (kbd "\C-n") 'company-select-next)
;;   (define-key company-active-map (kbd "\C-p") 'company-select-previous)
;;   (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
;;   (define-key company-active-map (kbd "M-.") 'company-show-location)
;;
;;; Code:

(require 'company)

(define-slime-contrib slime-company
  "Interaction between slime and the company completion mode."
  (:license "GPL")
  (:authors "Ole Arndt <anwyn@sugarshark.com>")
  (:slime-dependencies slime-autodoc)
  (:swank-dependencies swank-arglists)
  (:on-load
   (dolist (h '(slime-mode-hook slime-repl-mode-hook sldb-mode-hook))
     (add-hook h 'slime-company-maybe-enable)))
  (:on-unload
   (dolist (h '(slime-mode-hook slime-repl-mode-hook sldb-mode-hook))
     (remove-hook h 'slime-company-maybe-enable))
   (slime-company-disable)))

(defgroup slime-company nil
  "Interaction between slime and the company completion mode."
  :group 'company
  :group 'slime)

(defcustom slime-company-after-completion nil
  "What to do after a successful completion.
In addition to displaying the arglist slime-company will also do one of:

- `nil':  nothing,
- insert a space. Works best if you also call `delete-horizontal-space'
  before closing parentheses to remove excess whitespace.
- call an arbitrary function with the completion string as the first parameter.
"
  :group 'slime-company
  :type '(choice
          (const :tag "Do nothing" nil)
          (const :tag "Insert space" slime-company-just-one-space)
          (function :tag "Call custom function" nil)))

(defcustom slime-company-complete-in-comments-and-strings nil
  "Should slime-company also complete in comments and strings."
  :group 'slime-company
  :type 'boolean)

(defun slime-company-just-one-space (arg)
  (just-one-space))

(defsubst slime-company-active-p ()
  "Test if the slime-company backend should be active in the current buffer."
  (derived-mode-p 'lisp-mode 'clojure-mode 'slime-repl-mode))

(defun slime-company-maybe-enable ()
  (when (slime-company-active-p)
    (company-mode 1)
    (add-to-list 'company-backends 'company-slime)))

(defun slime-company-disable ()
  (setq company-backends (remove 'company-slime company-backends)))

(defun slime-company-fetch-candidates-async (prefix)
  (let ((slime-current-thread t))
    (lexical-let ((package (slime-current-package))
                  (prefix prefix))
      (cons :async (lambda (callback)
                     (lexical-let ((callback callback))
                       (slime-eval-async
                           `(swank:simple-completions ,prefix ',package)
                         (lambda (result)
                           (funcall callback (first result)))
                         package)))))))

(defun company-slime (command &optional arg &rest ignored)
  "Company mode backend for slime."
  (case command
    ('init
     (slime-company-active-p))
    ('prefix
     (when (and (slime-company-active-p)
                (slime-connected-p)
                (or slime-company-complete-in-comments-and-strings
                    (null (company-in-string-or-comment))))
       (company-grab-symbol)))
    ('candidates
     (when (slime-connected-p)
       (slime-company-fetch-candidates-async (substring-no-properties arg))))
    ('meta
     (let ((arglist (slime-eval `(swank:operator-arglist ,arg ,(slime-current-package)))))
       (if arglist
           (slime-fontify-string arglist)
         :not-available)))
    ('doc-buffer
     (let ((doc (slime-eval `(swank:describe-symbol ,arg))))
       (with-current-buffer (company-doc-buffer)
         (insert doc)
         (goto-char (point-min))
         (current-buffer))))
    ('location
     (let ((source-buffer (current-buffer)))
       (save-window-excursion
         (slime-edit-definition arg)
         (let ((buffer (if (eq source-buffer (current-buffer))
                           slime-xref-last-buffer
                         (current-buffer))))
           (when (buffer-live-p buffer)
             (cons buffer (with-current-buffer buffer
                            (point))))))))
    ('post-completion
     (slime-echo-arglist)
     (when slime-company-after-completion
       (funcall slime-company-after-completion arg)))
    ('sorted nil)))

(provide 'slime-company)

;;; slime-company.el ends here
