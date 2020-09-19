;;; maple-language.el --- Initialize language configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/maple-emacs

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; language configurations.
;;

;;; Code:

(defgroup maple-language nil
  "Display minibuffer with another frame."
  :group 'maple)

(defcustom maple-language:run nil
  "Language run script."
  :type 'function
  :group 'maple-language)

(make-variable-buffer-local 'maple-language:run)

(defcustom maple-language:fold 'maple-language:default-fold
  "Language toggle fold."
  :type 'function
  :group 'maple-language)

(make-variable-buffer-local 'maple-language:fold)

(defcustom maple-language:format 'maple-language:default-format
  "Language call indent format."
  :type 'function
  :group 'maple-language)

(make-variable-buffer-local 'maple-language:format)

(defcustom maple-language:definition 'maple-language:default-definition
  "Language find definition."
  :type 'function
  :group 'maple-language)

(make-variable-buffer-local 'maple-language:definition)

(defcustom maple-language:references 'xref-find-references
  "Language find references."
  :type 'function
  :group 'maple-language)

(make-variable-buffer-local 'maple-language:references)

(defcustom maple-language:documentation nil
  "Language find documentation."
  :type 'function
  :group 'maple-language)

(make-variable-buffer-local 'maple-language:documentation)

(defcustom maple-language:complete-enable-snippet t
  "Language auto completion enable company-yasnippet."
  :type 'boolean
  :group 'maple-language)

(defcustom maple-language:complete-backends
  '((company-dabbrev-code
     company-capf
     company-keywords
     company-files
     :with company-yasnippet)
    (company-dabbrev
     company-gtags
     company-etags
     :with company-yasnippet))
  "Language auto completion default backends."
  :type '(list)
  :group 'maple-language)

(defun maple-language:complete-backend(backend &optional with-snippet)
  "Return BACKENDs WITH-SNIPPET or not."
  (let ((backend (if (listp backend) backend (list backend))))
    (append (list (if with-snippet (maple-language:complete-backend-with-snippet backend) backend))
            maple-language:complete-backends)))

(defun maple-language:complete-backend-with-snippet(backend)
  "Return BACKEND with company-yasnippet."
  (if (or (not maple-language:complete-enable-snippet)
          (and (listp backend)
               (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defun maple-language:checker-backend(backend)
  "Return BACKEND."
  (cl-loop for checker in backend
           if (eq checker :disable)
           collect t into tmp
           else if tmp
           collect checker into b
           else collect checker into a
           finally (return (cons a b))))

(defun maple-language:imenu-items()
  "Get all definition with imenu."
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (let* ((imenu-max-item-length "Unlimited")
         (imenu-auto-rescan t)
         (imenu-auto-rescan-maxout (buffer-size))
         (items (imenu--make-index-alist t))
         (items (delete (assoc "*Rescan*" items) items)))
    (ignore imenu-max-item-length)
    (ignore imenu-auto-rescan)
    (ignore imenu-auto-rescan-maxout)
    items))

(defun maple-language:comment(&optional paste)
  "Call comment.Yank selected region if PASTE."
  (interactive)
  (save-excursion
    (when (and (bound-and-true-p hs-minor-mode) (hs-already-hidden-p))
      (set-mark (line-beginning-position))
      (end-of-visual-line)
      (activate-mark))
    (let (beg end)
      (if (region-active-p)
          (setq beg (region-beginning) end (region-end))
        (setq beg (line-beginning-position) end (line-end-position)))
      (when paste (copy-region-as-kill beg end) (goto-char end) (yank))
      (comment-or-uncomment-region beg end))))

(defun maple-language:copy-and-comment()
  "Copy and comment."
  (interactive)
  (maple-language:comment t))

(defmacro maple-language:define (mode &rest args)
  "Language define with MODE ARGS."
  (declare (indent defun))
  (cl-destructuring-bind (&key run fold format checker complete definition references documentation) args
    (let ((forms (list (when run `(setq maple-language:run ,run))
                       (when fold  `(setq maple-language:fold ,fold))
                       (when format `(setq maple-language:format ,format))
                       (when definition `(setq maple-language:definition ,definition))
                       (when references `(setq maple-language:references ,references))
                       (when documentation `(setq maple-language:documentation ,documentation))
                       (when complete
                         `(with-eval-after-load 'company
                            (setq-local company-backends (maple-language:complete-backend ,complete))))
                       (when checker
                         `(with-eval-after-load 'flycheck
                            (let ((checkers (maple-language:checker-backend ,checker)))
                              (setq-local flycheck-disabled-checkers (cdr checkers))))))))
      (when forms
        (let* ((fn `(lambda() ,@(cl-loop for form in forms when form collect form)))
               (hooks (if (listp mode)
                          (cl-loop for m in mode collect (intern (format "%s-hook" m)))
                        (list (intern (format "%s-hook" mode))))))
          `(progn ,@(cl-loop for hook in hooks collect `(add-hook ',hook ,fn))))))))

(defun maple-language:default-format()
  "Call default indent format."
  (interactive)
  (save-excursion
    (if (use-region-p)
        (indent-region (region-beginning) (region-end) nil)
      (indent-region (point-min) (point-max) nil))))

(defun maple-language:default-fold()
  "Call default fold."
  (interactive)
  (call-interactively (cond ((bound-and-true-p evil-mode)
                             'evil-toggle-fold)
                            (t 'hs-toggle-hiding))))

(defun maple-language:default-definition()
  "Call default definition."
  (interactive)
  (let (xref-prompt-for-identifier)
    (ignore xref-prompt-for-identifier)
    (call-interactively
     (if (bound-and-true-p lsp-mode) 'lsp-find-definition #'xref-find-definitions))))

(defun maple-language:call-run()
  "Call run."
  (interactive)
  (call-interactively maple-language:run))

(defun maple-language:call-fold()
  "Call fold."
  (interactive)
  (call-interactively maple-language:fold))

(defun maple-language:call-format()
  "Call indent format."
  (interactive)
  (call-interactively maple-language:format))

(defun maple-language:call-definition()
  "Call definition."
  (interactive)
  (call-interactively maple-language:definition))

(defun maple-language:call-references()
  "Call references."
  (interactive)
  (call-interactively maple-language:references))

(defun maple-language:call-documentation()
  "Call documentation."
  (interactive)
  (call-interactively maple-language:documentation))

;;;###autoload
(define-minor-mode maple-language-mode
  "maple header mode"
  :group      'maple-language
  :global     t
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global
      "gd" 'maple-language:call-definition))
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "=" 'maple-language:call-format))
  (global-set-key [f5] 'maple-language:call-run)
  (global-set-key [f6] 'maple-language:call-format))

(provide 'maple-language)
;;; maple-language.el ends here
