;;; maple-language.el --- Initialize language configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2023 lin.jiang

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
(defvar xref-prompt-for-identifier)

(declare-function imenu--make-index-alist 'imenu)
(declare-function hs-already-hidden-p 'hideshow)

(defgroup maple-language nil
  "Display minibuffer with another frame."
  :group 'maple)

(defcustom maple-language/run nil
  "Language run script."
  :type 'function
  :group 'maple-language)

(make-variable-buffer-local 'maple-language/run)

(defcustom maple-language/fold 'maple-language/default-fold
  "Language toggle fold."
  :type 'function
  :group 'maple-language)

(make-variable-buffer-local 'maple-language/fold)

(defcustom maple-language/format 'maple-language/default-format
  "Language call indent format."
  :type 'function
  :group 'maple-language)

(make-variable-buffer-local 'maple-language/format)

(defcustom maple-language/definition 'maple-language/default-definition
  "Language find definition."
  :type 'function
  :group 'maple-language)

(make-variable-buffer-local 'maple-language/definition)

(defcustom maple-language/references 'xref-find-references
  "Language find references."
  :type 'function
  :group 'maple-language)

(make-variable-buffer-local 'maple-language/references)

(defcustom maple-language/documentation nil
  "Language find documentation."
  :type 'function
  :group 'maple-language)

(make-variable-buffer-local 'maple-language/documentation)

(defcustom maple-language/complete-enable-snippet t
  "Language auto completion enable company-yasnippet."
  :type 'boolean
  :group 'maple-language)

(defcustom maple-language/complete-backends
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

;;;###autoload
(defun maple-language/complete-backend(&optional backend with-snippet)
  "Return BACKENDs WITH-SNIPPET or not."
  (if (not backend) maple-language/complete-backends
    (let ((backend (if (listp backend) backend (list backend))))
      (append (list (if with-snippet (maple-language/complete-backend-with-snippet backend) backend))
              maple-language/complete-backends))))

(defun maple-language/complete-backend-with-snippet(backend)
  "Return BACKEND with company-yasnippet."
  (if (or (not maple-language/complete-enable-snippet)
          (and (listp backend)
               (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defun maple-language/checker-backend(backend)
  "Return BACKEND."
  (cl-loop for checker in backend
           if (eq checker :disable)
           collect t into tmp
           else if tmp
           collect checker into b
           else collect checker into a
           finally (return (cons a b))))

(defun maple-language/imenu-items()
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

;;;###autoload
(defun maple-language/comment(&optional paste)
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

;;;###autoload
(defun maple-language/copy-and-comment()
  "Copy and comment."
  (interactive)
  (maple-language/comment t))

;;;###autoload
(defun maple-language/default-format()
  "Call default indent format."
  (interactive)
  (save-excursion
    (if (use-region-p)
        (indent-region (region-beginning) (region-end) nil)
      (indent-region (point-min) (point-max) nil))))

;;;###autoload
(defun maple-language/default-fold()
  "Call default fold."
  (interactive)
  (call-interactively (cond ((bound-and-true-p evil-mode)
                             'evil-toggle-fold)
                            (t 'hs-toggle-hiding))))

;;;###autoload
(defun maple-language/default-definition()
  "Call default definition."
  (interactive)
  (let (xref-prompt-for-identifier)
    (ignore xref-prompt-for-identifier)
    (call-interactively
     (if (bound-and-true-p lsp-mode) 'lsp-find-definition #'xref-find-definitions))))

;;;###autoload
(defun maple-language/call-run()
  "Call run."
  (interactive)
  (call-interactively maple-language/run))

;;;###autoload
(defun maple-language/call-fold()
  "Call fold."
  (interactive)
  (call-interactively maple-language/fold))

;;;###autoload
(defun maple-language/call-format()
  "Call indent format."
  (interactive)
  (call-interactively maple-language/format))

;;;###autoload
(defun maple-language/call-definition()
  "Call definition."
  (interactive)
  (call-interactively maple-language/definition))

;;;###autoload
(defun maple-language/call-references()
  "Call references."
  (interactive)
  (call-interactively maple-language/references))

;;;###autoload
(defun maple-language/call-documentation()
  "Call documentation."
  (interactive)
  (call-interactively maple-language/documentation))

;;;###autoload
(defun maple-language (mode &rest args)
  "Language define with MODE ARGS."
  (cl-destructuring-bind (&key tab run fold format checker complete definition references documentation) args
    (let (forms)
      (when run (push `(setq maple-language/run ',run) forms))
      (when fold (push `(setq maple-language/fold ',fold) forms))
      (when format (push `(setq maple-language/format ',format) forms))
      (when definition (push `(setq maple-language/definition ',definition) forms))
      (when references (push `(setq maple-language/references ',references) forms))
      (when documentation (push `(setq maple-language/documentation ',documentation) forms))
      (when tab (push (if tab `(setq tab-width ,tab) `(setq indent-tabs-mode nil)) forms))
      (when complete
        (push
         `(with-eval-after-load 'company
            (setq-local company-backends (maple-language/complete-backend ',complete)))
         forms))
      (when checker
        (push
         `(with-eval-after-load 'flycheck
            (let ((checkers (maple-language/checker-backend ',checker)))
              (when (car checkers) (setq-local flycheck-checkers (car checkers)))
              (when (cdr checkers) (setq-local flycheck-disabled-checkers (cdr checkers)))))
         forms))
      (when forms
        (let ((fn `(lambda() ,@forms))
              (ms mode))
          (unless (listp ms) (setq ms (list ms)))
          (dolist (m ms) (add-hook (intern (format "%s-hook" m)) fn)))))))

;;; language.el ends here
