;;; maple-language.el --- Initialize language configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2025 lin.jiang

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
(require 'cl-lib)

(defvar xref-prompt-for-identifier)

(declare-function imenu--make-index-alist 'imenu)
(declare-function hs-already-hidden-p 'hideshow)
(declare-function cape-capf-super 'cape)
(declare-function cape-capf-buster 'cape)
(declare-function cape-capf-predicate 'cape)
(declare-function cape-company-to-capf 'cape)

(defgroup maple-language nil
  "Display minibuffer with another frame."
  :group 'maple)

(defcustom maple-language-alist nil
  "Language alist configuration."
  :type 'list
  :group 'maple-language)

(defcustom maple-language-fallback t
  "Fallback command when error happen."
  :type 'boolean
  :group 'maple-language)

(defvar maple-language--alist nil)

(make-variable-buffer-local 'maple-language--alist)

(defun maple-language--plist-get(mode key &optional default)
  "Custom `plist-get` with ARGS and MODE KEY DEFAULT."
  (let ((args (or maple-language--alist
                  (cdr (assoc mode maple-language-alist))
                  (cdr (assoc t maple-language-alist)))))
    (or (plist-get args key) default)))

(defun maple-language--plist-combine (&rest plists)
  "Combine multi PLISTS."
  (let ((rtn (copy-sequence (pop plists)))
        p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq p (pop ls) v (pop ls))

        (when-let ((prev (plist-get rtn p)))
          (setq v (append (list v) (if (listp prev) prev (list prev)))))
        (setq rtn (plist-put rtn p v))))
    rtn))

(defun maple-language--checker-backend(backend)
  "Return checker BACKEND."
  (let ((disable nil)
        checkers disabled-checkers)
    (dolist (checker backend)
      (if (eq checker :disable)
          (setq disable t)
        (push checker (if disable disabled-checkers checkers))))
    (cons checkers disabled-checkers)))

(defun maple-language--complete-backend(backend)
  "Return completion BACKEND."
  (unless (listp backend)
    (setq backend (list backend)))
  (if (not (car backend)) '()
    (let ((args (apply 'append (mapcar 'maple-language--complete-backend (cdr backend)))))
      (pcase (car backend)
        (:super
         (list (apply 'cape-capf-super args)))
        (:buster
         (list (apply 'cape-capf-buster args)))
        (:predicate
         (list (apply 'cape-capf-predicate args)))
        (:company
         (mapcar 'cape-company-to-capf args))
        (:default
         (append (default-value 'completion-at-point-functions) args))
        (:yasnippet
         (append
          (cond ((featurep 'yasnippet-capf) (list 'yasnippet-capf))
                ((featurep 'company-yasnippet (list (cape-company-to-capf 'company-yasnippet)))))
          args))
        (_
         (append (list (car backend)) args))))))

(defun maple-language--execute(fns)
  "Execute FNS, call next fn when error happen NO-FALLBACK."
  (unless (listp fns) (setq fns (list fns)))
  (if maple-language-fallback
      (cl-loop for fn in fns
               with result do
               (setq result (ignore-errors (or (call-interactively fn) t)))
               when result return result)
    (and (car fns) (call-interactively (car fns)))))

(defun maple-language--comment(&optional paste)
  "Call comment.Yank selected region if PASTE."
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
(defun maple-language-comment-and-copy()
  "Copy and comment."
  (interactive)
  (maple-language--comment t))

;;;###autoload
(defun maple-language-comment()
  "Call fold."
  (interactive)
  (let ((fn (maple-language--plist-get major-mode :comment)))
    (or (and fn (maple-language--execute fn))
        (maple-language--comment))))

;;;###autoload
(defun maple-language-run()
  "Call run."
  (interactive)
  (let ((fn (maple-language--plist-get major-mode :run)))
    (and fn (maple-language--execute fn))))

;;;###autoload
(defun maple-language-fold()
  "Call fold."
  (interactive)
  (let ((fn (maple-language--plist-get major-mode :fold)))
    (or (and fn (maple-language--execute fn))
        (cond ((bound-and-true-p evil-mode)
               'evil-toggle-fold)
              (t 'hs-toggle-hiding)))))

;;;###autoload
(defun maple-language-format()
  "Call indent format."
  (interactive)
  (let ((fn (maple-language--plist-get major-mode :format)))
    (or (and fn (maple-language--execute fn))
        (save-excursion
          (if (use-region-p)
              (indent-region (region-beginning) (region-end) nil)
            (indent-region (point-min) (point-max) nil))))))

;;;###autoload
(defun maple-language-rename()
  "Rename variable."
  (interactive)
  (let ((fn (maple-language--plist-get major-mode :rename)))
    (or (and fn (maple-language--execute fn))
        (error "No rename function"))))

;;;###autoload
(defun maple-language-find-definition()
  "Call definition."
  (interactive)
  (let ((fn (maple-language--plist-get major-mode :definition)))
    (or (and fn (maple-language--execute fn))
        (let (xref-prompt-for-identifier)
          (call-interactively #'xref-find-definitions)))))

;;;###autoload
(defun maple-language-find-references()
  "Call references."
  (interactive)
  (let ((fn (maple-language--plist-get major-mode :references)))
    (or (and fn (maple-language--execute fn))
        (call-interactively #'xref-find-references))))

;;;###autoload
(defun maple-language-find-documentation()
  "Call documentation."
  (interactive)
  (let ((fn (maple-language--plist-get major-mode :documentation)))
    (and fn (maple-language--execute fn))))

;;;###autoload
(defun maple-language-define (mode &rest args)
  "Language define with MODE ARGS."
  (let ((tab (plist-get args :tab))
        (checker (plist-get args :checker))
        (complete (plist-get args :complete))
        (forms `((setq maple-language--alist (maple-language--plist-combine maple-language--alist ',args)))))
    (when tab
      (push
       (if tab `(setq tab-width ,tab) `(setq indent-tabs-mode nil))
       forms))
    (when complete
      (push
       `(setq-local completion-at-point-functions (maple-language--complete-backend ',complete))
       forms))
    (when checker
      (push
       `(with-eval-after-load 'flycheck
          (let ((checkers (maple-language--checker-backend ',checker)))
            (when (car checkers) (setq-local flycheck-checkers (car checkers)))
            (when (cdr checkers) (setq-local flycheck-disabled-checkers (cdr checkers)))))
       forms))
    (when forms
      (let ((fn `(lambda() ,@forms))
            (ms mode))
        (unless (listp ms) (setq ms (list ms)))
        (dolist (m ms) (add-hook (intern (format "%s-hook" m)) fn))))))

;;; language.el ends here
