;;; core/autoload/help.el ---  custom function.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2022 lin.jiang

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
;; custom functions.
;;

;;; Code:
(declare-function evil-make-overriding-map 'evil)
(declare-function company-indent-or-complete-common 'company)

;;;###autoload
(defun maple-plist-get(args key &optional default)
  "Custom `plist-get` with ARGS and KEY DEFAULT."
  (or (plist-get args key)
      (plist-get (cdr args) key)
      default))

;;;###autoload
(defun maple-add-path (parent-dir)
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (let ((dirs (cl-remove-if-not
               (lambda (dir) (file-directory-p dir))
               (directory-files parent-dir t "^[^\\.]"))))
    (setq load-path (append (if dirs dirs (list parent-dir)) load-path))))

;;;###autoload
(defmacro maple-add-hook(hook &rest args)
  "Custom hook with HOOK and ARGS no need lambda."
  (declare (indent defun))
  (let ((-if (maple-plist-get args :if t))
        (-local (maple-plist-get args :local))
        (-append (maple-plist-get args :append t))
        (hooks (if (cdr-safe (cadr hook))
                   (cadr hook)
                 (list (cadr hook))))
        (funcs (let ((val (car args)))
                 (if (memq (car-safe val) '(quote function))
                     (if (cdr-safe (cadr val)) (cadr val)
                       (list (cadr val)))
                   (list `(lambda(&rest _) ,@args)))))
        forms)
    (dolist (fn funcs)
      (setq fn `(function ,fn))
      (dolist (i hooks)
        (push `(add-hook ',i ,fn ,-append ,-local) forms)))
    `(when ,-if ,@forms)))

;;;###autoload
(defun maple-process-exit-sentinel(process _event)
  "Close current when PROCESS `exit'."
  (when (memq (process-status process) '(exit stop))
    (when (> (count-windows) 1) (delete-window))
    (kill-buffer (process-buffer process))))

;;;###autoload
(defun maple-process-exit()
  "Process auto exit."
  (let ((process (get-buffer-process (current-buffer))))
    (when process (set-process-sentinel process 'maple-process-exit-sentinel))))

;;;###autoload
(defun maple-toggle-indent-mode ()
  "Toggle indent tab mode."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode)))

;;;###autoload
(defun maple-company-or-indent ()
  "Company buffer or indent."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (company-indent-or-complete-common)))

;;;###autoload
(defun maple-escape ()
  "Run maple-escape."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         (abort-recursive-edit))
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ((keyboard-quit))))

;;;###autoload
(defun maple-region-string()
  "Get region string."
  (if (not (use-region-p)) ""
    (let* ((beg (region-beginning))
           (end (region-end))
           (eol (save-excursion (goto-char beg) (line-end-position))))
      (deactivate-mark) (buffer-substring-no-properties beg (min end eol)))))

;;;###autoload
(defun maple/region-string()
  "Get region string."
  (if (not (use-region-p)) ""
    (let* ((beg (region-beginning))
           (end (region-end))
           (eol (save-excursion (goto-char beg) (line-end-position))))
      (deactivate-mark) (buffer-substring-no-properties beg (min end eol)))))

;;;###autoload
(defun maple-truncate-lines()
  "Turn on `truncate-lines`."
  (interactive)
  (visual-line-mode t)
  (toggle-truncate-lines t))

;;;###autoload
(defun maple-evil-map(map &optional state)
  "Make MAP evil with STATE."
  (with-eval-after-load 'evil
    (evil-make-overriding-map map (or state 'normal))))

;;;###autoload
(defun maple-kill-emacs ()
  "Like `kill-emacs', but ignore `kill-emacs-hook'."
  (interactive)
  (let (kill-emacs-hook) (kill-emacs)))

;;; help.el ends here
