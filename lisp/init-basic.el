;;; init-basic.el --- Initialize basic configurations.	-*- lexical-binding: t -*-

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
;; Basic configurations.
;;

;;; Code:

(eval-when-compile (require 'cl))

(defvar maple-init-hook nil
  "Custom init hook.")

(defvar maple-theme-hook nil
  "Custom theme hook.")

(defconst maple-cache-directory
  (expand-file-name "cache/" user-emacs-directory)
  "Maple storage area for persistent files.")

(defconst maple-system-is-mac
  (eq system-type 'darwin))

(defconst maple-system-is-macport
  (and (eq system-type 'darwin)
       (boundp 'mac-carbon-version-string)))

(defconst maple-system-is-linux
  (eq system-type 'gnu/linux))

(defconst maple-system-is-windows
  (eq system-type 'windows-nt))

(defun maple/plist-get(args key &optional default)
  "Custom `plist-get` with ARGS and KEY DEFAULT."
  (or (plist-get args key)
      (plist-get (cdr args) key)
      default))

(defmacro maple/add-path (parent-dir)
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (declare (indent defun))
  `(let ((dirs (cl-remove-if-not
                (lambda (dir) (file-directory-p dir))
                (directory-files ,parent-dir t "^[^\\.]"))))
     (setq load-path (append (if dirs dirs (list ,parent-dir)) load-path))))

(defmacro maple/add-hook(hook &rest args)
  "Custom hook with HOOK and ARGS no need lambda."
  (declare (indent defun))
  (let ((-if (maple/plist-get args :if t))
        (-local (maple/plist-get args :local))
        (-append (maple/plist-get args :append t))
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

(defmacro maple/add-hook-once (hook f &optional append local)
  "Like `add-hook`, remove after call with HOOK F &OPTIONAL APPEND LOCAL."
  (let ((func (intern (format "maple/run-once-%s"
                              (symbol-name f)))))
    `(progn
       (defun ,func ()
         (remove-hook ',hook ',func ,local)
         (funcall ',f))
       (add-hook ',hook ',func ,append ,local))))

(defun maple/process-exit-sentinel(process _event)
  "Close current when PROCESS `exit'."
  (when (memq (process-status process) '(exit stop))
    (when (> (count-windows) 1) (delete-window))
    (kill-buffer (process-buffer process))))

(defun maple/process-exit()
  "Process auto exit."
  (let ((process (get-buffer-process (current-buffer))))
    (when process (set-process-sentinel process 'maple/process-exit-sentinel))))

(defun maple/toggle-indent-mode ()
  "Toggle indent tab mode."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode)))

(defun maple/company-or-indent ()
  "Company buffer or indent."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (company-indent-or-complete-common)))

(defun maple/reload-user-init-file()
  "Reload init file."
  (interactive)
  (load-file user-init-file))

(defun maple/define-key (keymap key def &rest bindings)
  "Define multi keybind with KEYMAP KEY DEF BINDINGS."
  (declare (indent defun))
  (while key
    (define-key keymap key def)
    (setq key (pop bindings)
          def (pop bindings))))

(defun maple/region-string()
  "Get region string."
  (if (not (use-region-p)) ""
    (let* ((beg (region-beginning))
           (end (region-end))
           (eol (save-excursion (goto-char beg) (line-end-position))))
      (deactivate-mark) (buffer-substring-no-properties beg (min end eol)))))

(defun maple/truncate-lines()
  "Turn on `truncate-lines`."
  (interactive)
  (visual-line-mode t)
  (toggle-truncate-lines t))

(defun maple/evil-map(map &optional state)
  "Make MAP evil with STATE."
  (with-eval-after-load 'evil
    (evil-make-overriding-map map (or state 'normal))))

(defun maple/kill-emacs ()
  "Like `kill-emacs', but ignore `kill-emacs-hook'."
  (interactive)
  (let (kill-emacs-hook) (kill-emacs)))

(defadvice load-theme (after run-maple-theme-hook activate)
  "Run `maple-theme-hook'."
  (run-hooks 'maple-theme-hook))

(maple/add-hook 'emacs-startup-hook
  (run-with-idle-timer 0.1 nil (lambda() (run-hooks 'maple-init-hook))))

(maple/add-path (expand-file-name "site-lisp" user-emacs-directory))

(provide 'init-basic)
;;; init-basic.el ends here
