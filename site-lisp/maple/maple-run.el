;;; maple-run.el ---  run script.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/dotfiles/tree/master/emacs.d

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
;; run script.
;;

;;; Code:
(require 'comint)

(defgroup maple-run nil
  "Execute buffer with comint"
  :group 'maple)

(defcustom maple-run:program "bash"
  "Default program."
  :group 'maple-run
  :type 'string)

(defcustom maple-run:arguments '()
  "Default arguments."
  :group 'maple-run
  :type 'list)

(defcustom maple-run:timeout 20
  "Timeout seconds for running too long process."
  :group 'maple-run
  :type 'integer)

(defcustom maple-run:prompt "^"
  "Prompt regexp within maple-run-mode."
  :group 'maple-run
  :type 'string)

(defcustom maple-run:focus t
  "Whether auto open process buffer."
  :group 'maple-run
  :type 'boolean)

(defcustom maple-run:auto-clear nil
  "Whether auto clear process buffer."
  :group 'maple-run
  :type 'boolean)

(defcustom maple-run:alist
  '((python-mode
     :command "python %F")
    (go-mode
     :command "go run %F")
    (lua-mode
     :command "lua %F")
    ((js-mode js2-mode)
     :command "node %F")
    ((html-mode web-mode)
     :command browse-url-of-file)
    (emacs-lisp-mode
     :command "emacs -Q --batch -l %F"))
  "Run commands alist."
  :group 'maple-run
  :type 'list)

(defvar maple-run:buffer-name "*maple-run-process*")
(defvar maple-run:process-name "maple-run-process")
(defvar maple-run:temp-files nil)
(defvar maple-run:last-command nil)

(defun maple-run:process-sentinel(process _msg)
  "Start process sentinel with PROCESS MSG."
  (when (memq (process-status process) '(exit signal))
    (maple-run:remove-temp-files)
    (let ((input (read-char "Press 'r' to run again, any other key to finish.")))
      (if (char-equal input ?r) (maple-run:retry) (maple-run:finish)))))

(defun maple-run:process-timeout(process)
  "Start process timeout with PROCESS MSG."
  (when (eq (process-status process) 'run)
    (kill-process process)
    (let ((buffer (get-buffer maple-run:buffer-name)))
      (with-current-buffer buffer
        (insert (format "\nTime out %s(running over %d second)"
                        (process-name process) maple-run:timeout)))
      (maple-run:remove-temp-files)
      (pop-to-buffer buffer))))

(defun maple-run:remove-temp-files()
  "Remove temp files."
  (dolist (file maple-run:temp-files)
    (cond
     ((file-directory-p file) (delete-directory file t))
     ((file-exists-p file) (delete-file file))))
  (setq maple-run:temp-files nil))

(defun maple-run:true-file (buffer)
  "Get BUFFER true file name."
  (with-current-buffer buffer
    (if (and (buffer-file-name)
             (not (use-region-p)))
        (buffer-file-name)
      (let* ((temporary-file-directory default-directory)
             (content (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end))
                        (buffer-substring-no-properties (point-min) (point-max))))
             (codec buffer-file-coding-system)
             (filename (make-temp-file "temprun_")))
        (with-temp-file filename
          (set-buffer-file-coding-system codec)
          (insert content))
        (push filename maple-run:temp-files) filename))))

(defun maple-run:command (command &optional buffer)
  "Get whole command with COMMAND BUFFER."
  (let* ((buffer (or buffer (current-buffer)))
         (filename (maple-run:true-file buffer))
         (places `(("%b" . ,(file-name-nondirectory (file-name-sans-extension filename)))
                   ("%f" . ,(file-name-nondirectory filename))
                   ("%d" . ,(file-name-directory filename))
                   ("%B" . ,(file-name-sans-extension filename))
                   ("%F" . ,filename)))
         (case-fold-search nil))
    (dolist (place places)
      (setq command (replace-regexp-in-string (car place) (cdr place) command t)))
    command))

(defun maple-run:script (&optional program args proc)
  "Run an inferior instance of &optional PROGRAM ARGS PROC."
  (interactive)
  (let* ((program (or program maple-run:program))
         (args    (or args maple-run:arguments))
         (proc    (or proc maple-run:process-name))
         (buffer  (apply 'make-comint-in-buffer proc nil program nil args))
         process)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (when maple-run:auto-clear (erase-buffer)))
      (maple-run-mode)
      (setq-local maple-run:last-command (cons program args)))
    (when (comint-check-proc buffer)
      (setq process (get-buffer-process buffer))
      (set-process-sentinel process 'maple-run:process-sentinel)
      (when (> maple-run:timeout 0) (run-with-timer maple-run:timeout nil 'maple-run:process-timeout process)))
    (when maple-run:focus (pop-to-buffer buffer))))

(defun maple-run ()
  "Run current buffer."
  (interactive)
  (let ((alist (cl-loop for args in maple-run:alist
                        when (memq major-mode (if (listp (car args)) (car args) (list (car args))))
                        return (cdr args))))
    (unless alist (error (format "no compile found for %s." major-mode)))
    (let* ((command (plist-get alist :command))
           cmdlist program args)
      (if (not (stringp command)) (call-interactively command)
        (setq cmdlist (split-string-and-unquote (maple-run:command command)))
        (setq program (or (plist-get alist :program) (car cmdlist)))
        (setq args    (or (plist-get alist :args) (cdr cmdlist)))
        (maple-run:script program args)))))

(defun maple-run:retry()
  "Run Retry."
  (interactive)
  (if maple-run:last-command
      (maple-run:script (car maple-run:last-command) (cdr maple-run:last-command))
    (message "no last command found")))

(defun maple-run:finish()
  "Run Finish."
  (interactive)
  (quit-window))

(define-derived-mode maple-run-mode comint-mode "maple-run"
  "Major mode for `maple-run'."
  (setq comint-use-prompt-regexp t)
  (setq comint-prompt-regexp maple-run:prompt)
  (setq comint-prompt-read-only t))

(provide 'maple-run)
;;; maple-run.el ends here
