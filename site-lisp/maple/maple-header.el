;;; maple-header.el ---  file header configuration.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 lin.jiang

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
;; file header configuration.
;;

;;; Code:
(require 'subr-x)

(defgroup maple-header nil
  "Auto update file header."
  :group 'maple-header)

(defcustom maple-header-limit 9
  "The number of search limit."
  :group 'maple-header
  :type 'number)

(defcustom maple-header-alist
  '(("filename"
     :find ".*\\(File Name:\\)\\(.*\\)"
     :replace (file-name-nondirectory (buffer-file-name)))
    ("email"
     :find ".*\\(Email:\\)\\(.*\\)"
     :replace user-mail-address)
    ("modify"
     :find
     (cond ((apply 'derived-mode-p '(org-mode markdown-mode))
            ".*\\(Modified:\\|MODIFIED[: ]\\)\\(.*\\)")
           (t ".*\\([lL]ast[ -][uU]pdate:\\)\\(.*\\)"))
     :replace
     (cond ((apply 'derived-mode-p '(org-mode markdown-mode))
            (format-time-string "%Y-%02m-%02d %02H:%02M:%02S"))
           (t (let ((system-time-locale "en_US.UTF-8"))
                (format-time-string "%A %Y-%02m-%02d %02H:%02M:%02S (%Z)"))))))
  "List of header."
  :group 'maple-header
  :type '(list))

(defun maple-header-action(replace)
  "Action with REPLACE value."
  (let ((beg (match-beginning 2))
        (end (match-end 2)))
    (when (not (string= replace (string-trim-left (match-string 2))))
      (goto-char beg)
      (delete-region beg end)
      (insert " " replace))))

(defmacro maple-header-define (name &rest args)
  "Define header update with NAME, ARGS."
  (declare (indent 1)
           (doc-string 2))
  (let ((-find (plist-get args :find))
        (-replace (or (plist-get args :replace) ""))
        (-limit (or (plist-get args :limit) maple-header-limit)))
    `(progn
       (defvar ,(intern (format "maple-header-%s-p" name)) t)
       (defun ,(intern (format "maple-header-update-%s" name)) (&optional only)
         ,(format "Update %s header with regex." name)
         (interactive)
         (if only (when (looking-at ,-find) (maple-header-action ,-replace))
           (save-excursion
             (goto-char (point-min))
             (dotimes (_ ,-limit)
               (when (looking-at ,-find) (maple-header-action ,-replace))
               (forward-line 1))))))))

(defun maple-header-update()
  "Header auto update."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (dotimes (_ maple-header-limit)
      (cl-mapcan
       (lambda(item)
         (when (symbol-value (intern (format "maple-header-%s-p" (car item))))
           (funcall (intern (format "maple-header-update-%s" (car item))) t)))
       maple-header-alist)
      (forward-line 1))))

(defun maple-header-init()
  "Init maple header."
  (dolist (item maple-header-alist)
    (eval `(maple-header-define ,@item)))
  (add-hook 'before-save-hook  'maple-header-update))

;;;###autoload
(define-minor-mode maple-header-mode
  "maple header mode"
  :group      'maple-header
  :init-value nil
  :global     t
  (if maple-header-mode (maple-header-init)
    (remove-hook 'before-save-hook 'maple-header-update)))

(provide 'maple-header)
;;; maple-header.el ends here
