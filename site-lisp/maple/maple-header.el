;;; maple-header.el ---  file header configuration.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 lin.jiang

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
;; file header configuration.
;;

;;; Code:
(require 'autoinsert)

(defgroup maple-header nil
  "Auto update file header."
  :group 'maple-header)

(defcustom maple-header:limit 9
  "The number of search limit."
  :group 'maple-header
  :type 'integer)

(defcustom maple-header:auto-insert-alist
  '(((ruby-mode . "Ruby program") nil
     "#!/usr/bin/env ruby\n"
     "# -*- encoding: utf-8 -*-\n"
     (maple-header:template) "\n")
    ((python-mode . "Python program") nil
     "#!/usr/bin/env python\n"
     "# -*- coding: utf-8 -*-\n"
     (maple-header:template) "\n")
    ((c-mode . "C program") nil
     "/*"
     (string-trim-left (maple-header:template " ")) "*/\n"
     "#include<stdio.h>\n"
     "#include<string.h>\n")
    ((sh-mode . "Shell script") nil
     "#!/bin/bash\n"
     (maple-header:template) "\n")
    ((go-mode . "Go program") nil
     "/*"
     (string-trim-left (maple-header:template " ")) "*/\n"
     "package main\n"))
  "List of header."
  :group 'maple-header
  :type '(list))

(defvar maple-header:auto-update-alist nil)

(defun maple-header:template(&optional prefix)
  "Template with PREFIX."
  (replace-regexp-in-string
   "^" (or prefix comment-start)
   (concat
    (make-string 80 ?*) "\n"
    "Copyright © " (substring (current-time-string) -4) " " (user-full-name) "\n"
    "File Name: " (file-name-nondirectory buffer-file-name) "\n"
    "Author: " (user-full-name)"\n"
    "Email: " user-mail-address "\n"
    "Created: " (format-time-string "%Y-%m-%d %T (%Z)" (current-time)) "\n"
    "Last Update: \n"
    "         By: \n"
    "Description: \n"
    (make-string 80 ?*))))

(defun maple-header:action(find replace)
  "When looking FIND with REPLACE value."
  (when (looking-at find)
    (let ((beg (match-beginning 2))
          (end (match-end 2)))
      (when (not (string= replace (string-trim-left (match-string 2))))
        (goto-char beg)
        (delete-region beg end)
        (insert " " replace)))))

(defun maple-header:auto-update()
  "Header auto update."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (dotimes (_ maple-header:limit)
      (cl-loop for item in maple-header:auto-update-alist
               when (symbol-value (intern (format "maple-header:%s-update-p" item)))
               do (funcall (intern (format "maple-header:%s-update" item)) t))
      (forward-line 1))))

(defun maple-header:auto-insert()
  "Conf auto insert."
  (setq auto-insert-query nil
        auto-insert-alist maple-header:auto-insert-alist)
  (auto-insert-mode 1))

(defmacro maple-header-define (name &rest args)
  "Define header update with NAME, ARGS."
  (declare (indent 1) (doc-string 2))
  (let ((name (format "%s" name))
        (find (plist-get args :find))
        (replace (or (plist-get args :replace) ""))
        (limit (plist-get args :limit)))
    `(progn
       (add-to-list 'maple-header:auto-update-alist ,name)
       (defcustom ,(intern (format "maple-header:%s-update-p" name)) t
         ,(format "Whether update %s." name)
         :group 'maple-header
         :type 'boolean)
       (defun ,(intern (format "maple-header:%s-update" name)) (&optional current-line)
         ,(format "Update %s header with regex." name)
         (interactive)
         (if current-line (maple-header:action ,find ,replace)
           (save-excursion
             (goto-char (point-min))
             (dotimes (_ (or ,limit maple-header:limit))
               (maple-header:action ,find ,replace)
               (forward-line 1))))))))

(maple-header-define filename
  :find ".*\\(File Name:\\)\\(.*\\)"
  :replace (file-name-nondirectory (buffer-file-name)))

(maple-header-define email
  :find ".*\\(Email:\\)\\(.*\\)"
  :replace user-mail-address)

(maple-header-define modify
  :find
  (cond ((apply 'derived-mode-p '(org-mode markdown-mode))
         ".*\\(Modified:\\|MODIFIED[: ]\\)\\(.*\\)")
        (t ".*\\([lL]ast[ -][uU]pdate:\\)\\(.*\\)"))
  :replace
  (cond ((apply 'derived-mode-p '(org-mode markdown-mode))
         (format-time-string "%Y-%02m-%02d %02H:%02M:%02S"))
        (t (let ((system-time-locale "en_US.UTF-8"))
             (format-time-string "%A %Y-%02m-%02d %02H:%02M:%02S (%Z)")))))

;;;###autoload
(define-minor-mode maple-header-mode
  "maple header mode"
  :group      'maple-header
  :global     t
  (if maple-header-mode
      (progn (add-hook 'prog-mode-hook 'maple-header:auto-insert)
             (add-hook 'before-save-hook  'maple-header:auto-update))
    (remove-hook 'prog-mode-hook 'maple-header:auto-insert)
    (remove-hook 'before-save-hook 'maple-header:auto-update)))

(provide 'maple-header)
;;; maple-header.el ends here
