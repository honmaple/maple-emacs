;;; init.el --- Initialize configurations.	-*- lexical-binding: t -*-

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
;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

;;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize); You may delete these explanatory comments.

;;; Code:
(eval-and-compile
  (defvar user-handler-alist file-name-handler-alist)
  (defvar user-default-theme nil)
  (defvar *lsp* t)
  (defvar *icon* t)
  (defvar *develop* t)
  (defvar *python3* t)

  (setq user-full-name "jianglin"
        user-default-theme 'monokai
        user-mail-address "mail@honmaple.com"
        gc-cons-threshold (* 256 1024 1024)
        gc-cons-percentage 0.6
        file-name-handler-alist nil)

  (defun maple/finish()
    "Restore defalut values after init"
    (setq file-name-handler-alist user-handler-alist
          gc-cons-threshold 800000
          gc-cons-percentage 0.1))

  (add-hook 'emacs-startup-hook 'maple/finish))

(defmacro maple/require (pkg)
  "Load PKG."
  `(require ,pkg (format "%s/%s.el" (expand-file-name "lisp" user-emacs-directory) ,pkg)))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(maple/require 'init-basic)
(maple/require 'init-elpa)        ;; Machinery for installing required packages
(maple/require 'init-maple)
(maple/require 'init-font)
(maple/require 'init-gui)         ;;ui设置 显示行号
(maple/require 'init-ui)          ;; modeline,which-key
(maple/require 'init-editor)      ;;自动补全括号等
(maple/require 'init-auto-insert) ;;自动插入文件头
(maple/require 'init-evil)
(maple/require 'init-ivy)
(maple/require 'init-dired)       ;;自带文件管理
(maple/require 'init-file)        ;;文件操作
(maple/require 'init-window)

(when *develop*
  (maple/require 'init-flycheck)
  (maple/require 'init-company)
  (maple/require 'init-git)
  (maple/require 'init-shell)     ;;shell
  (maple/require 'init-web)
  (maple/require 'init-python)
  (maple/require 'init-go)
  (maple/require 'init-lua)
  (maple/require 'init-c)
  (maple/require 'init-sql)
  (maple/require 'init-text)      ;; markdown rst
  (maple/require 'init-org)
  (maple/require 'init-tool))

(when *lsp*
  (maple/require 'init-lsp))

(maple/require 'init-keybind)

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (when (file-exists-p custom-file)
;;   (load custom-file))

(provide 'init)
;;; init.el ends here
