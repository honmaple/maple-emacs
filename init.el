;;; init.el --- Initialize configurations.	-*- lexical-binding: t -*-

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
;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

;;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize); You may delete these explanatory comments.

;;; Code:
(eval-and-compile
  (setq maple-user "lin.jiang"
        maple-mail "mail@honmaple.com"
        maple-theme 'monokai))

(autoload 'maple-initialize (expand-file-name "core/core" user-emacs-directory))
(maple-initialize)

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(defvar maple-packages
  '(init-ui
    init-maple
    init-editor      ;; 自动补全括号等
    init-evil
    init-ivy
    init-file        ;; 文件操作
    init-window
    init-keybind))

(defvar maple-develops
  '(init-company
    init-flycheck
    init-lsp
    init-git
    init-web
    init-backend
    init-org
    init-text      ;; markdown rst
    init-shell     ;; shell
    init-tool))

(apply 'maple-require "lisp" maple-packages)
(apply 'maple-require "lisp" maple-develops)

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (when (file-exists-p custom-file) (load custom-file))

(provide 'init)
;;; init.el ends here
