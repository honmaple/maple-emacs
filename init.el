;;; init.el --- Initialize configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2020 lin.jiang

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
  (defvar *icon* (display-graphic-p))
  (defvar *python3* t)

  (setq user-full-name "lin.jiang"
        user-mail-address "mail@honmaple.com"
        user-default-theme 'monokai
        gc-cons-threshold (* 256 1024 1024)
        gc-cons-percentage 0.6
        file-name-handler-alist nil)

  (defun maple/finish()
    "Restore defalut values after init"
    (setq file-name-handler-alist user-handler-alist
          gc-cons-threshold 800000
          gc-cons-percentage 0.1))

  (add-hook 'emacs-startup-hook 'maple/finish))

(defun maple/require (&rest pkgs)
  "Load PKGS."
  (mapc (lambda(pkg) (require `,pkg (format "%s/%s.el" (expand-file-name "lisp" user-emacs-directory) pkg))) pkgs))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(defvar maple-packages
  '(init-basic
    init-core        ;; Machinery for installing required packages
    init-maple
    init-ui          ;; modeline,which-key
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

(apply 'maple/require maple-packages)
(apply 'maple/require maple-develops)

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (when (file-exists-p custom-file) (load custom-file))

(provide 'init)
;;; init.el ends here
