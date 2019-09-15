;;; init-gui.el --- Initialize global configurations.	-*- lexical-binding: t -*-

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
;; Gui configurations.
;;

;;; Code:

(eval-when-compile (require 'init-basic))

;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
;; (add-to-list 'default-frame-alist '(tool-bar-lines 0))
;; (add-to-list 'default-frame-alist '(menu-bar-lines 0))
;; (add-to-list 'default-frame-alist '(vertical-scroll-bars))
(maple/add-hook 'after-init-hook
  ;; 关闭文件滑动控件
  (when (featurep 'scroll-bar) (scroll-bar-mode -1))
  ;; 关闭工具栏
  (when (featurep 'tool-bar) (tool-bar-mode -1))
  ;; 关闭菜单栏
  (when (featurep 'menu-bar) (menu-bar-mode -1)))

(fset 'yes-or-no-p 'y-or-n-p)
(fset 'display-startup-echo-area-message 'ignore)
(setq inhibit-startup-screen t
      inhibit-compacting-font-caches t
      inhibit-startup-echo-area-message user-full-name
      initial-major-mode 'fundamental-mode
      initial-scratch-message "")

(setq use-file-dialog nil
      use-dialog-box nil
      window-combination-resize t
      indicate-empty-lines t
      transient-mark-mode nil
      create-lockfiles nil
      backup-directory-alist `(("." . ,(concat maple-cache-directory "auto-save")))
      select-enable-clipboard t ;;激活粘贴板
      frame-title-format
      '("Happy Hacking - Emacs ♥ You" " "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))
;; (when (not (display-graphic-p))
;;   (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

(setq-default tab-width 4
              major-mode 'conf-mode
              fill-column 80
              case-fold-search t
              column-number-mode t
              ;; delete-selection-mode t ;;粘贴删除选中区域
              cursor-in-non-selected-windows nil
              indent-tabs-mode nil
              make-backup-files nil ;;禁止生成类似init.el~文件
              save-interprogram-paste-before-kill t
              set-mark-command-repeat-pop t
              x-gtk-use-system-tooltips nil
              x-wait-for-event-timeout nil
              truncate-lines t
              truncate-partial-width-windows nil
              ad-redefinition-action 'accept)

(setq mouse-yank-at-point t
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      xterm-mouse-mode 1)

;; 光标位于中间
(setq scroll-preserve-screen-position t
      scroll-margin 15
      scroll-conservatively 101)

(when maple-system-is-mac
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta
        mac-function-modifier 'hyper)

  (defun maple/mac-switch-input-source ()
    (shell-command
     "osascript -e 'tell application \"System Events\" to tell process \"SystemUIServer\"
      set currentLayout to get the value of the first menu bar item of menu bar 1 whose description is \"text input\"
      if currentLayout is not \"ABC\" then
        tell (1st menu bar item of menu bar 1 whose description is \"text input\") to {click, click (menu 1'\"'\"'s menu item \"ABC\")}
      end if
    end tell' &>/dev/null"))

  (add-hook 'focus-in-hook 'maple/mac-switch-input-source))

(use-package simple
  :ensure nil
  :hook (text-mode . visual-line-mode)
  :diminish visual-line-mode)

(use-package frame
  :ensure nil
  ;; blink-cursor-interval 0.4
  :init (blink-cursor-mode -1))

(use-package tooltip
  :ensure nil
  :config
  (setq tooltip-delay 1))

;; 设置默认浏览器
(use-package browse-url
  :ensure nil
  :config
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program
        (if maple-system-is-mac "open" "google-chrome-stable")))

;;高亮当前行
(use-package hl-line
  :ensure nil
  :hook (maple-init . global-hl-line-mode))

(use-package ediff
  :ensure nil
  :config
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" maple-cache-directory)
        bookmark-save-flag 1))

(provide 'init-gui)

;;; init-gui.el ends here
