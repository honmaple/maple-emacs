;;; core-general.el --- Initialize basic configurations.	-*- lexical-binding: t -*-

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
;; general configurations.
;;

;;; Code:
;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
;; (add-to-list 'default-frame-alist '(tool-bar-lines 0))
;; (add-to-list 'default-frame-alist '(menu-bar-lines 0))
;; (add-to-list 'default-frame-alist '(vertical-scroll-bars))
(maple-add-hook 'after-init-hook
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
      indicate-empty-lines t
      transient-mark-mode nil
      create-lockfiles nil
      backup-directory-alist `(("." . ,(expand-file-name "auto-save" maple-cache-directory)))
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
    (if (and (display-graphic-p) (fboundp 'mac-select-input-source))
        (mac-select-input-source "com.apple.keylayout.ABC")
      (let ((inhibit-message t))
        (shell-command
         "osascript -e 'tell application \"System Events\"
                             tell process \"TextInputMenuAgent\"
                                set currentLayout to menu bar item 1 of menu bar 2
                             end tell
                             if description of currentLayout is not \"ABC\" then
                                key code 49 using control down
                             end if
                        end tell' &>/dev/null"))))
  (add-function :after after-focus-change-function 'maple/mac-switch-input-source)
  (with-eval-after-load 'evil
    (add-hook 'evil-insert-state-exit-hook 'maple/mac-switch-input-source)))

;; (setq font-use-system-font t)
(prefer-coding-system 'utf-8)

(provide 'core-general)
;;; core-general.el ends here
