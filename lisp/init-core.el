;;; init-core.el --- Initialize configurations.	-*- lexical-binding: t -*-

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
;;
;; package configurations.
;; enter表示安装,d表示删除,x表示执行删除
;;

;;; Code:
(setq package-archives '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/"))
      package-enable-at-startup nil)

;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                          ("org"   . "https://orgmode.org/elpa/")
;;                          ("gnu"   . "https://elpa.gnu.org/packages/")))

;; (setq package-archives '(("gnu"   . "https://elpa.emacs-china.org/gnu/")
;;                          ("melpa" . "https://elpa.emacs-china.org/melpa/")
;;                          ("org"   . "https://elpa.emacs-china.org/org/")
;;                          ("melpa-stable" . "https://elpa.emacs-china.org/melpa-stable/")))

(eval-when-compile
  ;; (require 'maple-package)
  ;; (maple-package-initialize 'no-activate)
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; Setup `use-package'
(eval-when-compile
  (require 'use-package)
  (setq use-package-verbose nil
        use-package-always-ensure t
        use-package-always-defer t
        use-package-expand-minimally t
        use-package-minimum-reported-time 0.01))

(use-package quelpa
  :commands (quelpa)
  :config
  (setq quelpa-verbose nil
        quelpa-checkout-melpa-p nil
        quelpa-update-melpa-p nil
        quelpa-melpa-recipe-stores nil
        quelpa-self-upgrade-p nil))

;;显示状态mode
(use-package diminish
  :diminish abbrev-mode)

(use-package async-bytecomp
  :ensure async
  :hook (maple-init . async-bytecomp-package-mode)
  :config
  (setq async-bytecomp-allowed-packages '(all)))

;; (use-package benchmark-init
;;   :demand
;;   :init (benchmark-init/activate)
;;   :hook (after-init . benchmark-init/deactivate))

(use-package restart-emacs
  :commands (maple/restart-emacs)
  :config
  (defun maple/restart-emacs()
    "Restart Emacs."
    (interactive)
    (setq restart-emacs-restore-frames t)
    (restart-emacs)))

(use-package server
  :ensure nil
  :commands (server-running-p)
  :hook (maple-init . (lambda() (unless (server-running-p) (server-start)))))


(use-package fontawesome
  :commands (counsel-fontawesome))

(use-package default-text-scale
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)))

(use-package simple
  :ensure nil
  :hook (text-mode . visual-line-mode)
  :diminish visual-line-mode)

(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode)
  :config
  (setq-default adaptive-wrap-extra-indent 1))

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

(use-package xclip
  :if maple-system-is-linux
  :hook (maple-init . xclip-mode))

(use-package hydra
  :custom-face
  (hydra-face-red ((t (:foreground "chocolate" :weight bold)))))

(use-package which-key
  :diminish which-key-mode
  :hook (maple-init . which-key-mode)
  :config
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay 0.2)

  (maple/add-hook 'which-key-init-buffer-hook
    (setq window-size-fixed 'height))

  (which-key-add-key-based-replacements
    ",f" "file"
    ",b" "buffer"
    ",c" "comment"
    ",o" "orgmode"
    ",e" "flycheck"
    ",j" "avy"
    ",g" "git"
    ",w" "window"
    ",p" "project"
    ",q" "emacs"
    ",S" "search"
    ",sq" "sql"
    ",t" "toggle mode"))

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
    (if (fboundp 'mac-select-input-source)
        (mac-select-input-source "com.apple.keylayout.ABC")
      (let ((inhibit-message t))
        (shell-command
         "osascript -e 'tell application \"System Events\" to tell process \"SystemUIServer\"
      set currentLayout to get the value of the first menu bar item of menu bar 1 whose description is \"text input\"
      if currentLayout is not \"ABC\" then
        tell (1st menu bar item of menu bar 1 whose description is \"text input\") to {click, click (menu 1'\"'\"'s menu item \"ABC\")}
      end if
    end tell' &>/dev/null"))))

  (add-function :after after-focus-change-function 'maple/mac-switch-input-source)
  (with-eval-after-load 'evil
    (add-hook 'evil-insert-state-exit-hook 'maple/mac-switch-input-source)))

;; (setq font-use-system-font t)
(prefer-coding-system 'utf-8)

(provide 'init-core)
;;; init-core.el ends here
