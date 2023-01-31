;;; core.el --- Initialize basic configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2023 lin.jiang

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
(defvar maple-package-archive 'tuna)

(setq package-archives
      (pcase maple-package-archive
        ('tuna '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                 ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                 ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                 ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")))
        ('emacs-china '(("gnu"    . "https://elpa.emacs-china.org/gnu/")
                        ("nongnu" . "https://elpa.emacs-china.org/nongnu/")
                        ("melpa"  . "https://elpa.emacs-china.org/melpa/")
                        ("melpa-stable" . "https://elpa.emacs-china.org/stable-melpa/")))
        (_ '(("gnu"    . "https://elpa.gnu.org/packages/")
             ("nongnu" . "https://elpa.nongnu.org/nongnu/")
             ("melpa"  . "https://melpa.org/packages/")
             ("melpa-stable" . "https://stable.melpa.org/packages/"))))
      package-enable-at-startup nil)

(eval-when-compile
  ;; (require 'maple-package)
  ;; (maple-package-initialize 'no-activate)
  ;; (fset 'package-read-all-archive-contents 'ignore)
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
  :custom
  (quelpa-verbose nil)
  (quelpa-checkout-melpa-p nil)
  (quelpa-update-melpa-p nil)
  (quelpa-melpa-recipe-stores nil)
  (quelpa-self-upgrade-p nil))

;;显示状态mode
(use-package diminish
  :diminish abbrev-mode)

;; (use-package benchmark-init
;;   :demand
;;   :init (benchmark-init/activate)
;;   :hook (after-init . (lambda() (run-with-idle-timer 0.2 nil (lambda() (benchmark-init/deactivate))))))

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

(use-package startup
  :ensure nil
  :custom
  (auto-save-list-file-prefix (maple-cache-file "autosave")))

(use-package simple
  :ensure nil
  :hook (text-mode . visual-line-mode)
  :diminish visual-line-mode)

(use-package frame
  :ensure nil
  ;; blink-cursor-interval 0.4
  :init (blink-cursor-mode -1))

;; 设置默认浏览器
(use-package browse-url
  :ensure nil
  :custom
  (browse-url-generic-program (if maple-system-is-mac "open" "google-chrome-stable"))
  (browse-url-browser-function (cond (maple-system-is-mac
                                      'browse-url-default-macosx-browser)
                                     (maple-system-is-windows
                                      'browse-url-default-windows-browser)
                                     (t 'browse-url-generic))))

(use-package ediff
  :ensure nil
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-default-file (maple-cache-file "bookmarks"))
  (bookmark-save-flag 1))

(use-package async-bytecomp
  :ensure async
  :hook (maple-init . async-bytecomp-package-mode)
  :custom
  (async-bytecomp-allowed-packages '(all)))

(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode)
  :custom
  (adaptive-wrap-extra-indent 1))

(use-package xclip
  :if maple-system-is-linux
  :hook (maple-init . xclip-mode))

(use-package hydra
  :custom-face
  (hydra-face-red ((t (:foreground "chocolate" :weight bold)))))

(use-package transient
  :config
  ;; https://github.com/magit/transient/issues/18
  (with-no-warnings (transient-bind-q-to-quit))
  :custom
  (transient-mode-line-format nil)
  (transient-enable-popup-navigation nil)
  :custom-face
  (transient-argument
   ((t :inherit font-lock-warning-face :slant normal :weight bold :underline nil))))

(use-package which-key
  :diminish which-key-mode
  :hook
  (maple-init . which-key-mode)
  (which-key-init-buffer . (lambda() (setq window-size-fixed 'height)))
  :custom
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-idle-delay 0.2))

(provide 'core-package)
;;; core-package.el ends here
