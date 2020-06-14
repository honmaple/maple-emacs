;;; init-elpa.el --- Initialize elpa configurations.	-*- lexical-binding: t -*-

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
;; package configurations.
;; enter表示安装,d表示删除,x表示执行删除
;;

;;; Code:
(setq package-archives '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/"))
      package-enable-at-startup nil
      package--init-file-ensured t)

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

(use-package xclip
  :if maple-system-is-linux
  :hook (maple-init . xclip-mode))

(use-package server
  :ensure nil
  :commands (server-running-p)
  :hook (maple-init . (lambda() (unless (server-running-p) (server-start)))))

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



(provide 'init-elpa)
;;; init-elpa.el ends here
