;;; core-ui.el --- Initialize ui configurations.	-*- lexical-binding: t -*-

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
;; UI configurations.
;;

;;; Code:
(use-package maple-theme
  :ensure nil
  :commands (maple-theme/switch/body maple-theme/next maple-theme/previous)
  :hydra
  (maple-theme/switch
   ()
   ("n" maple-theme/next "next theme")
   ("p" maple-theme/previous "prev theme")))

(use-package monokai-theme)
(use-package solarized-theme)
(use-package spacemacs-theme)
(use-package doom-themes
  :custom-face (show-paren-match ((t (:background "#51afef")))))

;; this is ugly
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'relative))

;; 高亮括号
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :diminish rainbow-delimiters-mode)

;; 高亮括号配对
(use-package paren
  :ensure nil
  :hook (maple-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; 外置高亮括号
(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (setq hl-paren-colors '("Springgreen3"
                          "IndianRed1"
                          "#51afef"
                          "IndianRed3"
                          "#da8548"
                          "IndianRed4"))
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  :diminish highlight-parentheses-mode)

;; 颜色
(use-package rainbow-mode
  :hook ((prog-mode conf-unix-mode) . rainbow-mode)
  :diminish rainbow-mode)

;; 相同字符
(use-package highlight-symbol
  :hook
  ((prog-mode text-mode) . highlight-symbol-nav-mode)
  ((prog-mode text-mode) . highlight-symbol-mode)
  :diminish highlight-symbol-mode)

;;高亮当前行
(use-package hl-line
  :ensure nil
  :hook (maple-init . global-hl-line-mode))

;; 显示缩进
(use-package highlight-indent-guides
  :if (display-graphic-p)
  :hook ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|)
  :diminish highlight-indent-guides-mode)

(use-package all-the-icons
  :if (and (display-graphic-p) maple-icon)
  :config
  (setq all-the-icons-icon-alist
        (append
         (list '("\\.go$" all-the-icons-fileicon "golo" :height 1.0 :face all-the-icons-blue))
         (butlast all-the-icons-icon-alist)
         (list '("." all-the-icons-octicon "book" :height 1.0 :v-adjust 0.0 :face all-the-icons-lcyan)))))

(provide 'core-ui)
;;; core-ui.el ends here
