;;; init-ui.el --- Initialize ui configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2025 lin.jiang

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
(use-package monokai-theme)
(use-package spacemacs-theme)

(use-package doom-themes
  :custom-face (show-paren-match ((t (:background "#51afef")))))

(use-package dracula-theme
  :config
  (custom-theme-set-faces
   'dracula
   '(corfu-default ((t (:inherit tooltip :background unspecified))))))

(use-package maple-theme
  :ensure nil
  :transient
  (maple-theme/switch
   ()
   [[("n" "next theme" maple-theme/next :transient t)]
    [("p" "prev theme" maple-theme/previous :transient t)]]))

;; this is ugly
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'relative))

;; 高亮括号
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :diminish rainbow-delimiters-mode)

;; 高亮括号配对
(use-package paren
  :ensure nil
  :hook (maple-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; 外置高亮括号
(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode)
  :custom
  (highlight-parentheses-colors '("Springgreen3"
                                  "IndianRed1"
                                  "#51afef"
                                  "IndianRed3"
                                  "#da8548"
                                  "IndianRed4"))
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
(use-package indent-bars
  :if (display-graphic-p)
  :hook ((python-mode dart-mode yaml-mode) . indent-bars-mode)
  :custom
  (indent-bars-width-frac 0.1)
  (indent-bars-display-on-blank-lines nil))

(use-package default-text-scale
  :commands (default-text-scale-increase default-text-scale-decrease))

(use-package nerd-icons
  :if maple-icon
  :commands (nerd-icons-insert nerd-icons-insert-faicon nerd-icons-insert-mdicon))

(provide 'init-ui)
;;; init-ui.el ends here
