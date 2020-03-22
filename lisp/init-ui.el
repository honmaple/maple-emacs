;;; init-ui.el --- Initialize ui configurations.	-*- lexical-binding: t -*-

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
;; UI configurations.
;;

;;; Code:

(eval-when-compile (require 'init-basic))

(use-package monokai-theme)
(use-package solarized-theme)
(use-package spacemacs-theme)
(use-package doom-themes
  :custom-face (show-paren-match ((t (:background "#51afef")))))

(maple/add-hook 'after-init-hook
  (load-theme user-default-theme t))

;; (use-package powerline
;;   :hook (maple-theme . powerline-center-evil-theme))

;; (use-package spaceline-config
;;   :ensure spaceline
;;   :hook (maple-theme . spaceline-spacemacs-theme)
;;   :config
;;   (setq powerline-default-separator (if (display-graphic-p) 'wave 'utf-8)
;;         spaceline-byte-compile nil
;;         spaceline-window-numbers-unicode t
;;         spaceline-highlight-face-func 'spaceline-highlight-face-evil-state))

(use-package hydra
  :custom-face
  (hydra-face-red ((t (:foreground "chocolate" :weight bold)))))

(use-package maple-theme
  :ensure nil
  :commands (maple-theme:switch/body maple-theme:next maple-theme:previous)
  :hydra
  (maple-theme:switch
   ()
   ("n" maple-theme:next "next theme")
   ("p" maple-theme:previous "prev theme")))

(use-package which-key
  :diminish which-key-mode
  :hook (maple-init . which-key-mode)
  :config
  (setq which-key-echo-keystrokes 0.02
        which-key-max-description-length 32
        which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay 0.2
        which-key-allow-evil-operators t)

  (maple/add-hook 'which-key-init-buffer-hook
    (setq window-size-fixed 'height))

  (which-key-add-key-based-replacements
    ",f" "file"
    ",b" "buffer"
    ",c" "comment"
    ",o" "orgmode"
    ",e" "flycheck error"
    ",j" "avy"
    ",g" "git"
    ",w" "window"
    ",p" "project"
    ",q" "emacs"
    ",S" "search"
    ",sq" "sql"
    ",t" "toggle mode"))

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


(use-package volatile-highlights
  :hook (maple-init . volatile-highlights-mode)
  :config
  ;; additional extensions
  ;; evil
  (with-eval-after-load 'evil
    (vhl/define-extension 'evil
                          'evil-move
                          'evil-paste-after
                          'evil-paste-before
                          'evil-paste-pop)
    (vhl/install-extension 'evil))
  ;; undo-tree
  (with-eval-after-load 'undo-tree
    (vhl/define-extension 'undo-tree
                          'undo-tree-move
                          'undo-tree-yank)
    (vhl/install-extension 'undo-tree))
  :custom-face (vhl/default-face ((t (:background "Springgreen3" :foreground "#272822"))))
  :diminish volatile-highlights-mode)

;; 显示缩进
(use-package highlight-indent-guides
  :if (display-graphic-p)
  :hook ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|)
  :diminish highlight-indent-guides-mode)

(use-package whitespace
  :ensure nil
  :hook ((prog-mode conf-mode yaml-mode) . whitespace-mode)
  :config
  (setq whitespace-action '(auto-cleanup)
        whitespace-style '(face
                           trailing space-before-tab
                           indentation empty space-after-tab))
  :diminish whitespace-mode "ⓦ")

(provide 'init-ui)

;;; init-ui.el ends here
