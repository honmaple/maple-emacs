;;; init-evil.el --- Initialize evil configurations.	-*- lexical-binding: t -*-

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
;; Evil configurations.
;;

;;; Code:

(use-package evil
  :hook (maple-init . evil-mode)
  :config
  (fset 'evil-visual-update-x-selection 'ignore) ;;粘贴板
  ;; (evil-add-hjkl-bindings package-menu-mode-map 'emacs)
  (setq evil-kill-on-visual-paste nil
        evil-insert-state-cursor '((bar . 2) "chartreuse3")
        evil-normal-state-cursor '(box "DarkGoldenrod2")
        evil-visual-state-cursor '((hbox . 2) "gray")
        evil-emacs-state-cursor '(box "SkyBlue2")
        evil-replace-state-cursor '((hbox . 2) "chocolate"))

  (evil-set-undo-system 'undo-tree)

  (evil-define-operator maple/evil-join (beg end)
    "Join the selected lines."
    :motion evil-line
    (let ((sep (read-string "Separator: "))
          (count (count-lines beg end)))
      (when (> count 1)
        (setq count (1- count)))
      (goto-char beg)
      (dotimes (_ count)
        (join-line 1)
        (delete-horizontal-space)
        (insert sep))))

  :custom-face
  (region ((t (:background "gray" :foreground "#272822"))))
  :keybind (:map evil-normal-state-map
                 ("C-k" . evil-scroll-up)
                 ("C-j" . evil-scroll-down)))

(use-package evil-numbers
  :after evil
  :keybind (:map evil-normal-state-map
                 ("+" . evil-numbers/inc-at-pt)
                 ("-" . evil-numbers/dec-at-pt)))

(use-package expand-region
  :after evil
  :keybind (:map evil-visual-state-map
                 ("v" . er/expand-region)
                 ("V" . er/contract-region)))

(use-package evil-leader
  :hook (maple-init . global-evil-leader-mode)
  :config
  (evil-leader/set-leader ","))

(use-package evil-surround
  :hook (maple-init . global-evil-surround-mode)
  :keybind
  (:states visual :map evil-surround-mode-map
           ("s" . evil-surround-region)
           ("S" . evil-substitute)))

(use-package evil-matchit
  :hook (maple-init . global-evil-matchit-mode))

(use-package evil-escape
  :hook (maple-init . evil-escape-mode)
  :diminish 'evil-escape-mode
  :config
  (setq evil-escape-key-sequence "jj"
        evil-escape-delay 0.4
        evil-escape-excluded-major-modes '(dired-mode
                                           neotree-mode
                                           help-mode
                                           magit-mode
                                           shell-mode
                                           term-mode
                                           org-agenda-mode
                                           undo-tree-visualizer-mode)
        evil-escape-inhibit-functions '(evil-visual-state-p
                                        evil-escape--is-magit-buffer)))

(use-package evil-collection
  :init
  (setq evil-want-keybinding nil
        evil-collection-want-unimpaired-p nil
        evil-collection-mode-list '(dired magit ediff image 2048-game neotree xref))
  :hook (evil-mode . evil-collection-init)
  :config
  (defun maple/evil-collection-keybind(mode _keymaps)
    (pcase mode ('dired (evil-define-key 'normal dired-mode-map "H" 'dired-omit-mode))))

  (add-hook 'evil-collection-setup-hook 'maple/evil-collection-keybind))

(provide 'init-evil)
;;; init-evil.el ends here
