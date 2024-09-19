;;; init-completion.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-

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
;; Ivy configurations.
;;

;;; Code:
(defvar maple-icon)

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

;; (use-package vertico
;;   :hook ((maple-init . vertico-mode)
;;          (rfn-eshadow-update-overlay . vertico-directory-tidy))
;;   :custom
;;   (vertico-cycle t)
;;   (vertico-count 10)
;;   (vertico-resize nil)
;;   (vertico-preselect 'first)
;;   (enable-recursive-minibuffers t)
;;   :config

;;   (defun maple/vertico-done ()
;;     (interactive)
;;     (when (> vertico--total 0)
;;       (let* ((vertico--index (max 0 vertico--index))
;;              (cand (vertico--candidate)))
;;         (if (equal (minibuffer-contents-no-properties) cand)
;;             (vertico-exit)
;;           (insert (prog1 cand (delete-minibuffer-contents)))))))

;;   (defun maple/vertico-format(x)
;;     (concat (make-string (window-left-column (minibuffer-selected-window)) ?\s) x))

;;   (advice-add #'vertico--format-count :filter-return #'maple/vertico-format)
;;   (advice-add #'vertico--format-candidate :filter-return #'maple/vertico-format)

;;   :keybind (:map vertico-map
;;                  ("C-j" . vertico-next)
;;                  ("C-k" . vertico-previous)
;;                  ("C-h" . vertico-directory-delete-char)
;;                  ([tab] . maple/vertico-done)
;;                  ([escape] . minibuffer-keyboard-quit)))

;; (use-package nerd-icons-completion
;;   :if maple-icon
;;   :hook (vertico-mode . nerd-icons-completion-mode))

;; (use-package consult
;;   :commands (consult-recent-file)
;;   :custom
;;   (consult-narrow-key "<")
;;   (consult-preview-key nil)
;;   (consult-line-numbers-widen t)
;;   (consult-async-min-input 2)
;;   (consult-async-refresh-delay  0.15)
;;   (consult-async-input-throttle 0.2)
;;   (consult-async-input-debounce 0.1)
;;   :config

;;   (consult-customize
;;    consult-line
;;    consult-find
;;    consult-grep
;;    consult-ripgrep
;;    :initial
;;    (let ((str (maple-region-string)))
;;      (add-hook 'after-change-functions 'maple/consult-initial nil t)
;;      (propertize str 'face 'shadow))
;;    consult-line
;;    :preview-key '(:debounce 0.2 any))

;;   (defun maple/consult-initial(&optional beg end len string)
;;     (remove-hook 'after-change-functions 'maple/consult-initial t)
;;     (let ((string (buffer-substring-no-properties beg end)))
;;       (delete-minibuffer-contents)
;;       (insert string)))

;;   (defun maple/consult-find (&optional dir initial)
;;     (interactive "P")
;;     (call-interactively 'consult-find t (vector (or dir default-directory) initial)))

;;   (defun maple/consult-grep (&optional dir initial)
;;     (interactive "P")
;;     (call-interactively 'consult-ripgrep t (vector (or dir default-directory) initial)))

;;   (defun maple/consult-prompt-grep (&optional dir initial)
;;     (interactive "P")
;;     (call-interactively 'consult-ripgrep t (vector (or dir t) initial)))

;;   (defun maple/consult-project-grep (&optional dir initial)
;;     (interactive "P")
;;     (call-interactively 'consult-ripgrep t (vector dir initial)))

;;   :keybind
;;   (([remap imenu]                    . consult-imenu)
;;    ([remap yank]                     . consult-yank-from-kill-ring)
;;    ([remap yank-pop]                 . counsel-yank-pop)
;;    ([remap goto-line]                . consult-goto-line)
;;    ([remap bookmark-jump]            . consult-bookmark)
;;    ([remap switch-to-buffer]         . consult-buffer)
;;    ([remap recentf-open-files]       . consult-recent-file)
;;    ([remap project-switch-to-buffer] . consult-project-buffer)
;;    ([remap isearch-backward]         . consult-line)))

;; (use-package embark
;;   :custom
;;   (which-key-use-C-h-commands nil)
;;   (prefix-help-command #'embark-prefix-help-command))

;; (use-package embark-consult
;;   :keybind (:map minibuffer-mode-map
;;                  ("C-c C-e" . embark-export)))

;; (use-package marginalia
;;   :hook (maple-init . marginalia-mode))

(use-package corfu
  :hook ((prog-mode . corfu-mode)
         (corfu-mode . corfu-popupinfo-mode))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  :keybind (:map corfu-map
                 ("TAB" . corfu-next)
                 ([tab] . corfu-next)
                 ("S-TAB" . corfu-previous)
                 ([backtab] . corfu-previous)))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :hook (corfu-mode . corfu-terminal-mode))

(use-package nerd-icons-corfu
  :if maple-icon
  :hook (corfu-mode . (lambda() (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))))

(use-package cape
  :after corfu :demand
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

(use-package yasnippet
  :hook (maple-init . yas-global-mode)
  :custom
  (yas-triggers-in-field t)
  (yas-prompt-functions '(yas-completing-prompt))
  :dependencies
  (yasnippet-snippets))

(provide 'init-completion)
;;; init-completion.el ends here