;;; init-company.el --- Initialize company configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/dotfiles/tree/master/emacs.d

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
(eval-when-compile (require 'init-basic))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (maple-init . yas-global-mode)
  :config
  (setq yas-triggers-in-field t
        yas-prompt-functions '(yas-completing-prompt))
  (use-package yasnippet-snippets))

(use-package company
  :diminish company-mode
  :hook (maple-init . global-company-mode)
  :config
  (setq company-idle-delay 0.1
        company-show-numbers t
        company-tooltip-limit 15
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t ;; 提示右对齐
        company-dabbrev-downcase nil
        ;; company-transformers '(company-sort-by-occurrence) ;; 按使用频次排序
        company-begin-commands '(self-insert-command)
        company-global-modes '(not comint-mode
                                   erc-mode
                                   gud-mode
                                   rcirc-mode
                                   sql-interactive-mode
                                   minibuffer-inactive-mode
                                   inferior-python-mode
                                   shell-mode
                                   evil-command-window-mode))
  (setq company-backends maple-language:complete-backends)
  :custom-face
  (company-tooltip-common
   ((t (:inherit company-tooltip :weight bold :underline nil))))
  (company-tooltip-common-selection
   ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
  :bind (:map company-active-map
              ("C-d" . company-show-doc-buffer)
              ("C-j" . company-select-next)
              ("C-k" . company-select-previous)
              ("TAB" . company-complete-common-or-cycle)
              ("<tab>" . company-complete-common-or-cycle)
              ("<RET>" . company-complete-selection)))

(use-package company-statistics
  :hook (company-mode . company-statistics-mode)
  :config
  (setq company-statistics-file (expand-file-name "company-statistics.el" maple-cache-directory)))

(use-package company-box
  :disabled
  :if (and (display-graphic-p) *icon*)
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors nil
        company-box-show-single-candidate t
        company-box-max-candidates 50
        company-box-doc-delay 0.5
        company-box-icons-alist 'company-box-icons-all-the-icons)

  (defun company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
              ((featurep sym) 'Module)
              ((facep sym) 'Color)
              ((boundp sym) 'Variable)
              (t  'Unknown)))))

  (defun company-box-icons--yasnippet (candidate)
    (when (get-text-property 0 'yas-annotation candidate)
      'Snippet)))

(use-package company-quickhelp
  :disabled
  :if (display-graphic-p)
  :hook (company-mode . company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay 1))

(provide 'init-company)
;;; init-company.el ends here
