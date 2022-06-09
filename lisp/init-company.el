;;; init-company.el --- Initialize company configurations.	-*- lexical-binding: t -*-

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
;; Basic configurations.
;;

;;; Code:
(use-package yasnippet
  :hook (maple-init . yas-global-mode)
  :custom
  (yas-triggers-in-field t)
  (yas-prompt-functions '(yas-completing-prompt))
  :dependencies
  (yasnippet-snippets))

(use-package company
  :hook (maple-init . global-company-mode)
  :config
  (setq company-yasnippet-annotation-fn
        (lambda(name) (concat (unless company-tooltip-align-annotations " -> ") name " (Snip)"))
        company-backends (maple-language/complete-backend))

  (unless nil
    (setq company-format-margin-function 'company-text-icons-margin)
    (setq company-format-margin-function nil))

  (defun maple/company-yasnippet ()
    (interactive)
    (let ((backend company-backend))
      (company-cancel)
      (company-begin-backend
       (if (eq backend 'company-yasnippet)
           (car company-backends)
         'company-yasnippet))))

  (defun maple/company-yasnippet-advice (fun command &optional arg &rest ignore)
    (if (eq command 'prefix)
        (let ((prefix (funcall fun command)))
          (unless (or (not prefix) (memq (char-before (- (point) (length prefix))) '(?. ?> ?\())) prefix))
      (funcall fun command arg ignore)))

  (advice-add 'company-yasnippet :around 'maple/company-yasnippet-advice)
  :custom
  (company-idle-delay 0.1)
  (company-show-numbers t)
  (company-tooltip-limit 15)
  (company-tooltip-align-annotations t) ;; 提示右对齐
  (company-tooltip-offset-display 'lines)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-dabbrev-downcase nil)
  (company-begin-commands '(self-insert-command))
  (company-global-modes '(not comint-mode
                              erc-mode
                              gud-mode
                              rcirc-mode
                              sql-interactive-mode
                              minibuffer-inactive-mode
                              inferior-python-mode
                              shell-mode
                              evil-command-window-mode))
  (:face
   (company-tooltip-common
    ((t (:inherit company-tooltip :weight bold :underline nil))))
   (company-tooltip-common-selection
    ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
  :keybind (:map company-active-map
                 ("C-d" . company-show-doc-buffer)
                 ("C-j" . company-select-next)
                 ("C-k" . company-select-previous)
                 ("<backtab>" . maple/company-yasnippet)
                 ("TAB" . company-complete-common-or-cycle)
                 ("<tab>" . company-complete-common-or-cycle)
                 ("<RET>" . company-complete-selection)))

(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :custom
  (prescient-save-file (expand-file-name "prescient-save.el" maple-cache-directory)))

(use-package company-english-helper
  :quelpa (:fetcher github :repo "manateelazycat/company-english-helper")
  :commands (company-english-helper-search)
  :custom
  (:mode
   (org-mode markdown-mode)
   (company-tooltip-align-annotations nil))
  (:language
   (markdown-mode org-mode)
   :complete 'company-english-helper-search))

(provide 'init-company)
;;; init-company.el ends here
