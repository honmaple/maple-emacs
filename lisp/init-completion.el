;;; init-completion.el --- Initialize completion configurations.	-*- lexical-binding: t -*-

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
;; Completion configurations.
;;

;;; Code:
(defvar maple-icon)

(use-package orderless
  :after-call maple-init-hook
  :custom
  (completion-ignore-case t)
  ;; 不要添加这一行，会导致目录搜索有问题
  ;; (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-styles '(orderless basic)))

(use-package pinyinlib
  :after-call orderless
  :config
  (defun orderless-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'orderless-pinyin))

(use-package vertico
  :hook ((maple-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :custom
  (vertico-cycle t)
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-preselect 'first)
  (enable-recursive-minibuffers t)
  :config

  (defun maple/vertico-done ()
    (interactive)
    (when (> vertico--total 0)
      (let* ((vertico--index (max 0 vertico--index))
             (cand (vertico--candidate)))
        (if (equal (minibuffer-contents-no-properties) cand)
            (vertico-exit)
          (insert (prog1 cand (delete-minibuffer-contents)))))))

  (defun maple/vertico-delete ()
    (interactive)
    (if (eq 'file (vertico--metadata-get 'category))
        (vertico-directory-up)
      (vertico-directory-delete-char)))

  (defun maple/vertico-format(x)
    (concat (make-string (window-left-column (minibuffer-selected-window)) ?\s) x))

  (advice-add #'vertico--format-count :filter-return #'maple/vertico-format)
  (advice-add #'vertico--format-candidate :filter-return #'maple/vertico-format)
  :keybind (:map vertico-map
                 ("C-j" . vertico-next)
                 ("C-k" . vertico-previous)
                 ("C-h" . maple/vertico-delete)
                 ([escape] . keyboard-escape-quit)))

(use-package vertico-multiform
  :ensure vertico
  :hook (vertico-mode . vertico-multiform-mode)
  :custom
  (vertico-multiform-commands
   '((execute-extended-command
      (vertico-transform-function . maple/vertico-highlight-mode))))
  (vertico-multiform-categories
   '((file (vertico-transform-function . maple/vertico-highlight-directory))))
  :config
  (defvar vertico-transform-function nil)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not vertico-transform-function) null))
    (dolist (fun (ensure-list vertico-transform-function))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  (defun maple/vertico-highlight-directory (file)
    (if (string-suffix-p "/" file)
        (propertize file 'face 'dired-directory)
      file))

  (defun maple/vertico-highlight-mode (cmd)
    (let ((sym (intern cmd)))
      (if (or (eq sym major-mode)
              (and (memq sym minor-mode-list) (boundp sym)))
          (propertize cmd 'face 'font-lock-constant-face)
        cmd))))

(use-package consult
  :commands (consult-recent-file maple/find-file maple/project-find-file maple/consult-grep maple/consult-project-grep)
  :custom
  (consult-narrow-key "<")
  (consult-preview-key nil)
  (consult-line-numbers-widen t)
  (consult-async-min-input 2)
  (consult-async-split-style nil)
  (consult-async-refresh-delay  0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  :config

  (defvar maple/consult-preview-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<tab>") #'ignore)
      map))

  (consult-customize
   consult-ripgrep
   :initial (maple-region-string)
   :keymap maple/consult-preview-map
   :preview-key "<tab>"
   consult-line
   consult-imenu
   :keymap maple/consult-preview-map
   :initial (maple-region-string)
   :preview-key '(:debounce 0.2 any))

  (defun maple/completion-table (cands &optional category)
    (lambda (string pred action)
      (cond
       ((eq action 'metadata)
        (cons 'metadata (list (cons 'category (or category 'file)))))
       (t
        (complete-with-action action cands string pred)))))

  (defun maple/find-file ()
    (interactive)
    (let* ((cmd "rg --files --color=never")
           (cands (split-string (shell-command-to-string cmd) "\n" t))
           (file (completing-read "Find file: " (maple/completion-table cands) nil t)))
      (find-file file)))

  (defun maple/project-find-file ()
    (interactive)
    (let* ((default-directory (project-root (project-current))))
      (call-interactively 'maple/find-file)))

  (defun maple/consult-grep (&optional dir initial)
    (interactive "P")
    (let ((this-command 'consult-ripgrep))
      (consult-ripgrep (or dir default-directory) initial)))

  (defun maple/consult-prompt-grep (&optional dir initial)
    (interactive "P")
    (let ((this-command 'consult-ripgrep))
      (consult-ripgrep (or dir t) initial)))

  (defun maple/consult-project-grep (&optional dir initial)
    (interactive "P")
    (let ((this-command 'consult-ripgrep))
      (consult-ripgrep dir initial)))

  (defun maple/consult-project-flymake ()
    (interactive)
    (let ((this-command 'consult-flymake))
      (consult-flymake t)))

  (defun maple/consult-process()
    (interactive)
    (let* ((name (completing-read "Process list: " (mapcar 'process-name (process-list)) nil t))
           (proc (get-process name))
           (buf (and proc (process-buffer proc))))
      (if buf (switch-to-buffer buf)
        (message "Process %s doesn't have a buffer" name))))

  :keybind
  (([remap imenu]                    . consult-imenu)
   ([remap yank]                     . consult-yank-from-kill-ring)
   ([remap yank-pop]                 . consult-yank-pop)
   ([remap goto-line]                . consult-goto-line)
   ([remap bookmark-jump]            . consult-bookmark)
   ([remap switch-to-buffer]         . consult-buffer)
   ([remap recentf-open-files]       . consult-recent-file)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap isearch-backward]         . consult-line)))

(use-package embark
  :custom
  (which-key-use-C-h-commands nil)
  (prefix-help-command #'embark-prefix-help-command)
  (embark-prompter 'embark-completing-read-prompter)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :config
  (defun embark-export@ensure ()
    (pcase (completion-metadata-get (embark--metadata) 'category)
      ('consult-grep
       (with-eval-after-load 'wgrep
         (run-with-idle-timer 0 nil 'wgrep-change-to-wgrep-mode))))

    (with-eval-after-load 'evil
      (run-with-idle-timer 0 nil 'evil-normal-state)))

  (advice-add 'embark-export :before 'embark-export@ensure)
  :keybind
  (:map minibuffer-mode-map
        ("C-c a"   . embark-act)
        ("C-c C-e" . embark-export))
  :dependencies
  (embark-consult
   :after-call embark-export))

(use-package marginalia
  :hook (maple-init . marginalia-mode))

(use-package nerd-icons-completion
  :if maple-icon
  :hook (marginalia-mode . nerd-icons-completion-mode)
  :custom-face
  (nerd-icons-completion-dir-face ((t (:inherit dired-directory)))))

(use-package corfu
  :hook (prog-mode . corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-preview-current nil)
  :keybind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))

(use-package corfu-popupinfo
  :ensure corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.4 . 0.2)))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :hook (corfu-mode . corfu-terminal-mode))

(use-package nerd-icons-corfu
  :if maple-icon
  :hook (corfu-mode . (lambda() (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))))

(use-package cape
  :after-call maple-init-hook
  :custom
  (completion-at-point-functions (list (cape-capf-super 'cape-file 'cape-dabbrev 'cape-abbrev 'cape-keyword))))

(use-package yasnippet
  :hook (maple-init . yas-global-mode)
  :custom
  (yas-triggers-in-field t)
  (yas-prompt-functions '(yas-completing-prompt))
  :dependencies
  (yasnippet-snippets)
  (yasnippet-capf
   :after-call yas-global-mode
   :config
   (add-to-list 'completion-at-point-functions #'yasnippet-capf t)))

(provide 'init-completion)
;;; init-completion.el ends here