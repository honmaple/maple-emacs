;;; init-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-

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

;; 必须的,使用频率排序
(use-package smex
  :custom
  (smex-save-file (maple-cache-file "smex-items")))

(use-package ivy
  :diminish (ivy-mode)
  :hook (maple-init . ivy-mode)
  :defines
  (magit-completing-read-function
   projectile-completion-system)
  :config
  (setq enable-recursive-minibuffers t
        completing-read-function 'ivy-completing-read)

  (setq ivy-use-selectable-prompt t
        ivy-wrap t
        ivy-extra-directories nil
        ivy-fixed-height-minibuffer t
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ;; disable magic slash on non-match
        ;; ~ to /home/user
        ivy-magic-tilde t
        ivy-magic-slash-non-match-action nil
        ivy-use-virtual-buffers nil
        ivy-virtual-abbreviate 'full
        ;; ivy display
        ivy-format-functions-alist
        '((t . maple/ivy-format-function))
        ;; fuzzy match
        ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))

  ;; custom ivy display function
  (defvar ivy-format-padding nil)

  (defun maple/ivy-read-before (&rest args)
    "Advice ivy-read with `ARGS`."
    (setq ivy-format-padding (make-string (window-left-column) ?\s)
          ivy-count-format (concat ivy-format-padding "(%d/%d) ")))

  (advice-add 'ivy-read :before #'maple/ivy-read-before)

  (defun maple/ivy-format-function (cands)
    "Transform CANDS into a string for minibuffer."
    (ivy--format-function-generic
     (lambda (str)
       (concat ivy-format-padding (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat ivy-format-padding str))
     cands "\n"))

  ;; complete or done
  (defun maple/ivy-done()
    (interactive)
    (let ((dir ivy--directory))
      (ivy-partial-or-done)
      (when (string= dir ivy--directory)
        (ivy-insert-current)
        (when (and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
                   (setq dir (ivy-expand-file-if-directory (ivy-state-current ivy-last))))
          (ivy--cd dir)
          (setq this-command 'ivy-cd)))))

  (defun maple/ivy-backward-delete-char ()
    (interactive)
    (let ((dir ivy--directory)
          (p (and ivy--directory (= (minibuffer-prompt-end) (point)))))
      (ivy-backward-delete-char)
      (when p (insert (file-name-nondirectory (directory-file-name dir))))))

  (defun maple/ivy-c-h ()
    (interactive)
    (if (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
        (if (string-equal (ivy--input) "")
            (counsel-up-directory)
          (delete-minibuffer-contents))
      (ivy-backward-delete-char)))

  ;; ivy-occur custom
  (defun maple/ivy-edit ()
    "Edit the current search results in a buffer using wgrep."
    (interactive)
    (run-with-idle-timer 0 nil 'ivy-wgrep-change-to-wgrep-mode)
    (ivy-occur))

  ;; completion-system
  (with-eval-after-load 'evil
    (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
    (evil-make-overriding-map ivy-occur-mode-map 'normal)
    (evil-make-overriding-map ivy-occur-grep-mode-map 'normal))

  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  :custom-face
  (ivy-highlight-face ((t (:background nil))))
  :dependencies
  (swiper)
  (ivy-xref
   :custom
   (xref-show-xrefs-function #'ivy-xref-show-xrefs)
   (xref-show-definitions-function #'ivy-xref-show-defs)))

(use-package counsel
  :diminish (counsel-mode)
  :hook (ivy-mode . counsel-mode)
  :config
  (setq counsel-preselect-current-file t
        counsel-more-chars-alist '((t . 1))
        counsel-find-file-ignore-regexp "\\.\\(pyc\\|pyo\\)\\'")

  (setq ivy-initial-inputs-alist
        (mapcar (lambda(x) (cons x 'maple-region-string))
                '(swiper
                  counsel-rg
                  counsel-ag
                  counsel-grep
                  counsel-git-grep
                  counsel-find-file)))

  (defun maple/counsel-grep(&optional initial-input initial-directory)
    (interactive)
    (cond ((executable-find "rg") (counsel-rg initial-input initial-directory))
          ((executable-find "ag") (counsel-ag initial-input initial-directory))
          (t (counsel-grep initial-input))))

  (defun maple/counsel-grep-dir()
    (interactive)
    (maple/counsel-grep nil (read-directory-name "Search in directory: ")))

  (defun maple/counsel-grep-parent-dir ()
    "Search upwards in the directory tree."
    (interactive)
    (ivy-quit-and-run
      (maple/counsel-grep ivy-text (file-name-directory (directory-file-name default-directory)))))

  ;; custom counsel-ag
  (defun maple/counsel-ag(oldfunc &optional initial-input initial-directory extra-ag-args ag-prompt &key caller)
    (let* ((int (or initial-input (maple-region-string)))
           (dir (or initial-directory default-directory))
           (prompt (concat ag-prompt (abbreviate-file-name (directory-file-name dir)) ": ")))
      (funcall oldfunc int dir extra-ag-args prompt :caller caller)))

  (advice-add 'counsel-ag :around #'maple/counsel-ag)

  :keybind (([remap imenu]                    . counsel-semantic-or-imenu)
            ([remap yank-pop]                 . counsel-yank-pop)
            ([remap bookmark-jump]            . counsel-bookmark)
            ([remap switch-to-buffer]         . counsel-switch-buffer)
            ([remap recentf-open-files]       . counsel-recentf)
            ([remap execute-extended-command] . counsel-M-x)
            ([remap isearch-backward]         . counsel-grep-or-swiper)
            :map ivy-minibuffer-map
            ("C-j" . ivy-next-line)
            ("C-k" . ivy-previous-line)
            ("C-h" . maple/ivy-c-h)
            ("C-c C-e" . maple/ivy-edit)
            ([tab] . maple/ivy-done)
            ([escape] . minibuffer-keyboard-quit)
            ([backspace] . maple/ivy-backward-delete-char)
            :map ivy-switch-buffer-map
            ("C-k" . ivy-previous-line)
            :map counsel-find-file-map
            ("C-<return>" . ivy-immediate-done)
            ([tab] . maple/ivy-done)
            ([backspace] . maple/ivy-backward-delete-char)
            :map counsel-rg-map
            ([tab] . ivy-call)
            ("C-s" . maple/counsel-grep-parent-dir)
            :map counsel-ag-map
            ([tab] . ivy-call)
            ("C-s" . maple/counsel-grep-parent-dir)
            :map counsel-grep-map
            ([tab] . ivy-call)
            :map counsel-git-grep-map
            ([tab] . ivy-call)
            :map swiper-map
            ([tab] . ivy-done)))

(use-package counsel-projectile
  :commands (maple/counsel-projectile-grep)
  :config
  (defun maple/counsel-projectile-grep()
    (interactive)
    (call-interactively
     (cond ((executable-find "rg") 'counsel-projectile-rg)
           ((executable-find "ag") 'counsel-projectile-ag)
           (t 'counsel-projectile-grep)))))

(use-package ivy-rich
  :hook (counsel-mode . ivy-rich-mode)
  :config
  (setq ivy-rich-path-style 'abbrev)

  (defun ivy-rich-format (candidate columns &optional delimiter)
    (mapconcat
     (lambda (column)
       (or (ivy-rich-format-column candidate column) ""))
     columns " "))

  (use-package all-the-icons-ivy-rich
    :if (and (display-graphic-p) maple-icon)
    :demand
    :config
    (defun maple/ivy-file-transformer(candidate)
      "Advice ivy-read-file-transformer with `CANDIDATE`."
      (format "%s %s" (propertize "\t" 'display (all-the-icons-ivy-rich-file-icon candidate)) candidate))

    (with-eval-after-load 'projectile
      (ivy-set-display-transformer
       'projectile-completing-read 'maple/ivy-file-transformer))

    (advice-add 'all-the-icons-ivy-rich--project-root :override 'ignore)
    (advice-add 'all-the-icons-ivy-rich-file-id :override #'(lambda(x) ""))

    (advice-add 'all-the-icons-ivy-rich--format-icon :filter-return 'string-trim-left)
    (setq ivy-rich-display-transformers-list all-the-icons-ivy-rich-display-transformers-list)))

(provide 'init-ivy)
;;; init-ivy.el ends here
