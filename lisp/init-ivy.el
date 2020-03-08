;;; init-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-

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
;; Ivy configurations.
;;

;;; Code:

(eval-when-compile (require 'init-basic))

;; 必须的,使用频率排序
(use-package smex
  :config
  (setq smex-save-file (concat maple-cache-directory "smex-items")))

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

  (defun maple/ivy-make-directory-maybe ()
    "Create parent directory if not exists while visiting file."
    (let ((dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p dir)
        (if (y-or-n-p (format "Directory %s does not exist,do you want you create it? " dir))
            (make-directory dir t)
          (keyboard-quit)))))

  (add-to-list 'find-file-not-found-functions 'maple/ivy-make-directory-maybe nil #'eq)

  ;; completion-system
  (with-eval-after-load 'evil
    (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
    (evil-make-overriding-map ivy-occur-mode-map 'normal))

  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  (use-package ivy-xref
    :init
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

  :custom-face
  (ivy-highlight-face ((t (:background nil)))))

(use-package counsel
  :diminish (counsel-mode)
  :hook (ivy-mode . counsel-mode)
  :config
  (setq counsel-preselect-current-file t
        counsel-more-chars-alist '((t . 1))
        counsel-find-file-ignore-regexp "\\.\\(pyc\\|pyo\\)\\'")

  (setq ivy-initial-inputs-alist '((counsel-ag . maple/region-string)
                                   (swiper . maple/region-string)))

  (defun maple/counsel-ag-directory()
    (interactive)
    (counsel-ag nil (read-directory-name "Search in directory: ")))

  ;; custom counsel-ag
  (defun maple/counsel-ag(-counsel-ag &optional initial-input initial-directory extra-ag-args ag-prompt)
    (funcall -counsel-ag
             (or initial-input (maple/region-string))
             (or initial-directory default-directory)
             extra-ag-args
             ag-prompt))

  (defun maple/ivy-dired-occur()
    (interactive)
    (find-name-dired
     (or (projectile-project-root) default-directory) (concat (ivy--input) "*"))
    (ivy-exit-with-action
     (lambda (_)
       (pop-to-buffer (get-buffer "*Find*"))
       (dired-hide-details-mode)
       (wdired-change-to-wdired-mode)
       (when (bound-and-true-p evil-local-mode) (evil-normal-state)))))

  (advice-add 'counsel-ag :around #'maple/counsel-ag)

  (use-package counsel-projectile
    :preface (setq projectile-keymap-prefix (kbd "C-c p")))

  (use-package swiper)

  :bind (("M-x" . counsel-M-x)
         ("C-x C-m" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
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
         ("C-c C-e" . maple/ivy-dired-occur)
         ([tab] . maple/ivy-done)
         ([backspace] . maple/ivy-backward-delete-char)
         :map counsel-ag-map
         ([tab] . ivy-call)
         :map swiper-map
         ([tab] . ivy-done)))

(use-package ivy-rich
  :hook (counsel-mode . ivy-rich-mode)
  :config
  (setq ivy-rich-path-style 'abbrev
        ivy-rich-switch-buffer-align-virtual-buffer t)

  (defun ivy-rich-format (candidate columns &optional delimiter)
    (mapconcat
     (lambda (column)
       (or (ivy-rich-format-column candidate column) ""))
     columns " "))

  (use-package all-the-icons-ivy-rich
    :if (and (display-graphic-p) *icon*)
    :demand
    :config
    (setq all-the-icons-scale-factor 1.15
          all-the-icons-icon-alist
          (append
           (butlast all-the-icons-icon-alist)
           (list '("." all-the-icons-octicon "book" :height 1.0 :v-adjust 0.0 :face all-the-icons-lcyan))))

    (defun maple/ivy-file-transformer(candidate)
      "Advice ivy-read-file-transformer with `CANDIDATE`."
      (format "%s %s" (propertize "\t" 'display (all-the-icons-ivy-rich-file-icon candidate)) candidate))

    (with-eval-after-load 'projectile
      (ivy-set-display-transformer
       'projectile-completing-read 'maple/ivy-file-transformer))

    (advice-add 'all-the-icons-ivy-rich--format-icon :filter-return 'string-trim-left)
    (advice-add 'ivy-rich-bookmark-type :override 'all-the-icons-ivy-rich-bookmark-type)
    (setq ivy-rich-display-transformers-list all-the-icons-ivy-rich-display-transformers-list)))

(use-package ivy-posframe
  :disabled
  :hook (ivy-mode . ivy-posframe-mode)
  :config
  (defun ivy-posframe-get-size ()
    (list
     :height (or ivy-posframe-height (+ ivy-height 1))
     :width (or ivy-posframe-width (window-pixel-width))))

  (setq ivy-posframe-style 'window-bottom-left
        ivy-posframe-parameters
        '((left-fringe . 5)
          (right-fringe . 5)
          (minibuffer . only))))

(provide 'init-ivy)
;;; init-ivy.el ends here
