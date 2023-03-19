;;; init-editor.el --- Initialize editor configurations.	-*- lexical-binding: t -*-

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
;; Editor configurations.
;;

;;; Code:
(use-package so-long
  :ensure nil
  :hook (maple-init . global-so-long-mode))

(use-package autorevert
  :ensure nil
  :hook (maple-init . global-auto-revert-mode)
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :diminish auto-revert-mode)

(use-package elec-pair
  :ensure nil
  :hook (maple-init . electric-pair-mode)
  :custom
  ;; (electric-pair-pairs '((?\' . ?\')))
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package align
  :ensure nil
  :config
  (defun maple/align-regexp(func &rest args)
    (let ((indent-tabs-mode nil)) (apply func args)))

  (advice-add 'align-regexp :around 'maple/align-regexp))

(use-package xref
  :ensure nil
  :custom
  (xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package tramp
  :ensure nil
  :custom
  (tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :hook ((conf-mode prog-mode) . hs-minor-mode)
  :config
  (defun maple/hs-overlay (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let ((face '((t (:inherit 'font-lock-comment-face :underline t))))
            (nlines (count-lines (overlay-start ov) (overlay-end ov))))
        (overlay-put ov 'display (propertize (format "...#%d" nlines) 'face face)))))
  (setq hs-set-up-overlay 'maple/hs-overlay))

(use-package isearch
  :ensure nil
  :hook (isearch-mode . (lambda() (when (use-region-p)
                                    (isearch-yank-string
                                     (buffer-substring-no-properties
                                      (region-beginning) (1+ (region-end))))
                                    (deactivate-mark))))
  :keybind (:map isearch-mode-map
                 ([remap isearch-delete-char] . isearch-del-char)))

(use-package anzu
  :diminish anzu-mode
  :hook (maple-init . global-anzu-mode)
  :custom
  (anzu-cons-mode-line-p nil)
  (anzu-kode-lighter "")
  (anzu-search-threshold 1000)
  (anzu-replace-to-string-separator " → ")
  (:face
   (anzu-replace-to ((t (:inherit query-replace)))))
  :keybind (:map query-replace-map
                 ([return] . automatic)))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t)
  :keybind (:map wgrep-mode-map
                 ("C-c C-c" . wgrep-finish-edit)))

(use-package avy
  :custom
  (avy-all-windows t)
  (avy-background t)
  :keybind
  (:states (normal visual) ("F" . avy-goto-char)))

(use-package ace-pinyin
  :diminish ace-pinyin-mode
  :hook (maple-init . ace-pinyin-global-mode))

(use-package pangu-spacing
  :commands (pangu-spacing-space-current-buffer)
  :config
  (defun pangu-spacing-search-and-replace (match regexp)
    (let* ((p (use-region-p))
           (start (if p (region-beginning) (point-min)))
           (end (if p (region-end) (point-max))))
      (pangu-spacing-search-buffer regexp start end (replace-match match nil nil)))))

(use-package edit-indirect
  :commands (edit-indirect-region))

(use-package string-inflection
  :commands (maple/string-inflection-toggle)
  :config
  (defun maple/string-inflection-toggle()
    (interactive)
    (save-excursion (call-interactively 'string-inflection-toggle)))
  :keybind
  (:states normal ("gr" . maple/string-inflection-toggle))
  (:states visual ("gr" . maple/string-inflection-toggle)))

(use-package whitespace
  :ensure nil
  :hook ((prog-mode conf-mode yaml-mode org-mode) . whitespace-mode)
  :custom
  (whitespace-action '(auto-cleanup))
  (whitespace-style '(face
                      trailing indentation empty
                      space-before-tab space-after-tab))
  :diminish whitespace-mode "ⓦ")

(use-package projectile
  :diminish projectile-mode "ⓟ"
  :hook (maple-init . projectile-mode)
  :custom
  (projectile-sort-order 'recentf)
  (projectile-current-project-on-switch 'keep)
  (projectile-cache-file (maple-cache-file "projectile.cache"))
  (projectile-known-projects-file (maple-cache-file "projectile-bookmarks.eld"))
  (:function
   (add-to-list 'projectile-project-root-files-bottom-up "go.mod" t)))

(provide 'init-editor)
;;; init-editor.el ends here
