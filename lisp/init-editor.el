;;; init-editor.el --- Initialize editor configurations.	-*- lexical-binding: t -*-

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
;; Editor configurations.
;;

;;; Code:

(eval-when-compile (require 'init-basic))

(use-package autorevert
  :ensure nil
  :hook (maple-init . global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  :diminish auto-revert-mode)

(use-package elec-pair
  :ensure nil
  :hook (maple-init . electric-pair-mode)
  :config
  ;; (setq electric-pair-pairs '((?\' . ?\')))
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; 高亮括号配对
(use-package paren
  :ensure nil
  :hook (maple-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package xref
  :ensure nil
  :config
  (maple/evil-map xref--xref-buffer-mode-map))

(use-package tramp
  :ensure nil
  :config
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"))

(use-package isearch
  :ensure nil
  :config
  (defun maple/evil-search-paste()
    (when (use-region-p)
      (isearch-yank-string
       (buffer-substring-no-properties
        (region-beginning) (1+ (region-end))))
      (deactivate-mark)))

  :hook (isearch-mode . maple/evil-search-paste)
  :bind (:map isearch-mode-map
              ([remap isearch-delete-char] . isearch-del-char)))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

(use-package comint
  :ensure nil
  :evil-state (comint-mode . insert)
  :hook (comint-mode . maple/close-process)
  :config
  (setq comint-prompt-read-only t)
  :bind
  (:map comint-mode-map
        ("<up>" . comint-previous-input)
        ("<down>" . comint-next-input)
        ("<escape>" . (lambda() (interactive)
                        (goto-char (cdr comint-last-prompt))))))

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :hook ((conf-mode prog-mode) . hs-minor-mode))

(use-package anzu
  :hook (maple-init . global-anzu-mode)
  :config
  (setq anzu-cons-mode-line-p nil
        anzu-kode-lighter ""
        anzu-search-threshold 1000
        anzu-replace-to-string-separator " → ")
  ;; (use-package evil-anzu :after evil :demand)
  :custom-face
  (anzu-replace-to ((t (:inherit query-replace))))
  :bind (:map query-replace-map
              ([return] . 'automatic)))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  :bind (:map wgrep-mode-map
              ("C-c C-c" . wgrep-finish-edit)))

(use-package projectile
  :diminish projectile-mode "ⓟ"
  :hook (maple-init . projectile-mode)
  :config
  (setq projectile-sort-order 'recentf
        projectile-cache-file
        (expand-file-name "projectile.cache" maple-cache-directory)
        projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" maple-cache-directory)))


(provide 'init-editor)

;;; init-editor.el ends here
