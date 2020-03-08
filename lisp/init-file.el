;;; init-file.el --- Initialize file configurations.	-*- lexical-binding: t -*-

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
;; file configurations.
;;

;;; Code:

(use-package recentf
  :ensure nil
  :init
  (maple/add-hook 'find-file-hook
    (unless recentf-mode
      (recentf-mode)
      (recentf-track-opened-file)))
  :config
  (setq recentf-save-file (concat maple-cache-directory "recentf")
        recentf-max-saved-items 100
        recentf-auto-cleanup 'never
        recentf-exclude (list "\\.\\(png\\|jpg\\)\\'"
                              "COMMIT_EDITMSG\\'"
                              (expand-file-name maple-cache-directory)
                              ;; (expand-file-name package-user-dir)
                              )))

(use-package savehist
  :ensure nil
  :hook (maple-init . savehist-mode)
  :config
  ;; Minibuffer history
  (setq savehist-file (concat maple-cache-directory "savehist")
        savehist-autosave-interval nil ; save on kill only
        savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring)))

(use-package saveplace
  :ensure nil
  :hook (maple-init . save-place-mode)
  :config
  (setq save-place-file (concat maple-cache-directory "places")))

(use-package neotree
  :commands neo-global--window-exists-p
  :config
  (setq neo-create-file-auto-open t
        neo-banner-message nil
        neo-show-updir-line t
        neo-mode-line-type 'default
        neo-cwd-line-style 'button
        neo-smart-open t
        neo-show-hidden-files nil
        neo-auto-indent-point t
        neo-vc-integration '(face))
  (maple/evil-map neotree-mode-map)

  :bind (([f2] . neotree-toggle)
         :map neotree-mode-map
         ("C" . neotree-copy-node)
         ("D" . neotree-delete-node)
         ("R" . neotree-rename-node)
         ("+" . neotree-create-node)
         ("^" . neotree-select-up-node)))

(use-package undo-tree
  :ensure nil
  :hook (maple-init . global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  (let ((dir (expand-file-name "undo-tree" maple-cache-directory)))
    (unless (file-exists-p dir) (make-directory dir t))
    (setq undo-tree-history-directory-alist (list (cons "." dir))))
  :diminish undo-tree-mode)

(use-package maple-file
  :ensure nil
  :demand
  :hook (find-file . maple-file/check-large))

(use-package maple-buffer
  :ensure nil
  :demand)

(provide 'init-file)
;;; init-file.el ends here
