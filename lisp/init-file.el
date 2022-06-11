;;; init-file.el --- Initialize file configurations.	-*- lexical-binding: t -*-

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
;; file configurations.
;;
;; Dired:
;; C-x C-j打开当前文件所在目录
;; C-x C-f 新建文件
;; + 新建目录
;; m 标记文件
;; u 取消标记
;; U 取消所有标记
;; D 直接删除文件
;; R 重命名
;; d 标记删除
;; c 标记拷贝文件
;; C 直接拷贝文件
;; q 退出
;; H 显示隐藏文件
;; w 复制文件名
;;

;;; Code:
(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-copies 'always ;;递归拷贝
        dired-recursive-deletes 'always
        dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil)  ;; 只有一个buffer
  :keybind (:map dired-mode-map
                 ("RET" . dired-find-alternate-file)
                 ("C-c C-e" . wdired-change-to-wdired-mode)))

(use-package dired-async
  :ensure async
  :diminish dired-async-mode
  :hook (dired-mode-hook . dired-async-mode))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  (dired-omit-files
   (concat dired-omit-files "\\|^\\..+$\\|\\.pdf$\\|\\.tex$\\|\\*~$"))
  :keybind (:map dired-mode-map ("H" . dired-omit-mode)))

(use-package all-the-icons-dired
  :defines maple-icon
  :if (and (display-graphic-p) maple-icon)
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom
  (all-the-icons-dired-monochrome nil))

(use-package image-dired
  :ensure nil
  :commands (image-dired)
  :custom
  (image-dired-dir (maple-cache-file "image-dired"))
  (image-dired-thumbnail-storage 'standard)
  :keybind
  (:states normal :map image-dired-thumbnail-mode-map
           ("j" . image-dired-next-line)
           ("k" . image-dired-previous-line)
           ("l" . image-dired-forward-image)
           ("h" . image-dired-backward-image)
           ("q" . image-dired-kill-buffer-and-window)
           ("RET" . image-dired-display-thumbnail-original-image)))

(use-package image-mode
  :ensure nil
  :keybind
  (:states normal :map image-mode-map
           ("j" . image-next-file)
           ("k" . image-previous-file)
           ("n" . image-next-file)
           ("p" . image-previous-file)
           ("q" . quit-window)))

(use-package recentf
  :ensure nil
  :hook (maple-init . recentf-mode)
  :custom
  (recentf-save-file (maple-cache-file "recentf"))
  (recentf-max-saved-items 100)
  (recentf-auto-cleanup 'never)
  (recentf-filename-handlers '(abbreviate-file-name))
  (recentf-exclude (list "\\.\\(png$\\|jpg$\\|pdf$\\)\\'" "COMMIT_EDITMSG\\'"
                         ;; (expand-file-name package-user-dir)
                         (expand-file-name maple-cache-directory))))

(use-package savehist
  :ensure nil
  :hook (maple-init . savehist-mode)
  :custom
  (savehist-file (maple-cache-file "savehist"))
  (savehist-autosave-interval nil) ; save on kill only
  (savehist-additional-variables '(kill-ring
                                   search-ring
                                   regexp-search-ring)))

(use-package saveplace
  :ensure nil
  :hook (maple-init . save-place-mode)
  :custom
  (save-place-file (maple-cache-file "places")))

(use-package neotree
  :commands (neotree)
  :custom
  (neo-create-file-auto-open t)
  (neo-banner-message nil)
  (neo-show-updir-line t)
  (neo-mode-line-type 'default)
  (neo-cwd-line-style 'button)
  (neo-smart-open t)
  (neo-show-hidden-files nil)
  (neo-auto-indent-point t)
  (neo-vc-integration '(face))
  :config
  (maple-evil-map neotree-mode-map)
  :keybind (([f2] . neotree-toggle)
            :map neotree-mode-map
            ("C" . neotree-copy-node)
            ("D" . neotree-delete-node)
            ("R" . neotree-rename-node)
            ("+" . neotree-create-node)
            ("^" . neotree-select-up-node)))

(use-package undo-tree
  :hook (maple-init . global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-auto-save-history nil)
  (undo-tree-history-directory-alist (list (cons "." (maple-cache-file "undo-tree" t))))
  :diminish undo-tree-mode)

(provide 'init-file)
;;; init-file.el ends here
