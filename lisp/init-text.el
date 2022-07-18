;;; init-text.el --- Initialize text configurations.	-*- lexical-binding: t -*-

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
;; TEXT configurations.
;;

;;; Code:
(use-package json-mode)
(use-package vimrc-mode)
(use-package nginx-mode)
(use-package protobuf-mode)
(use-package dockerfile-mode)

(use-package markdown-mode
  :config
  (use-package org-table
    :ensure nil
    :hook (markdown-mode . orgtbl-mode))

  (defun cleanup-org-tables ()
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "-+-" nil t) (replace-match "-|-"))))

  (maple-add-hook 'markdown-mode-hook
    (add-hook 'after-save-hook 'cleanup-org-tables  nil 'make-it-local))

  :language
  (markdown-mode :run 'markdown-toggle-markup-hiding)
  :dependencies
  (markdown-toc))

(use-package yaml-mode
  :hook (yaml-mode . (lambda() (setq tab-width 2) (origami-mode 1)))
  :language
  (yaml-mode :fold 'origami-toggle-node)
  :dependencies
  (origami :diminish origami-mode))

(use-package writeroom-mode
  :custom
  (writeroom-mode-line t)
  (writeroom-bottom-divider-width 0))

(provide 'init-text)
;;; init-text.el ends here
