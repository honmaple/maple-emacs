;;; init-text.el --- Initialize text configurations.	-*- lexical-binding: t -*-

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
;; TEXT configurations.
;;

;;; Code:
(use-package json-mode)
(use-package vimrc-mode)
(use-package nginx-mode)
(use-package dockerfile-mode)

(use-package diff-mode
  :ensure nil
  :hook (diff-mode . (lambda() (setq indent-region-function 'ignore))))

(use-package valign
  :hook ((org-mode markdown) . valign-mode))

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
  :config
  (with-eval-after-load 'expand-region
    (defun maple/yaml-expand-region(func regex &optional next-indent-level)
      (funcall func regex 1000))

    (advice-add 'er/mark-yaml-block-base :around 'maple/yaml-expand-region))

  (defun maple/yaml-compute-indentation()
    (let ((ci (current-indentation))
          (cm (looking-at yaml-hash-key-re))
          (cl (looking-at "^\s*-\s+.*$")))
      (save-excursion
        (beginning-of-line)
        (if (looking-at yaml-document-delimiter-re) 0
          (forward-line -1)
          (while (and (looking-at yaml-blank-line-re)
                      (> (point) (point-min)))
            (forward-line -1))
          (let ((li (current-indentation))
                (ll (looking-at "^\s*-\s+.*$")))
            ;; 如果上一行的缩进大于当前行, 上一行是列表，当前行也是是列表 - 正常缩进
            ;; 如果上一行的缩进大于当前行, 上一行和当前行有一个不是列表 - 原有缩进
            ;; 如果上一行的缩进等于当前行, 上一行是字典，当前行是列表 - 正常缩进
            ;; 如果上一行的缩进等于当前行, 上一行是字典，当前行是不是列表 - 原有缩进
            ;; 如果上一行的缩进小于当前行, 上一行是字典 - 正常缩进
            ;; 如果上一行的缩进小于当前行, 上一行是列表，当前行也是列表 - 正常缩进
            ;; 如果上一行的缩进小于当前行, 上一行是列表，当前行是字典 - 错误/正常缩进
            (if (or (and (> li ci) (or (not cl) (not ll))) (and (= li ci) cm (not ll))) ci
              (+ li
                 (if (looking-at yaml-nested-map-re) yaml-indent-offset 0)
                 (if (looking-at yaml-nested-sequence-re) yaml-indent-offset 0)
                 (if (looking-at yaml-block-literal-re) yaml-indent-offset 0))))))))

  (defun maple/yaml-indent-region(func &rest args)
    (if (derived-mode-p 'yaml-mode)
        (letf (((symbol-function 'yaml-compute-indentation) 'maple/yaml-compute-indentation))
              (apply func args))
      (apply func args)))

  (advice-add 'indent-region :around 'maple/yaml-indent-region)
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
