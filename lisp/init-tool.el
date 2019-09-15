;;; init-tool.el --- Initialize tool configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/dotfiles/tree/master/emacs.d

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
;; TOOL configurations.
;;

;;; Code:

(use-package docker-tramp)
(use-package dockerfile-mode)

(use-package esup
  :config
  (maple/evil-map esup-mode-map))

(use-package pangu-spacing
  :commands (pangu-spacing-space-current-buffer)
  :config
  (defun pangu-spacing-search-and-replace (match regexp)
    "Replace regexp with match in buffer."
    (let* ((p (use-region-p))
           (start (if p (region-beginning) (point-min)))
           (end (if p (region-end) (point-max))))
      (pangu-spacing-search-buffer regexp start end (replace-match match nil nil)))))

(use-package quickrun
  :hook (quickrun--mode . maple/truncate-lines)
  :config
  (maple/evil-map quickrun--mode-map))

(use-package youdao-dictionary
  :config
  (setq url-automatic-caching t
        youdao-dictionary-search-history-file
        (expand-file-name "youdao" maple-cache-directory)
        youdao-dictionary-use-chinese-word-segmentation t)
  (maple/evil-map youdao-dictionary-mode-map))

(use-package avy
  :commands (avy-pop-mark)
  :config
  (setq avy-all-windows 'all-frames
        avy-background t))

(use-package figlet)

(use-package 2048-game
  :config
  (maple/evil-map 2048-mode-map))

(provide 'init-tool)
;;; init-tool.el ends here
