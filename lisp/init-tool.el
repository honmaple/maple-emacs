;;; init-tool.el --- Initialize tool configurations.	-*- lexical-binding: t -*-

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
;; TOOL configurations.
;;

;;; Code:

(use-package docker-tramp)

(use-package youdao-dictionary
  :custom
  (url-automatic-caching t)
  (youdao-dictionary-search-history-file (maple-cache-file "youdao"))
  (youdao-dictionary-use-chinese-word-segmentation t)
  :config
  (maple-evil-map youdao-dictionary-mode-map))

(use-package figlet)

(use-package esup
  :config
  (maple-evil-map esup-mode-map))

(use-package 2048-game
  :config
  (maple-evil-map 2048-mode-map))

(provide 'init-tool)
;;; init-tool.el ends here
