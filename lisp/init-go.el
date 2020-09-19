;;; init-gui.el --- Initialize golang configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2020 lin.jiang

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
;; Golang configurations.
;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/golang/lint/golint
;; go get -u github.com/haya14busa/gopkgs/cmd/gopkgs
;;

;;; Code:

(use-package go-mode
  :config
  (setq gofmt-show-errors nil)
  :custom
  (:language
   "go-mode"
   :format 'gofmt))

(use-package go-rename)

(use-package go-add-tags)

(provide 'init-go)
;;; init-go.el ends here
