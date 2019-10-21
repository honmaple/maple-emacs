;;; init-lua.el --- Initialize lua configurations.	-*- lexical-binding: t -*-

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
;; Lua configurations.
;;

;;; Code:

(use-package lua-mode
  :diminish lua-mode
  :config
  (setq lua-indent-level 4
        lua-indent-string-contents t)

  (use-package company-lua)
  :custom
  (:language
   "lua-mode"
   :complete 'company-lua))

(provide 'init-lua)
;;; init-lua.el ends here
