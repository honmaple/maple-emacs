;;; init-sql.el --- Initialize sql configurations.	-*- lexical-binding: t -*-

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
;; SQL configurations.
;;

;;; Code:

(use-package sql
  :ensure nil
  :config
  (setq sql-input-ring-file-name
        (expand-file-name "sql.history" user-emacs-directory)
        sql-postgres-login-params
        '((user :default "postgres")
          (database :default "postgres")
          (server :default "localhost")
          (port :default 5432)))
  (maple/add-hook 'sql-interactive-mode-hook
    '(maple/truncate-lines maple/process-exit))
  :evil-state (sql-interactive-mode . insert))

(use-package sql-indent
  :hook (sql-mode-hook .  sqlind-minor-mode))

(provide 'init-sql)
;;; init-sql.el ends here
