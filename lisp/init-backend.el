;;; init-backend.el --- Initialize backend language configurations.	-*- lexical-binding: t -*-

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
;; Backend language configurations.
;;
;; Golang configurations.
;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/golang/lint/golint
;; go get -u github.com/haya14busa/gopkgs/cmd/gopkgs
;; GO111MODULE=on go get golang.org/x/tools/gopls@latest
;;

;;; Code:
(use-package python
  :ensure nil
  :config
  (defun maple/run-python ()
    (interactive)
    (or (python-shell-get-process) (call-interactively 'run-python))
    (if (use-region-p)
        (python-shell-send-region (region-beginning) (region-end) t)
      (python-shell-send-buffer t)))
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil)
  (python-shell-completion-native-enable nil)
  (:language python-mode
             :run 'maple/run-python
             :checker '(:disable python-pylint))
  :dependencies
  (pyenv-mode
   :commands (pyenv-mode-set))
  (py-isort
   :commands (py-isort-buffer))
  (pip-requirements))

(use-package go-mode
  :custom
  (gofmt-show-errors nil)
  (:language go-mode
             :format 'gofmt
             :checker '(go-build go-test go-vet))
  :dependencies
  (go-rename)
  (go-add-tags))

(use-package rust-mode
  :dependencies
  (flycheck-rust
   :hook (rust-mode . flycheck-rust-setup)))

(use-package cc-mode
  :custom
  (c-default-style "linux")
  (c-basic-offset 4)
  (:language c-mode :complete 'company-c-headers)
  :dependencies
  (company-c-headers))

(use-package lua-mode
  :diminish lua-mode
  :custom
  (lua-indent-level 4)
  (lua-indent-string-contents t)
  (:language lua-mode :complete 'company-lua)
  :dependencies
  (company-lua))

(use-package dart-mode
  :diminish)

(use-package sh-mode
  :ensure nil
  :custom
  (:language sh-mode :checker '(sh-posix-bash)))

(use-package sql
  :ensure nil
  :hook
  (sql-interactive-mode . maple-truncate-lines)
  (sql-interactive-mode . maple-process-exit)
  :custom
  (sql-input-ring-file-name
   (expand-file-name "sql.history" user-emacs-directory))
  (sql-postgres-login-params
   '((user :default "postgres")
     (database :default "postgres")
     (server :default "localhost")
     (port :default 5432)))
  :dependencies
  (sql-indent
   :hook (sql-mode-hook . sqlind-minor-mode)))

(provide 'init-backend)
;;; init-backend.el ends here
