;;; init-backend.el --- Initialize backend language configurations.	-*- lexical-binding: t -*-

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
;; Backend language configurations.
;;
;; Golang configurations.
;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/golang/lint/golint
;; go get -u github.com/haya14busa/gopkgs/cmd/gopkgs
;; go install golang.org/x/tools/gopls@latest
;; go install honnef.co/go/tools/cmd/staticcheck@latest
;;

;;; Code:
(use-package elisp-mode
  :ensure nil
  :dependencies
  (package-lint-flymake
   :if (eq maple-syntax-checker 'flymake)
   :hook (emacs-lisp-mode . package-lint-flymake-setup)))

(use-package python
  :ensure nil
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil)
  (python-shell-completion-native-enable nil)
  (:language python-mode
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

(use-package cc-mode
  :custom
  (c-default-style "linux")
  (c-basic-offset 4))

(use-package lua-mode
  :custom
  (lua-indent-level 4)
  (lua-indent-string-contents t))

(use-package dart-mode)
(use-package rust-mode)
(use-package swift-mode)
(use-package kotlin-mode)

(use-package protobuf-mode
  :dependencies
  (flymake-buf-lint
   :if (eq maple-syntax-checker 'flymake)
   :quelpa (:fetcher github :repo "honmaple/buf-lint-checker")
   :hook (protobuf-mode . flymake-buf-lint-setup))
  (flycheck-buf-lint
   :if (eq maple-syntax-checker 'flycheck)
   :quelpa (:fetcher github :repo "honmaple/buf-lint-checker")
   :hook (protobuf-mode . flycheck-buf-lint-setup)))

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
  (sql-input-ring-file-name (maple-cache-file "sql.history"))
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
