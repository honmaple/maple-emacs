;;; init-lsp.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

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
;; Lsp configurations.
;;

;;; Code:
(use-package company-lsp
  :config
  (setq company-lsp-cache-candidates 'auto))

;; (use-package eglot
;;   :hook ((python-mode go-mode yaml-mode) . eglot-ensure))

(use-package lsp-mode
  :diminish lsp-mode
  :hook ((python-mode go-mode yaml-mode) . lsp-deferred)
  :config
  (setq lsp-restart 'ignore
        lsp-auto-guess-root t
        lsp-diagnostic-package :none
        lsp-signature-auto-activate nil
        lsp-enable-snippet nil
        lsp-enable-file-watchers nil
        lsp-enable-symbol-highlighting nil
        lsp-keep-workspace-alive nil
        lsp-session-file (expand-file-name "lsp-session-v1" maple-cache-directory))

  (defun maple/lsp-project-root(func session file-name)
    (let ((result (funcall func session file-name)))
      (if result (or (lsp-find-session-folder session file-name) result)
        (lsp--find-root-interactively session))))

  (advice-add 'lsp--calculate-root :around 'maple/lsp-project-root)

  (defun maple/lsp-restart-workspace(&rest _)
    (call-interactively 'lsp-restart-workspace))

  (defun maple/lsp-configure-complete(&rest _)
    (setq company-backends (cdr company-backends)))

  (advice-add 'lsp--auto-configure :after 'maple/lsp-configure-complete)

  (defun maple/lsp--init-if-visible(func &rest args)
    (unless (bound-and-true-p git-timemachine-mode) (apply func args)))

  (advice-add 'lsp--init-if-visible :around 'maple/lsp--init-if-visible)

  ;; pip install python-language-server
  (use-package lsp-pyls
    :ensure nil
    :config
    (setq lsp-pyls-configuration-sources ["flake8"]
          ;; synax check
          lsp-pyls-plugins-flake8-enabled t
          lsp-pyls-plugins-mccabe-enabled nil
          lsp-pyls-plugins-pylint-enabled nil
          lsp-pyls-plugins-pyflakes-enabled nil
          lsp-pyls-plugins-pycodestyle-enabled nil
          ;; format
          lsp-pyls-plugins-autopep8-enabled nil
          lsp-pyls-plugins-yapf-enabled t)

    (defun lsp-pyls-get-pyenv-environment()
      (if lsp-pyls-plugins-jedi-environment
          lsp-pyls-plugins-jedi-environment
        (let ((version (getenv "PYENV_VERSION")))
          (when (and version (not (string= version "system")))
            (concat (replace-regexp-in-string
                     "\n" "" (shell-command-to-string "pyenv root"))
                    "/versions/" version)))))

    (with-eval-after-load 'pyenv-mode
      (advice-add 'pyenv-mode-set :after 'maple/lsp-restart-workspace)
      (advice-add 'pyenv-mode-unset :after 'maple/lsp-restart-workspace)))

  ;; go get -u github.com/sourcegraph/go-langserver
  ;; go get golang.org/x/tools/cmd/gopls
  (use-package lsp-go
    :ensure nil)

  ;; npm install -g yaml-language-server
  (use-package lsp-yaml
    :ensure nil)

  :custom
  (:language
   ("yaml-mode" "python-mode")
   :format 'lsp-format-buffer)
  (:language
   "lsp-mode"
   :definition 'lsp-find-definition
   :complete   '(company-lsp :with company-yasnippet)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-border (face-foreground 'default)
        lsp-ui-doc-position 'top
        lsp-ui-imenu-enable nil
        lsp-ui-sideline-enable nil)

  (defun maple/lsp-ui-doc-format(func string symbol)
    (funcall func (replace-regexp-in-string "^\s*\n" "" string) symbol))

  (advice-add 'lsp-ui-doc--render-buffer :around 'maple/lsp-ui-doc-format)
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)))

(provide 'init-lsp)
;;; init-lsp.el ends here
