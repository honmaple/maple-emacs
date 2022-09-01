;;; init-lsp.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

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
;; Lsp configurations.
;;

;;; Code:
;; (use-package eglot
;;   :hook ((python-mode go-mode yaml-mode) . eglot-ensure))

(use-package lsp-mode
  :diminish "LSP"
  :hook ((python-mode go-mode yaml-mode) . lsp-deferred)
  :custom
  (lsp-restart 'auto-restart)
  (lsp-auto-guess-root t)
  (lsp-signature-auto-activate nil)
  (lsp-keep-workspace-alive nil)
  (lsp-enable-snippet nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-session-file (maple-cache-file "lsp-session-v1"))
  :config
  (defun maple/lsp-project-root(func session file-name)
    (let ((result (funcall func session file-name)))
      (if result (or (lsp-find-session-folder session file-name) result)
        (lsp--find-root-interactively session))))

  (advice-add 'lsp--calculate-root :around 'maple/lsp-project-root)

  (defun maple/lsp-restart-workspace(&rest _)
    (call-interactively 'lsp-restart-workspace))

  (defun maple/lsp--init-if-visible(func &rest args)
    (unless (bound-and-true-p git-timemachine-mode) (apply func args)))

  (advice-add 'lsp--init-if-visible :around 'maple/lsp--init-if-visible)

  (defun maple/lsp-shutdown-all()
    (interactive)
    (dolist (workspace (lsp--session-workspaces (lsp-session)))
      (let ((lsp--cur-workspace workspace))
        (lsp--shutdown-workspace))))

  (defun maple/lsp-shutdown()
    (interactive)
    (let* ((ws (mapcar (lambda(w) (cons (format "%s --> %s"
                                                (lsp--workspace-root w)
                                                (lsp--workspace-print w))
                                        w))
                       (lsp--session-workspaces (lsp-session))))
           (lsp--cur-workspace (cdr (assoc (completing-read "Shutdown workspace: " ws) ws))))
      (lsp--shutdown-workspace)))

  (use-package lsp-diagnostics
    :ensure nil
    :custom
    (lsp-diagnostics-provider :none))

  ;; pip install python-language-server
  (use-package lsp-pyls
    :ensure nil
    :custom
    (lsp-pyls-configuration-sources ["flake8"])
    ;; synax check
    (lsp-pyls-plugins-flake8-enabled t)
    (lsp-pyls-plugins-mccabe-enabled nil)
    (lsp-pyls-plugins-pyflakes-enabled nil)
    (lsp-pyls-plugins-pycodestyle-enabled nil)
    ;; format
    (lsp-pyls-plugins-autopep8-enabled nil)
    (lsp-pyls-plugins-yapf-enabled t)
    (lsp-pyls-disable-warning t)
    :config
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
    :ensure nil
    :custom
    (lsp-go-codelenses nil))

  ;; npm install -g yaml-language-server
  (use-package lsp-yaml
    :ensure nil
    :config
    (add-to-list 'lsp--formatting-indent-alist '(yaml-mode . tab-width)))

  (use-package lsp-rust
    :ensure nil
    :custom
    (lsp-rust-server 'rls)
    (lsp-rust-library-directories
     (list (expand-file-name "registry/src" (or (getenv "CARGO_HOME") "~/.cargo"))
           (expand-file-name "toolchains" (or (getenv "RUSTUP_HOME") "~/.rustup")))))

  ;; npm install -g vls
  ;; npm install -g typescript-language-server
  ;; npm install -g typescript
  (use-package lsp-javascript
    :ensure nil
    :custom
    (lsp-vetur-ignore-project-warning t))

  :language
  ((yaml-mode python-mode js-mode)
   :format 'lsp-format-buffer)
  (lsp-mode
   :definition 'lsp-find-definition
   :complete   '(company-capf :with company-yasnippet)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-doc-position 'top)
  (lsp-ui-imenu-enable nil)
  (lsp-ui-sideline-enable nil)
  :config
  (defun maple/lsp-ui-doc-format(func string symbol)
    (funcall func (replace-regexp-in-string "^\s*\n" "" string) symbol))

  (advice-add 'lsp-ui-doc--render-buffer :around 'maple/lsp-ui-doc-format)
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  :keybind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)))

(provide 'init-lsp)
;;; init-lsp.el ends here
