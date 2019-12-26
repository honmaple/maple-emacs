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

(use-package lsp-mode
  :diminish lsp-mode
  :hook ((python-mode go-mode yaml-mode) . lsp)
  :config
  (setq lsp-restart 'ignore
        lsp-auto-guess-root t
        lsp-prefer-flymake :none
        lsp-signature-auto-activate nil
        lsp-enable-snippet nil
        lsp-enable-symbol-highlighting nil
        lsp-session-file (expand-file-name "lsp-session-v1" maple-cache-directory))

  (defun maple/lsp-project-root(func session file-name)
    (let ((result (funcall func session file-name)))
      (if result (or (lsp-find-session-folder session file-name) result)
        (lsp--find-root-interactively session))))

  (advice-add 'lsp--calculate-root :around 'maple/lsp-project-root)

  (defun maple/lsp-restart-workspace(&rest _)
    (call-interactively 'lsp-restart-workspace))

  ;; pip install python-language-server
  (use-package lsp-pyls
    :ensure nil
    :config
    (setq lsp-pyls-plugins-pycodestyle-enabled nil
          lsp-pyls-plugins-pyflakes-enabled nil
          lsp-pyls-configuration-sources ["flake8"]
          lsp-clients-python-library-directories '("/usr/" "~/repo/python/lib/python3.7/"))

    (with-eval-after-load 'pyvenv
      (add-hook 'pyvenv-post-activate-hooks 'maple/lsp-restart-workspace)
      (add-hook 'pyvenv-post-deactivate-hooks 'maple/lsp-restart-workspace))
    (with-eval-after-load 'pyenv-mode
      (advice-add 'pyenv-mode-set :after 'maple/lsp-restart-workspace)
      (advice-add 'pyenv-mode-unset :after 'maple/lsp-restart-workspace)))

  ;; go get -u github.com/sourcegraph/go-langserver
  ;; go get golang.org/x/tools/cmd/gopls
  (use-package lsp-go
    :ensure nil)

  ;; npm install -g yaml-language-server
  (use-package lsp-yaml
    :ensure nil
    :custom
    (:language
     "yaml-mode"
     :format 'lsp-format-buffer))

  (use-package company-lsp
    :config
    (setq company-lsp-cache-candidates 'auto))

  :custom
  (:language
   "lsp-mode"
   :definition 'lsp-find-definition
   :complete   '(company-lsp :with company-yasnippet)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-border (face-foreground 'default)
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
