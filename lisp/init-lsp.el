;;; init-lsp.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

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
;; Lsp configurations.
;;

;;; Code:

(use-package lsp-mode
  :diminish lsp-mode
  :hook ((python-mode go-mode) . lsp)
  :config
  (setq lsp-prefer-flymake :none
        lsp-restart 'ignore
        lsp-enable-snippet nil
        lsp-enable-symbol-highlighting nil
        lsp-session-file (concat maple-cache-directory "lsp-session-v1"))

  (defun maple/lsp-message(func &rest args)
    (if (car args)
        (let ((str (apply 'format-message args)))
          (unless (string-match "\\(no identifier found\\|no object for ident\\)" str)
            (funcall func str)))
      (apply func args)))

  (advice-add 'message :around 'maple/lsp-message)

  (setq lsp-auto-guess-root t)

  (defun maple/lsp--calculate-root(session file-name)
    (ignore file-name)
    (lsp--find-root-interactively session))
  (advice-add 'lsp--calculate-root :after-until 'maple/lsp--calculate-root)

  ;; pip install python-language-server
  (use-package lsp-pyls
    :ensure nil
    :init
    (setq lsp-pyls-plugins-pycodestyle-enabled nil
          lsp-pyls-plugins-pyflakes-enabled nil
          lsp-pyls-configuration-sources ["flake8"]
          lsp-clients-python-library-directories '("/usr/" "~/repo/python/lib/python3.7/"))

    (with-eval-after-load 'pyvenv
      (add-hook 'pyvenv-post-activate-hooks 'lsp-restart-workspace)
      (add-hook 'pyvenv-post-deactivate-hooks 'lsp-restart-workspace)))

  ;; go get -u github.com/sourcegraph/go-langserver
  ;; go get -u golang.org/x/tools/cmd/gopls
  (use-package lsp-go
    :ensure nil)

  (use-package company-lsp)
  :custom
  (:language
   "lsp-mode"
   :complete '(company-lsp :with company-yasnippet)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-imenu-enable nil
        lsp-ui-sideline-enable nil)
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)))

(provide 'init-lsp)
;;; init-lsp.el ends here
