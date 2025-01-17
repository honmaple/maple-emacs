;;; init-lsp.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2024 lin.jiang

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
(pcase maple-lsp
  ('eglot
   ;; eglot 补全速度太慢了
   (use-package eglot
     :hook ((python-mode go-mode dart-mode js-mode web-mode vue-mode typescript-mode) . eglot-ensure)
     :custom
     (read-process-output-max (* 256 1024)) ;; 改善性能
     (eglot-autoshutdown t)
     (eglot-events-buffer-size 0)
     (eglot-send-changes-idle-time 0.2)
     (eglot-connect-timeout 10)
     (eglot-stay-out-of '(eldoc))
     (eglot-report-progress nil)
     (eglot-ignored-server-capabilities
      '(:inlayHintProvider :hoverProvider :documentHighlightProvider))
     (eglot-workspace-configuration
      (maple-ht '(("pyls.plugins.flake8.enabled" t)
                  ("pyls.plugins.pyflakes.enabled" :json-false)
                  ("pyls.plugins.pycodestyle.enabled" :json-false)
                  ("pyls.plugins.maccabe.enabled" :json-false)
                  ("pyls.plugins.yapf.enabled" t)
                  ("gopls.staticcheck" t))))
     :config
     (defvar maple/eglot-init-hook nil)

     (defvar maple/eglot-ignored-modes
       '(magit-blob-mode))

     (defvar maple/eglot-ignored-major-modes nil)

     (maple-add-hook 'go-mode-hook
       (setq-local eglot-stay-out-of '(eldoc imenu)))

     (maple-add-hook 'maple/eglot-init-hook
       (pcase major-mode
         ('go-mode
          (setq-local eglot-stay-out-of '(eldoc imenu)))))

     ;; 无法使用around eglot-ensure, magit-blob-mode总是为nil
     ;; 可以使用(memq this-command '(magit-blob-previous magit-blob-next))判断
     (defun eglot-ensure@override ()
       (let ((buffer (current-buffer)))
         (cl-labels
             ((maybe-connect
                ()
                (eglot--when-live-buffer buffer
                  (remove-hook 'post-command-hook #'maybe-connect t)
                  (unless (or eglot--managed-mode
                              (memq major-mode maple/eglot-ignored-major-modes)
                              (cl-loop for mode in maple/eglot-ignored-modes
                                       if (and (boundp mode) (symbol-value mode))
                                       return t))
                    (run-hooks 'maple/eglot-init-hook)
                    (apply #'eglot--connect (eglot--guess-contact))))))
           (when buffer-file-name
             (add-hook 'post-command-hook #'maybe-connect 'append t)))))

     (advice-add 'eglot-ensure :override 'eglot-ensure@override)

     (defun eglot-volar-find (package &optional global)
       (let ((path (string-trim-right
                    (shell-command-to-string
                     (format "npm list %s --parseable %s | head -n1" (if global "-g" "") package)))))
         (if (or global (not (string= path ""))) path
           (eglot-volar-find package t))))

     (add-to-list 'eglot-server-programs
                  `((web-mode :language-id "html") . ,(eglot-alternatives '(("vscode-html-language-server" "--stdio")
                                                                            ("vscode-css-language-server" "--stdio")
                                                                            ("typescript-language-server" "--stdio")))))
     (add-to-list 'eglot-server-programs
                  `(vue-mode . ("vue-language-server" "--stdio"
                                :initializationOptions
                                ,(maple-ht
                                  `(("vue.hybridMode" :json-false)
                                    ("typescript.tsdk" ,(expand-file-name "lib" (eglot-volar-find "typescript"))))))))

     :language
     ((python-mode go-mode dart-mode js-mode web-mode vue-mode typescript-mode)
      :format 'eglot-format)
     (eglot-managed-mode
      :complete '(:buster eglot-completion-at-point))
     :keybind
     (:prefix "," :states normal :map (dart-mode-map go-mode-map)
              ("ra" . eglot-code-actions)
              ("rx" . eglot-code-action-quickfix)
              ("rI" . eglot-code-action-organize-imports)))

   (use-package eglot-booster
     :quelpa (:fetcher github :repo "jdtsmith/eglot-booster")
     :hook (maple/eglot-init . (lambda() (when (executable-find "emacs-lsp-booster") (eglot-booster-mode))))
     :custom
     (eglot-booster-no-remote-boost t)))

  ('lsp-mode
   (use-package lsp-mode
     :diminish "LSP"
     :hook ((python-mode go-mode dart-mode js-mode web-mode vue-mode typescript-mode) . lsp-deferred)
     :custom
     (read-process-output-max (* 256 1024)) ;; 改善lsp-dart性能
     (lsp-restart 'auto-restart)
     (lsp-auto-guess-root t)
     (lsp-signature-auto-activate nil)
     (lsp-keep-workspace-alive nil)
     (lsp-enable-snippet nil)
     (lsp-enable-file-watchers nil)
     (lsp-enable-symbol-highlighting nil)
     (lsp-completion-provider :none)
     (lsp-diagnostics-provider :flymake)
     (lsp-headerline-breadcrumb-enable nil)
     (lsp-modeline-code-actions-enable nil)
     (lsp-modeline-diagnostics-enable nil)
     (lsp-session-file (maple-cache-file "lsp-session-v1"))
     :config
     (defun maple/lsp-restart-workspace(&rest _)
       (call-interactively 'lsp-restart-workspace))

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
       :keybind (:prefix "," :states normal :map go-mode-map ("rI" . lsp-organize-imports)))

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

     (use-package lsp-dart
       :custom
       (lsp-dart-flutter-fringe-colors nil)
       (lsp-dart-flutter-widget-guides nil))

     ;; npm install -g vls
     ;; npm install -g typescript-language-server
     ;; npm install -g typescript
     (use-package lsp-javascript
       :ensure nil
       :custom
       (lsp-vetur-ignore-project-warning t)
       (lsp-clients-typescript-log-verbosity "off"))

     :language
     ((python-mode js-mode)
      :format 'lsp-format-buffer)
     (lsp-mode
      :definition 'lsp-find-definition))

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
     (lsp-ui-doc-background ((t (:background unspecified))))
     :keybind
     (:map lsp-ui-mode-map
           ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
           ([remap xref-find-references] . lsp-ui-peek-find-references)))))

(provide 'init-lsp)
;;; init-lsp.el ends here
