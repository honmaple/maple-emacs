;;; init-lsp.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

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
;; Lsp configurations.
;;

;;; Code:
(defvar maple-lsp-major-modes
  '(python-mode
    go-mode dart-mode
    html-mode css-mode scss-mode js-mode
    web-mode vue-mode typescript-mode))

(defvar maple-lsp-minor-modes
  '(:not magit-blob-mode))

(defvar maple-lsp-format-indent-alist
  '((web-mode . 2)
    (yaml-mode . 2)))

(defun maple-lsp-indent-width (mode)
  "Get indent width with MODE."
  (let ((value (or (alist-get mode maple-lsp-format-indent-alist)
                   (when-let ((parent-mode (get mode 'derived-mode-parent)))
                     (maple-lsp-indent-width parent-mode)))))
    (or (if (symbolp value) (symbol-value value) value) tab-width)))

(defmacro maple-lsp-with(&rest body)
  "Start lsp server and execute BODY within special major modes or minor modes."
  (declare (indent 0) (debug t))
  `(when (and (memq major-mode maple-lsp-major-modes)
              (cl-loop for mode in maple-lsp-minor-modes
                       if (and (not (keywordp mode)) (boundp mode) (symbol-value mode))
                       return (not (eq (car maple-lsp-minor-modes) :not))
                       finally return t))
     ,@body))

(pcase maple-lsp
  ('eglot
   (use-package eglot
     :hook (prog-mode . eglot-ensure)
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
      (lambda (server)
        (maple-ht
         (pcase (car (eglot--major-modes server))
           ('web-mode
            `(("emmet.showExpandedAbbreviation" "always")
              ("emmet.showAbbreviationSuggestions" t)
              ("emmet.showSuggestionsAsSnippets" t)
              ;; https://github.com/microsoft/vscode-html-languageservice/blob/main/src/htmlLanguageTypes.ts
              ("tailwindCSS.emmetCompletions" t)
              ("tailwindCSS.experimental.configFile" ,(expand-file-name "tailwind.config.js" (project-root (eglot--project server))))))
           ('go-mode
            '(("gopls.staticcheck" :json-false)))
           ('python-mode
            '(("pylsp.plugins.flake8.enabled" :json-false)
              ("pylsp.plugins.pyflakes.enabled" :json-false)
              ("pylsp.plugins.pycodestyle.enabled" :json-false)
              ("pylsp.plugins.maccabe.enabled" :json-false)
              ("pylsp.plugins.yapf.enabled" :json-false)
              ("pylsp.plugins.ruff.enabled" t)))))))
     :config
     (defvar maple/eglot-init-hook nil)

     (maple-add-hook 'go-mode-hook
       (setq-local eglot-stay-out-of '(eldoc imenu)))

     (maple-add-hook 'maple/eglot-init-hook
       (pcase major-mode
         ('go-mode
          (setq-local eglot-stay-out-of '(eldoc imenu)))))

     ;; 无法使用around eglot-ensure, magit-blob-mode总是为nil
     ;; 或者可以使用(memq this-command '(magit-blob-previous magit-blob-next))判断
     (defun eglot-ensure@override ()
       (let ((buffer (current-buffer)))
         (cl-labels
             ((maybe-connect
                ()
                (eglot--when-live-buffer buffer
                  (remove-hook 'post-command-hook #'maybe-connect t)
                  (unless eglot--managed-mode
                    (maple-lsp-with
                      (run-hooks 'maple/eglot-init-hook)
                      (apply #'eglot--connect (eglot--guess-contact)))))))
           (when buffer-file-name
             (add-hook 'post-command-hook #'maybe-connect 'append t)))))

     (advice-add 'eglot-ensure :override 'eglot-ensure@override)

     (defun eglot-format@around(oldfunc &rest args)
       (let ((tab-width (maple-lsp-indent-width major-mode)))
         (apply oldfunc args)))

     (advice-add 'eglot-format :around 'eglot-format@around)

     (defun eglot-volar-find (package &optional global)
       (let ((path (string-trim-right
                    (shell-command-to-string
                     (format "npm list %s --parseable %s | head -n1" (if global "-g" "") package)))))
         (if (or global (not (string= path ""))) path
           (eglot-volar-find package t))))

     (defun eglot-volar-options (server)
       (maple-ht
        `(("vue.hybridMode" :json-false)
          ("typescript.tsdk" ,(expand-file-name "lib" (eglot-volar-find "typescript"))))))

     (add-to-list 'eglot-server-programs
                  '((web-mode :language-id "html") . ("vscode-html-language-server" "--stdio")))

     (add-to-list 'eglot-server-programs
                  '(vue-mode . ("vue-language-server" "--stdio" :initializationOptions eglot-volar-options)))

     (define-derived-mode web-emmet-mode web-mode "Emmet"
       "Custom emmet mode."
       (add-to-list 'eglot-server-programs
                    '((web-emmet-mode :language-id "html") . ("emmet-ls" "--stdio"))))

     ;; https://github.com/tailwindlabs/tailwindcss-intellisense/blob/main/packages/tailwindcss-language-service/src/util/languages.ts
     (define-derived-mode tailwindcss-mode web-mode "TailwindCSS"
       "Custom tailwindcss mode."
       (add-to-list 'eglot-server-programs
                    '((tailwindcss-mode :language-id "html") . ("tailwindcss-language-server" "--stdio"))))

     (defvar eglot-lspx-command (executable-find "eglot-lspx"))

     (when eglot-lspx-command
       (add-to-list 'eglot-server-programs `((web-mode :language-id "html")
                                             . (,eglot-lspx-command
                                                "--" "vscode-html-language-server" "--stdio"
                                                "--" "tailwindcss-language-server" "--stdio")))
       (add-to-list 'eglot-server-programs `((vue-mode :language-id "html")
                                             . (,eglot-lspx-command
                                                "--" "vue-language-server" "--stdio"
                                                "--" "vscode-html-language-server" "--stdio"
                                                "--" "tailwindcss-language-server" "--stdio"
                                                :initializationOptions
                                                (lambda (server) (list :vue-language-server (eglot-volar-options server))))))
       (add-to-list 'eglot-server-programs `(((js-mode :language-id "javascript") (typescript-mode :language-id "typescript"))
                                             . (,eglot-lspx-command
                                                "--" "typescript-language-server" "--stdio"
                                                "--" "vscode-eslint-language-server" "--stdio"))))
     :language
     (eglot-managed-mode
      :rename 'eglot-rename
      :format 'eglot-format
      :complete '(:buster eglot-completion-at-point))
     :keybind
     (:prefix "," :states normal :map (dart-mode-map go-mode-map)
              ("ra" . eglot-code-actions)
              ("rx" . eglot-code-action-quickfix)
              ("rI" . eglot-code-action-organize-imports)))

   (use-package eglot-booster
     :quelpa (:fetcher github :repo "jdtsmith/eglot-booster")
     ;; :hook (maple/eglot-init . (lambda() (when (executable-find "emacs-lsp-booster") (eglot-booster-mode))))
     :custom
     (eglot-booster-no-remote-boost t)))
  ('lsp-bridge
   (use-package lsp-bridge
     :quelpa (:fetcher github
                       :repo "manateelazycat/lsp-bridge"
                       :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources"))
     :autoload (lsp-bridge-mode)
     :hook (prog-mode . (lambda()
                          (when (bound-and-true-p corfu-mode) (corfu-mode -1))
                          (lsp-bridge-mode)))
     :custom
     (acm-enable-yas nil)
     (lsp-bridge-python-command (expand-file-name "versions/lsp-bridge/bin/python3" (getenv "PYENV_ROOT")))
     (lsp-bridge-multi-lang-server-mode-list
      '(((web-mode) . "html_tailwindcss")))
     (lsp-bridge-multi-lang-server-extension-list nil)
     (lsp-bridge-completion-in-string-file-types '("vue" "dart" "html"))
     :keybind
     (:map acm-mode-map
           ("TAB" . acm-select-next)
           ([tab] . acm-select-next)
           ("S-TAB" . acm-select-prev)
           ([backtab] . acm-select-prev)))))

(provide 'init-lsp)
;;; init-lsp.el ends here
