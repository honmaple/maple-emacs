;;; init-maple.el --- Initialize maple configurations.	-*- lexical-binding: t -*-

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
;; maple configurations.
;;

;;; Code:
(use-package maple-use-package
  :ensure nil :demand)

(use-package maple-search
  :ensure nil
  :hook (maple-init . maple-search-init))

(use-package maple-run
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-run")
  :commands (maple-run))

(use-package maple-preview
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-preview" :files ("*.el" "index.html" "static"))
  :commands (maple-preview-mode))

(use-package maple-note
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-note")
  :commands (maple-note)
  :config
  (setq maple-note-root-path "~/git/pelican"
        maple-note-draft-path "content/draft")
  (maple/evil-map maple-note-mode-map))

(use-package maple-imenu
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-imenu")
  :commands (maple-imenu)
  :config
  (maple/evil-map maple-imenu-mode-map)
  (setq maple-imenu-display-alist '((side . left) (slot . 1))))

(use-package maple-line
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-line")
  :commands (maple-line-mode))

(use-package maple-line-hide
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-line" :files ("maple-line-hide.el"))
  :commands (maple-line-hide-mode))

(use-package maple-xpm
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-xpm" :files ("*.el"))
  :config
  (setq maple-xpm-style (if (display-graphic-p) 'wave 'default)))

(use-package maple-modeline
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-modeline" :files ("*.el"))
  :hook (maple-theme . maple-modeline-init)
  :config
  (maple/add-hook 'neotree-mode-hook
    (setq-local maple-modeline-style 'sidebar))

  (maple/add-hook 'maple-imenu-mode-hook
    (setq-local maple-modeline-style 'sidebar))

  (when (and (display-graphic-p) *icon*)
    (require 'maple-modeline-icon))

  (defun maple-modeline-reset-face(color &optional frame)
    "Reset face when theme change with FRAME."
    (set-face-background 'maple-modeline-active1 color frame)
    (set-face-background 'maple-modeline-inactive1 color frame))

  (defun maple/modeline-theme(theme &rest args)
    (pcase theme
      ('doom-one
       (maple-modeline-reset-face (if (display-graphic-p) "#282c2f" "#444444")))
      ('doom-vibrant
       (maple-modeline-reset-face (if (display-graphic-p) "#242730" "#444444")))
      ('spacemacs-dark
       (maple-modeline-reset-face (if (display-graphic-p) "#5d4d7a" "#444444")))
      (_
       (maple-modeline-reset-face (if (display-graphic-p) "#35331D" "#333333")))))

  ;; (use-package powerline :demand)

  (if (not (featurep 'powerline))
      (advice-add 'load-theme :after #'maple/modeline-theme)
    (put 'maple-modeline-active0 'face-alias 'powerline-active0)
    (put 'maple-modeline-active1 'face-alias 'powerline-active1)
    (put 'maple-modeline-inactive0 'face-alias 'powerline-inactive0)
    (put 'maple-modeline-inactive1 'face-alias 'powerline-inactive1))
  :custom-face
  (mode-line ((t (:box nil))))
  (mode-line-inactive ((t (:box nil)))))

(use-package maple-minibuffer
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-minibuffer")
  :commands (maple-minibuffer-mode))

(use-package maple-iedit
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-iedit" :files ("*.el"))
  :commands (maple-iedit-match-all maple-iedit-match-next maple-iedit-match-previous)
  :hydra
  (maple/iedit
   ()
   ("n" maple-iedit-match-next "next")
   ("t" maple-iedit-skip-and-match-next "skip and next")
   ("T" maple-iedit-skip-and-match-previous "skip and previous")
   ("p" maple-iedit-match-previous "prev"))
  :custom-face
  (iedit-occurrence ((t (:background "chocolate" :foreground "#272822"))))
  :evil-bind (:state visual
                     ("n" . maple/iedit/body)
                     ("C-n" . maple-iedit-match-next)
                     ("C-p" . maple-iedit-match-previous)
                     ("C-t" . maple-iedit-skip-and-match-next)))

(use-package maple-scratch
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-scratch")
  :hook (window-setup . maple-scratch-mode)
  :config
  (maple/evil-map maple-scratch-mode-map)
  (setq maple-scratch-alist
        (append (butlast maple-scratch-alist)
                '(("Init"
                   :action 'maple-file/open-init
                   :desc "Open Init File")
                  ("Test"
                   :action 'maple-file/open-test
                   :desc "Open Test File"))
                (last maple-scratch-alist))
        maple-scratch-source nil))

(use-package maple-echoarea
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-echoarea")
  :commands (maple-echoarea-mode))

(use-package maple-tabbar
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-tabbar")
  :commands (maple-tabbar-mode))

(use-package maple-env
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-env")
  :hook (maple-init . maple-env-mode)
  :config
  (with-eval-after-load 'pyvenv
    (add-hook 'pyvenv-post-activate-hooks 'maple-env-mode-on)
    (add-hook 'pyvenv-post-deactivate-hooks 'maple-env-mode-on))

  (setq maple-env:path (substitute-in-file-name "$HOME/repo")
        maple-env:python-command (if *python3* "pip3" "pip")
        maple-env:python-packages
        '("flake8" "isort" "yapf" "python-language-server[all]")
        maple-env:golang-packages
        '("github.com/nsf/gocode"
          "github.com/rogpeppe/godef"
          "github.com/golang/lint/golint"
          "github.com/haya14busa/gopkgs/cmd/gopkgs"
          "golang.org/x/tools/cmd/gopls")
        maple-env:npm-packages
        '("js-beautify" "tern"))

  (use-package exec-path-from-shell
    :if maple-system-is-mac
    :init (exec-path-from-shell-initialize)))

(provide 'init-maple)
;;; init-maple.el ends here
