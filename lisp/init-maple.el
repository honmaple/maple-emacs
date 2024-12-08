;;; init-maple.el --- Initialize maple configurations.	-*- lexical-binding: t -*-

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
;; maple configurations.
;;

;;; Code:
(use-package maple-package
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-package")
  :commands (maple-package-upgrade))

(use-package maple-header
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-header")
  :hook (maple-init . maple-header-mode))

(use-package maple-run
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-run")
  :commands (maple-run)
  :config
  (setq maple-run:auto-clear t))

(use-package maple-diff
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-diff")
  :hook (maple-init . global-maple-diff-mode)
  :config
  (define-fringe-bitmap 'maple-diff:added-fringe
    [24] nil nil '(center repeated))

  (define-fringe-bitmap 'maple-diff:deleted-fringe
    [24] nil nil '(center repeated))

  (define-fringe-bitmap 'maple-diff:changed-fringe
    [24] nil nil '(center repeated)))

(use-package maple-preview
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-preview" :files ("*.el" "index.html" "static"))
  :commands (maple-preview-mode)
  :dependencies
  (web-server)
  (websocket))

(use-package maple-translate
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-translate")
  :custom
  (maple-translate-sdcv-dicts
   '(("lazyworm-ec" . "stardict/stardict-lazyworm-ec-2.4.2")
     ("lazyworm-ce" . "stardict/stardict-lazyworm-ce-2.4.2")))
  (maple-translate-google-proxies
   '(("http" . "127.0.0.1:1086")
     ("https" . "127.0.0.1:1086")))
  :commands (maple-translate maple-translate+))

(use-package maple-note
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-note")
  :commands (maple-note)
  :config
  (setq maple-note-base-directory "~/Git/pelican/content/posts/"
        maple-note-draft-directory "~/Git/pelican/content/drafts/")
  (maple-evil-map maple-note-mode-map))

(use-package maple-line
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-line")
  :commands (maple-line-mode))

(use-package maple-line-hide
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-line" :files ("maple-line-hide.el"))
  :commands (maple-line-hide-mode))

(use-package maple-modeline
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-modeline" :files ("*.el"))
  :hook (maple-theme . maple-modeline-mode)
  :config
  (setq maple-modeline-message-p nil)
  (setq maple-modeline-icon (and (display-graphic-p) maple-icon))

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

  (advice-add 'load-theme :after #'maple/modeline-theme)
  :custom-face
  (mode-line ((t (:box nil))))
  (mode-line-inactive ((t (:box nil))))
  :dependencies
  (maple-xpm
   :quelpa (:fetcher github :repo "honmaple/emacs-maple-xpm" :files ("*.el"))
   :config
   (setq maple-xpm-height 20
         maple-xpm-style (if (display-graphic-p) 'wave 'default))))

(use-package maple-minibuffer
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-minibuffer")
  :commands (maple-minibuffer-mode)
  :config
  (setq maple-minibuffer:position-type 'window-center))

(use-package maple-iedit
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-iedit" :files ("*.el"))
  :commands (maple-iedit-match-all maple-iedit-match-next maple-iedit-match-previous)
  :custom-face
  (iedit-occurrence ((t (:background "chocolate" :foreground "#272822"))))
  :transient
  (maple/iedit
   ()
   [[("n" "next" maple-iedit-match-next :transient t)]
    [("p" "prev" maple-iedit-match-previous :transient t)]
    [("t" "skip and next" maple-iedit-skip-and-match-next :transient t)]
    [("T" "skip and previous" maple-iedit-skip-and-match-previous :transient t)]])
  :keybind
  (:states visual
           ("n" . maple/iedit)
           ("C-n" . maple-iedit-match-next)
           ("C-p" . maple-iedit-match-previous)
           ("C-t" . maple-iedit-skip-and-match-next))
  :dependencies (iedit))

(use-package maple-scratch
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-scratch")
  :hook (maple-theme . maple-scratch-init)
  :config
  (maple-evil-map maple-scratch-mode-map)
  (setq maple-scratch-source nil
        maple-scratch-items '(maple-scratch-banner
                              maple-scratch-navbar
                              maple-scratch-default
                              maple-scratch-startup)))

(use-package maple-echoarea
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-echoarea")
  :commands (maple-echoarea-mode))

(use-package maple-tabbar
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-tabbar")
  :commands (maple-tabbar-mode))

(use-package maple-explorer
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-explorer")
  :commands (maple-explorer-file maple-explorer-buffer maple-explorer-imenu maple-explorer-recentf maple-explorer-search)
  :config
  (when (and (display-graphic-p) maple-icon) (maple-explorer-icon-mode)))

(use-package maple-env
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-env")
  :hook (maple-init . maple-env-mode)
  :config
  (setq maple-env:path (substitute-in-file-name "$HOME/repo")
        maple-env:python-command (if (eq maple-python 'python3) "pip3" "pip")
        ;; https://github.com/davidhalter/jedi/issues/1423
        maple-env:python-packages
        '("flake8" "isort" "yapf" "python-language-server[all]" "jedi==0.15.2")
        maple-env:golang-packages
        '("github.com/nsf/gocode"
          "github.com/rogpeppe/godef"
          "github.com/golang/lint/golint"
          "github.com/haya14busa/gopkgs/cmd/gopkgs"
          "golang.org/x/tools/cmd/gopls"
          "github.com/cweill/gotests/...")
        maple-env:npm-packages
        '("js-beautify" "tern" "yaml-language-server")))

(provide 'init-maple)
;;; init-maple.el ends here
