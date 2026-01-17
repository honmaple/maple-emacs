;;; init-maple.el --- Initialize maple configurations.	-*- lexical-binding: t -*-

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
  :custom
  (maple-run:auto-clear t))

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
  :commands (maple-translate maple-translate+ maple-translate-offline)
  :custom
  (maple-translate-sdcv-dicts
   '(("lazyworm-ec" . "stardict/stardict-lazyworm-ec-2.4.2")
     ("lazyworm-ce" . "stardict/stardict-lazyworm-ce-2.4.2")))
  (maple-translate-google-proxies
   '(("http" . "127.0.0.1:1086")
     ("https" . "127.0.0.1:1086")))
  (maple-translate-display-alist
   (when (display-graphic-p)
     '((maple-translate . maple-translate-show-in-posframe)
       (maple-translate-offline . maple-translate-show-in-posframe)))))

(use-package maple-note
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-note")
  :commands (maple-note)
  :custom
  (maple-note-basedir "~/ssd/Git/pelican/content/"))

(use-package maple-line
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-line")
  :commands (maple-line-mode))

(use-package maple-line-hide
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-line" :files ("maple-line-hide.el"))
  :commands (maple-line-hide-mode))

(use-package maple-modeline
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-modeline" :files ("*.el"))
  :hook (maple-load-theme . maple-modeline-mode)
  :custom
  (maple-modeline-icon maple-icon)
  (maple-modeline-height 22)
  (maple-modeline-direction '(right . left))
  (maple-modeline-separator (if (display-graphic-p) 'arrow 'icon-arrow))
  :config
  (defun maple/modeline-reset-face(theme &rest args)
    ;; (set-face-background 'maple-modeline-active0 (face-attribute 'default :background nil t))
    (set-face-background 'maple-modeline-active1 (color-lighten-name (face-attribute 'mode-line :background nil t) 25))
    (set-face-background 'maple-modeline-inactive1 (color-lighten-name (face-attribute 'mode-line-inactive :background nil t) 25)))

  (advice-add 'load-theme :after #'maple/modeline-reset-face)
  :custom-face
  (mode-line ((t (:box nil))))
  (mode-line-inactive ((t (:box nil)))))

(use-package maple-minibuffer
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-minibuffer")
  :commands (maple-minibuffer-mode)
  :custom
  (maple-minibuffer:position-type 'window-center))

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
  :hook (maple-load-theme . maple-scratch-init)
  :config
  (maple-evil-map maple-scratch-mode-map)
  :custom
  (maple-scratch-source nil)
  (maple-scratch-items '(maple-scratch-banner
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
  (when maple-icon (maple-explorer-icon-mode)))

(use-package maple-env
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-env")
  :hook (maple-init . maple-env-mode)
  :custom
  (maple-env:path (substitute-in-file-name "$HOME/repo"))
  (maple-env:python-command (if (eq maple-python 'python3) "pip3" "pip"))
  ;; https://github.com/davidhalter/jedi/issues/1423
  (maple-env:python-packages
   '("flake8" "isort" "yapf" "python-language-server[all]" "jedi==0.15.2"))
  (maple-env:golang-packages
   '("github.com/nsf/gocode"
     "github.com/rogpeppe/godef"
     "github.com/golang/lint/golint"
     "github.com/haya14busa/gopkgs/cmd/gopkgs"
     "golang.org/x/tools/cmd/gopls"
     "github.com/cweill/gotests/..."))
  (maple-env:npm-packages
   '("js-beautify" "tern" "yaml-language-server")))

(provide 'init-maple)
;;; init-maple.el ends here
