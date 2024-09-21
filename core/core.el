;;; core.el --- Initialize core configurations.	-*- lexical-binding: t -*-

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
;; core configurations.
;;

;;; Code:
(defvar maple-icon (display-graphic-p)
  "Show icon.")

(defvar maple-user user-full-name
  "Emacs user name.")

(defvar maple-mail user-mail-address
  "Emacs mail address.")

(defvar maple-theme 'monokai
  "Emacs theme.")

(defvar maple-python 'python3
  "Use python2 or python3.")

(defvar maple-lsp 'eglot
  "Language server.")

(defvar maple-syntax-checker 'flymake
  "Syntax checker.")

(defvar maple-package-archive 'tuna
  "Elpa package mirror.")

(defvar maple-init-hook nil
  "Custom init hook.")

(defvar maple-theme-hook nil
  "Custom theme hook.")

(defvar maple-cache-directory
  (expand-file-name "cache/" user-emacs-directory)
  "Storage area for cache files.")

(defconst maple-system-is-mac
  (eq system-type 'darwin))

(defconst maple-system-is-macport
  (and (eq system-type 'darwin)
       (boundp 'mac-carbon-version-string)))

(defconst maple-system-is-linux
  (eq system-type 'gnu/linux))

(defconst maple-system-is-windows
  (eq system-type 'windows-nt))

(defconst maple-system-is-wsl
  (and (eq system-type 'gnu/linux)
       (string-match "-[Mm]icrosoft" operating-system-release)))

(defvar user-handler-alist file-name-handler-alist)

(defadvice load-theme (after run-maple-theme-hook activate)
  "Run `maple-theme-hook'."
  (run-hooks 'maple-theme-hook))

(defun maple-cache-file(file &optional isdir)
  "Get cache FILE name with ISDIR."
  (let ((dir (expand-file-name file maple-cache-directory)))
    (when (and isdir (not (file-exists-p dir))) (make-directory dir t))
    dir))

(defun maple-require (path &rest pkgs)
  "Load PKGS from PATH."
  (mapc (lambda(pkg) (require `,pkg (format "%s/%s.el" (expand-file-name path user-emacs-directory) pkg))) pkgs))

(defun maple-finish()
  "Restore defalut values after init."
  (setq file-name-handler-alist user-handler-alist
        gc-cons-threshold (* 8 1024 1024)
        gc-cons-percentage 0.1))

(defun maple-initialize ()
  "Load core config file for Emacs."
  (setq user-full-name maple-user
        user-mail-address maple-mail
        gc-cons-threshold (* 256 1024 1024)
        gc-cons-percentage 0.6
        file-name-handler-alist nil)

  (when (and maple-system-is-wsl (display-graphic-p))
    (set-face-attribute 'default nil :height 140))

  (require 'core-autoload)
  (require 'core-general)
  (require 'core-package)
  (require 'core-use-package)

  (with-no-warnings
    (maple-add-hook 'emacs-startup-hook
      (run-with-idle-timer 0.1 nil (lambda() (run-hooks 'maple-init-hook))))

    (maple-add-hook 'emacs-startup-hook
      'maple-finish)

    (maple-add-hook 'after-init-hook
      (load-theme maple-theme t))

    (maple-add-hook 'find-file-hook
      'maple-file/check-large)

    (maple-add-hook 'find-file-not-found-functions
      'maple-file/mkdir-maybe)

    (maple-add-path (expand-file-name "site-lisp" user-emacs-directory))))

(add-to-list 'load-path (file-name-directory load-file-name))

(provide 'core)
;;; core.el ends here
