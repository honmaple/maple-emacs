;;; theme.el ---  theme configuration.	-*- lexical-binding: t -*-

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
;; theme configuration.
;;

;;; Code:
(declare-function cl-position 'cl-seq)
(declare-function powerline-reset 'powerline)

;; (defvar maple-theme/alist (custom-available-themes))

(defvar maple-theme/alist
  '(monokai
    dracula
    spacemacs-dark
    doom-molokai
    doom-monokai-classic
    doom-monokai-pro
    doom-dracula
    doom-gruvbox
    doom-one
    doom-horizon
    doom-peacock
    doom-vibrant))

(defun maple-theme/cycle (&optional backward)
  "Theme switch with BACKWARD."
  (let* ((themes (if backward (reverse maple-theme/alist) maple-theme/alist))
         (index  (cl-position (car custom-enabled-themes) themes))
         (theme  (nth (if (or (not index) (= index (- (length themes) 1))) 0 (+ index 1)) themes)))
    (mapc 'disable-theme custom-enabled-themes)
    (let ((progress-reporter
           (make-progress-reporter
            (format "Loading theme %s..." theme))))
      (load-theme theme t)
      (when (featurep 'powerline)
        (powerline-reset))
      (progress-reporter-done progress-reporter))))

;;;###autoload
(defun maple-theme/next()
  "Next theme."
  (interactive)
  (maple-theme/cycle))

;;;###autoload
(defun maple-theme/previous()
  "Previous theme."
  (interactive)
  (maple-theme/cycle t))

;;; theme.el ends here
