;;; maple-theme.el ---  theme configuration.	-*- lexical-binding: t -*-

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
;; theme configuration.
;;

;;; Code:
(declare-function powerline-reset 'powerline)

;; (defvar maple-theme/alist
;;   (mapcar 'symbol-name (custom-available-themes)))
;; (defvar maple-theme/alist
;;   (delete "doom-one-light" (mapcar 'symbol-name (custom-available-themes))))

(defvar maple-theme/alist
  '(monokai
    spacemacs-dark
    solarized-light
    solarized-dark
    doom-molokai
    doom-one
    doom-horizon
    doom-peacock
    doom-vibrant))

(defun maple-theme/cycle (&optional backward)
  "Theme switch with BACKWARD."
  (let* ((themes (if backward (reverse maple-theme/alist) maple-theme/alist))
         (theme  (car custom-enabled-themes))
         (index  (cl-loop for i in themes and index from 0
                          when (eq i theme) return index))
         (ntheme (nth (if (= index (- (length themes) 1)) 0 (+ index 1)) themes)))
    (mapc 'disable-theme custom-enabled-themes)
    (let ((progress-reporter
           (make-progress-reporter
            (format "Loading theme %s..." ntheme))))
      (load-theme ntheme t)
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
