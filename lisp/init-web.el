;;; init-web.el --- Initialize web configurations.	-*- lexical-binding: t -*-

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
;; Web configurations.
;;

;;; Code:
(use-package web-mode
  :mode
  ("\\.vue\\'" . vue-mode)
  ("\\.\\(xml\\|html?\\)$" . web-mode)
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-closing t) ; enable auto close tag in text-mode
  (web-mode-enable-auto-indentation nil)
  (web-mode-enable-css-colorization nil)
  (web-mode-enable-current-element-highlight t)
  (web-mode-engines-alist '(("django" . "\\.\\(xml\\|vue\\|html?\\)$")))
  (web-mode-engines-auto-pairs '(("django" . (("{{ " . " }")
                                              ("{% " . " %")
                                              ("{%-" . " | %")
                                              ("{%=" . " | %")
                                              ("{{-" . " | }")
                                              ("{{{" . " | }}")
                                              ("{# " . " #")
                                              ("<% " . " %>")))))
  :config
  (define-derived-mode vue-mode web-mode "Vue")
  :language
  (web-mode
   :run  'browse-url-of-file
   :fold 'web-mode-fold-or-unfold))

(use-package css-mode
  :custom
  (css-indent-offset 4)
  :dependencies
  (less-css-mode)
  (sass-mode)
  (scss-mode))

(use-package js-mode
  :ensure nil
  :hook (js-mode . (lambda() (indent-tabs-mode -1)))
  :dependencies
  (npm-mode
   :hook (js-mode . npm-mode))
  (coffee-mode)
  (typescript-mode))

(use-package web-beautify
  :commands (web-beautify-html web-beautify-css web-beautify-js))

(use-package emmet-mode
  :diminish emmet-mode
  :hook ((html-mode web-mode) . emmet-mode))

(use-package mmm-mode
  ;; :hook (web-mode . mmm-mode)
  :custom
  (mmm-global-mode nil)
  (mmm-mode-ext-classes-alist nil)
  (mmm-submode-decoration-level 0)
  :config
  (define-derived-mode tailwindcss-mode css-mode "Tailwindcss")

  (with-no-warnings
    (mmm-add-group
     'html-tailwindcss
     '((tailwindcss
        :submode tailwindcss-mode
        :face mmm-code-submode-face
        :front "class=\"*"
        :back "\""
        :back-offset -1)))
    (mmm-add-mode-ext-class 'web-mode nil 'html-tailwindcss)))

(provide 'init-web)
;;; init-web.el ends here
