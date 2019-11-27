;;; maple-org.el ---  custom org configuration.	-*- lexical-binding: t -*-

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
;; custom org configuration.
;;

;;; Code:
(defgroup maple-org nil
  "Org configuration."
  :group 'maple)

(defcustom maple-org:root-path (expand-file-name "org/" user-emacs-directory)
  "Org src path."
  :type 'string
  :group 'maple-org)

(defcustom maple-org:src-path (expand-file-name "org/src/" user-emacs-directory)
  "Org src path."
  :type 'string
  :group 'maple-org)

(defcustom maple-org:img-path (expand-file-name "org/images/" user-emacs-directory)
  "Org img path."
  :type 'string
  :group 'maple-org)

(defcustom maple-org:capture-templates
  `(("t" "待办"
     entry (file+headline ,(expand-file-name "gtd.org" maple-org:root-path) "待办事项")
     "* TODO [#B] %?      :%^{Where|@Office|@Home|@Lunchtime|@School}:\n  %^T\n%i"
     :empty-lines 1)
    ("w" "工作"
     entry (file+headline ,(expand-file-name "project.org" maple-org:root-path) "工作安排")
     "* TODO [#A] %?      :Project:%^{Where|@Office|@Home|@Lunchtime}:\n  %i\n %U"
     :empty-lines 1)
    ("n" "笔记")
    ("na" "笔记"
     entry (file+headline ,(expand-file-name "note.org" maple-org:root-path) "笔记")
     "*  %?\n  %i\n %U"
     :empty-lines 1)
    ("nm" "电影"
     entry (file+headline ,(expand-file-name "note.org" maple-org:root-path) "影视歌曲")
     "*  %?               :%^{看了什么|Movie|Song}:\n Watched on %T\n %i\n"
     :empty-lines 1)
    ("nr" "阅读"
     entry (file+headline ,(expand-file-name "note.org" maple-org:root-path) "阅读")
     "*  %?               :Book:\n  %T\n %i\n"
     :empty-lines 1)
    ("b" "博客"
     entry (file+headline ,(expand-file-name "blog.org" maple-org:root-path) "博客")
     "** TODO [#B] %?     :blog:\n  %i %U"
     :empty-lines 1)
    ("c" "账单"
     table-line (file+headline ,(expand-file-name "mine.org" maple-org:root-path) "账单")
     "| %^{用途|吃饭|购买衣服|出行} | %U | %? | |")
    ("s" "代码片段")
    ("j" "日程安排"
     entry (file+headline ,(expand-file-name "gtd.org" maple-org:root-path) "日程安排")
     "* TODO [#B] %?      :%^{去哪儿|上海|南京|常州|昆明}:Journal:\n %^U\n"
     :empty-lines 1)
    ("z" "总结"
     entry (file+datetree ,(expand-file-name "summary.org" maple-org:root-path) "总结")
     "* %?                :%^{周期|Yearly|Monthly|Weekly|Daily}:Summary:"
     :empty-lines 1))
  "Org capture templates."
  :type '(list)
  :group 'maple-org)

(defun maple-org:capture-target ()
  "Set point for capturing at what capture target file+headline with headline set to %l would do."
  (org-capture-put :target (list
                            'file+headline
                            (nth 1 (org-capture-get :target))
                            (completing-read "Followed by: " (plist-get org-capture-plist :tags))))
  (org-capture-put-target-region-and-position)
  (widen)
  (let ((hd (nth 2 (org-capture-get :target))))
    (goto-char (point-min))
    (if (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote hd))
         nil t)
        (goto-char (point-at-bol))
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "* " hd "\n")
      (beginning-of-line 0))))

(defun maple-org:capture-snip (keybind src &optional tags)
  "Dynamic capture with KEYBIND SRC optional TAGS."
  (add-to-list 'org-capture-templates
               `(,keybind ,src entry (file+function
                                      ,(expand-file-name (format "%s笔记.org" src) maple-org:src-path)
                                      maple-org:capture-target)
                          ,(concat "** %?\t\n#+BEGIN_SRC " src "\n\n#+END_SRC") :tags ,tags)))

(defun maple-org:insert-img-link (prefix imagename)
  "Insert link to current buffer with PREFIX and IMAGENAME."
  (if (equal (file-name-extension (buffer-file-name)) "org")
      (insert (format "[[%s]]" prefix))
    (insert (format "![%s](%s)" imagename prefix))))

(defun maple-org:capture-screenshot (basename)
  "Screenshot and insert link to current buffer with BASENAME."
  (interactive "sScreenshot name: ")
  (let ((blog-image-path (expand-file-name
                          (format "%s-%s.png" basename (format-time-string "%Y%m%d_%H%M%S")) maple-org:img-path)))
    (if (file-exists-p blog-image-path)
        (message "the path '%s' already exists!" blog-image-path)
      (shell-command
       (format "scrot -s %s" blog-image-path))
      (maple-org:insert-img-link blog-image-path basename)))
  (insert "\n"))

(with-eval-after-load 'org-capture
  (setq org-capture-templates maple-org:capture-templates
        org-refile-targets
        `((,(expand-file-name "gtd.org" maple-org:root-path) :level . 1)
          (,(expand-file-name "summary.org" maple-org:root-path) :level . 4)))

  (maple-org:capture-snip "sp" "python" '("Tool" "Flask" "Tornado"))
  (maple-org:capture-snip "sl" "lua" '("Tool" "Nginx"))
  (maple-org:capture-snip "sg" "golang" '("Tool")))

(with-eval-after-load 'org-agenda
  (setq org-agenda-files (list maple-org:root-path)
        org-default-notes-file (expand-file-name "gtd.org" maple-org:root-path)))

(provide 'maple-org)
;;; maple-org.el ends here
