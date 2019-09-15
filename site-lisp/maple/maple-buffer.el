;;; maple-buffer.el --- Initialize buffer configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/dotfiles/tree/master/emacs.d

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
;; Buffer configurations.
;;

;;; Code:

(defun maple-buffer/switch-to-previous ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun maple-buffer/switch-to-scratch ()
  "Switch to the `*scratch*' buffer.  Create it first if needed."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun maple-buffer/copy-to-clipboard ()
  "Copy entire buffer to clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun maple-buffer/copy-clipboard ()
  "Copy clipboard and replace buffer."
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(defun maple-buffer/safe-revert ()
  "Prompt before reverting the file."
  (interactive)
  (revert-buffer nil nil))

(defun maple-buffer/safe-erase ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun maple-buffer/kill-others ()
  "Kill all other buffers."
  (interactive)
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? " (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (message "Buffers deleted!")))

(defun maple-buffer/reopen(buffer-name &optional restore)
  "Reopen BUFFER-NAME RESTORE."
  (if (get-buffer buffer-name)
      (let ((buffer-text (with-current-buffer buffer-name
                           (buffer-substring (point-min) (point-max)))))
        (kill-buffer buffer-name)
        (with-current-buffer (get-buffer-create buffer-name)
          (when restore (insert buffer-text))))
    (message (format "%s buffer is not exists" buffer-name))))

(provide 'maple-buffer)
;;; maple-buffer.el ends here
