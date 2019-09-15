;; (defcustom maple-package-default
;;   '(:fetcher github :user "honmaple" :location "~/.emacs.d/site-lisp/")
;;   "Display buffer action."
;;   :type 'list
;;   :group 'maple-line)


;; (defun maple-package-install()
;;   )

;; (defun maple-package-install-from-location(dir)
;;   )

;; (defun maple-package-install-from-github(url)
;;   )

;; (defun maple-package-recipe(args)
;;   (let* ((name (car args))
;;          (recipe (cdr args))
;;          (user (or (plist-get recipe :user)
;;                    (plist-get maple-package-default :user)))
;;          (repo (or (plist-get recipe :repo)
;;                    (plist-get maple-package-default :repo)))
;;          (fetcher (or (plist-get recipe :fetcher)
;;                       (plist-get maple-package-default :fetcher)))
;;          (location (or (plist-get recipe :location)
;;                        (plist-get maple-package-default :location)))
;;          url)
;;     (unless (and user (string-match "/" repo))
;;       (error "`%s' does not exist" repo))
;;     (when (not (string-match "/" repo))
;;       (setq repo (concat user "/" repo)))

;;     (setq url (format "https://github.com/%s.git" repo))
;;     ))

;; (maple-package-install
;;  '(maple-line
;;    :fetcher github
;;    :user "honmaple"
;;    :repo "emacs-maple-line"
;;    :files ("*.el")
;;    :location t))
