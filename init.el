(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; I want this really early on so you don't see the startup message
;; flash on the screen before this suppresses it
(setq inhibit-startup-message t)

;; This bootstraps us if we don't have anything
(when (not package-archive-contents)
  (package-refresh-contents))

;; org-mode always needs to be installed in an emacs where it isn't loaded.
;;
;; note that otfrom's original version installs org-plus-contrib from
;; http://orgmode.org/elpa/ -- I don't know quite why yet so sticking
;; with this for the moment
(when (not (package-installed-p 'org))
  (package-install 'org))
(require 'org)

;; load the main config file
(org-babel-load-file (concat user-emacs-directory "org/config.org"))

;; import local settings (deprecated, use /local/secrets.el.gpg instead)
(load (concat user-emacs-directory "init-local.el") 'noerror)
