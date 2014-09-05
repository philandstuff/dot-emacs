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

(defun maybe-install-and-require (p)
  (when (not (package-installed-p p))
    (package-install p))
  (require p))

;; This bootstraps us if we don't have anything
(when (not package-archive-contents)
  (package-refresh-contents))

;; org-mode always needs to be installed in an emacs where it isn't loaded.
;;
;; note that otfrom's original version installs org-plus-contrib from
;; http://orgmode.org/elpa/ -- I don't know quite why yet so sticking
;; with this for the moment
(maybe-install-and-require 'org)

(org-babel-load-file (concat user-emacs-directory "org/config.org"))

;; base load path
(defconst dotfiles-dir
  (file-name-directory
   (or (buffer-file-name) load-file-name))
  "Base path for customised Emacs configuration")

(add-to-list 'load-path dotfiles-dir)

;; import local settings (deprecated, use /local/secrets.el.gpg instead)
(require 'init-local nil 'noerror)
