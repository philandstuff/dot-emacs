(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq inhibit-startup-message t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

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

(set-default 'indicate-empty-lines t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; org mode shortcuts
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(require 'org)
(setq org-default-notes-file (concat org-directory "/todo.org"))
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; set up various mode hooks
;; *.pp -> puppet-mode
;; *.cljs -> clojure-mode
;; clojure-mode -> paredit
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.pp$" . puppet-mode))
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'pretty-lambdas)

;; scheme stuff
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'pretty-lambdas)

;; haskell stuff
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook #'pretty-arrows)

;; font-lock
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("\\(\\<lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))
(defun pretty-arrows ()
  (font-lock-add-keywords
   nil `(("\\(->\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    8594 ;; unicode RIGHT ARROW
                                    ))))
         ("\\(<-\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    8592 ;; unicode LEFT ARROW
                                    ))))
         ("\\(\\\\\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    955 ;; unicode GREEK SMALL LETTER LAMBDA
                                    )))))))


;; base load path
(defconst dotfiles-dir
  (file-name-directory
   (or (buffer-file-name) load-file-name))
  "Base path for customised Emacs configuration")

(add-to-list 'load-path dotfiles-dir)

;; twitter stuff

(setq twittering-initial-timeline-spec-string
      '(":home"
        ":replies"
        ))

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (ditaa . t)
   (python . t)
   (ruby . t)
   ))
(setq org-src-fontify-natively t)

;; org export options
(setq org-export-with-toc nil)

;; import local settings
(require 'init-local nil 'noerror)


(custom-set-variables
 '(geiser-racket-binary "~/racket/bin/racket")
 '(markdown-command "kramdown")
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(org-ditaa-jar-path "~/bin/ditaa.jar")
 '(rcirc-buffer-maximum-lines 2000))
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
