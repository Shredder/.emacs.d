;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; TODO
;; (if (fboundp 'scroll-bar-mode) (scoll-bar-mode -1))

;; Inhibit startup clutter
(setq inhibit-startup-message t
      inhibit-startup-screen t)

;; We use use-package with package.el backend to maintain external packages.
;; Packages which are not available via package.el are installed and added to
;; the load-path with el-get. Then they can be initialized with use-package.

;; Configure which modules to run
;; These should be reasonable defaults and can be overridden by the user.
;; Copy this block to
;;                ~/.emacs.d/users/<user>/user-package-config.el
;; and customize.
;; Add other .el files in that directory to further setup emacs. These files
;; will be loaded (in alphabetic order) at the end of this emacs config.
(setq default-package-settings-alist
      '((evil . nil)
        (org . t)
        (yasnippet . nil)
        (auto-complete . nil)
        (browse-kill-ring . t)
        (multiple-cursors . nil)
        (ace-jump-mode . t)
        (expand-region . nil)
        (ibuffer-vc . t)
        (ibuffer . t)
        (key-chord . t)
        (ido . t)
        (smex . t)
        (desktop . t)
        (undo-tree . t)
        (magit . nil)
        (back-button . t)
        (guide-key . t)
        (better-defaults . t)
        (ido-anywhere . t)
        (setup-keys . nil)
        (zenburn-theme . t)
        (python-mode.el nil)
        ))

(defun use-package-p (p)
  (assoc-default p default-package-settings-alist nil nil))
;;;; Path setup

;; Packages administered through package.el and el-get automatically have
;; their load-path set.

;; Define directories

;; Setup directory for config files part of this config
;; ~/.emacs.d/setup
(setq setup-dir
      (expand-file-name "setup" user-emacs-directory)) 

;; Functions (load all files in defuns-dir)
(setq defuns-dir
      (expand-file-name "defuns" user-emacs-directory))

;; Set path to local code (not installed via package.el or el-get)
;; ~/.emacs.d/site-lisp/
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Settings for currently logged in user go in
;; ~/.emacs.d/users/<user> (sourced at the end of this file)
(setq user-settings-dir
      (expand-file-name user-login-name
			(expand-file-name "users" user-emacs-directory)))

;; Directory for backup files
(setq backup-dir
      (expand-file-name
       (expand-file-name "backups" user-emacs-directory)))
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(setq user-package-config-file "user-package-config.el")
(setq user-package-conf
      (expand-file-name user-package-config-file user-settings-dir))

;; Create directories
(setq my-dirs (list site-lisp-dir
		    user-settings-dir
		    backup-dir))

;; Create directories if they don't exist
(dolist (d my-dirs)
  (unless (file-directory-p d)
    (mkdir d t)))

;; Create user package settings file if it doesn't exist
(unless (file-exists-p user-package-conf)
  (append-to-file
   (mapconcat (lambda (x)
		(format ";; (push '(%s . t) default-package-settings-alist) ;; default: %s" (car x) (cdr x)))
	      default-package-settings-alist "\n")
   nil
   user-package-conf))

;;;; Set up load path

;; el-get package management tool (see below)
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; ~/.emacs.d/setup directory
(add-to-list 'load-path setup-dir)

;; ~/.emacs.d/defuns directory
(add-to-list 'load-path defuns-dir)

;; Files in ~/.emacs.d/ (for setup-*.el files part of this config)
(add-to-list 'load-path user-emacs-directory)
;; Files in ~/.emacs.d/site-lisp/
(add-to-list 'load-path site-lisp-dir)
;; Files within directories in ~/.emacs.d/site-lisp/
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(add-to-list 'load-path user-settings-dir)
;; Load per-user package-config file to override package activation on top of this file.
(if (file-readable-p user-package-conf)
    (load-file user-package-conf))

;; Keep emacs Custom-settings in separate file
(if (file-exists-p custom-file)
    (load custom-file))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name backup-dir))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; /Configuration of paths

;;;; Package system

;; Initialization of package.el
(require 'package)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
                         ;; ("gnu" . "http://elpa.gnu.org/packages/")) ;; Currently offline
(package-initialize)
(package-refresh-contents)

                                        ;(if (null (require 'req-package "req-package" t))
                                        ;    (progn (package-install 'req-package)
                                        ;	   (require 'req-package)))
(if (null (require 'use-package "use-package" t))
    (progn (package-install 'use-package)
	   (require 'use-package)))

(require 'setup-general)

;;;; Initialization of el-get

;; Install el-get if not available
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(require 'el-get)
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; Packages which are not available through package.el
(setq my-packages (append '(evil
                            evil-surround
                            evil-exchange
                            evil-org
                            ;; python-mode-el
                            )
                          (mapcar 'el-get-source-name el-get-sources)))

;; Install
(dolist (p my-packages)
  (when (not (el-get-package-exists-p p))
    (el-get-do-install p)))

;; Initialize (set load-path)
(dolist (p my-packages)
  (el-get-do-init p))

;;; /el-get

;;;; Actual package configuration follows

(when (use-package-p 'python-mode.el)
  (add-to-list 'load-path
	       (expand-file-name "python-mode.el-current" site-lisp-dir)) 
  (setq py-install-directory
	(expand-file-name "python-mode.el-current" site-lisp-dir))
  ;; (setq py-install-directory (expand-file-name "el-get/python-mode-el" user-emacs-directory))
  (use-package python-mode
    :init (progn
            (use-package jedi
              :ensure jedi
              :init (progn
                      (setq jedi:setup-keys t)
                      (setq jedi:complete-on-dot t)))
            (add-hook 'python-mode-hook (lambda ()
                                          (jedi:setup)
                                          (define-key python-mode-map "\r" 'newline-and-indent)
                                          (define-key python-mode-map (kbd "M-p") 'python-nav-backward-block)
                                          (define-key python-mode-map (kbd "M-n") 'python-nav-forward-block)
                                          (define-key python-mode-map (kbd "M-a") 'python-nav-backward-statement)
                                          (define-key python-mode-map (kbd "M-e") 'python-nav-forward-statement)))
            )))

(use-package recentf)

;; Useful elisp libraries
;; Needed for defuns/buffer-defuns.el included in setup/setup-keys.el
(use-package s
  :ensure s)
(use-package dash
  :ensure dash)

(when (use-package-p 'better-defaults)
  (use-package better-defaults
    :ensure better-defaults
    ))

(when (use-package-p 'evil)
  (use-package evil
    :ensure evil
    :init (progn
            (evil-mode 1)
            (require 'cl)
            ;; Change modeline color with mode
            (lexical-let ((default-color (cons (face-background 'mode-line)
                                               (face-foreground 'mode-line))))
              (add-hook 'post-command-hook
                        (lambda ()
                          (let ((color (cond ((minibufferp) default-color)
                                             ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                                             ((evil-emacs-state-p) '("#444488" . "#ffffff"))
                                             ((buffer-modified-p) '("#006fa0" . "#ffffff"))
                                             (t default-color))))
                            (set-face-background 'mode-line (car color))
                            (set-face-foreground 'mode-line (cdr color))))))
            ;; evil-surround
            (use-package surround
              ;; Installed by el-get
              :init (progn
                      (global-surround-mode 1)
                      ))
            (use-package evil-matchit
              :ensure evil-matchit
              :init (progn
                      (global-evil-matchit-mode 1)
                      ))
            (use-package evil-exchange
              ;; Installed by el-get
              :init (progn
                      ;; (setq evil-exchange-key (kbd "z"))
                      (evil-exchange-install)
                      ))
            (use-package evil-nerd-commenter
              :ensure evil-nerd-commenter
              :init (progn
                      (evilnc-default-hotkeys)
                      ))
            (use-package evil-leader
              :ensure evil-leader
              :init (progn
                      (global-evil-leader-mode)
                      (evil-leader/set-leader ",")
                      (evil-leader/set-key
                        "e" 'find-file
                        "b" 'switch-to-buffer
                        "k" 'kill-buffer
                        )
                      ))
            )))

(when (use-package-p 'guide-key)
  (use-package guide-key
    :ensure guide-key
    :init (progn
            (setq guide-key/guide-key-sequence '("C-x" "C-c")
                  guide-key/recursive-key-sequence-flag t
                  guide-key/idle-delay 0.5
                  )
            (guide-key-mode 1)
            )))

(when (use-package-p 'back-button)
  (use-package back-button
    :ensure back-button
    :init (progn
            (back-button-mode 1)
            )))

(when (use-package-p 'magit)
  (use-package magit
    :ensure magit
    :init (progn
            (autoload 'magit-status "magit")
            )
    :bind (
           ("C-x m" . magit-status)
           )))

(when (use-package-p 'undo-tree)
  (use-package undo-tree
    :ensure undo-tree
    :init (progn
            (global-undo-tree-mode 1)
            )))

(when (use-package-p 'desktop)
  (use-package desktop
    :ensure desktop
    :init (progn
            (desktop-save-mode 1)
            (defun my-desktop-save ()
              (interactive)
              (if (eq (desktop-owner) (emacs-pid))
                  (desktop-save desktop-dirname)))
            (add-hook 'auto-save-hook 'my-desktop-save)
            )))

(when (use-package-p 'smex)
  (use-package smex
    :ensure smex
    :init (progn
            (smex-initialize)
            )
    :bind (
           ("M-x" . smex)
           ("C-x C-m" . smex)
           )))

                                        ; (use-package jump-char)
					; (use-package sane-defaults)

(when (use-package-p 'ido)
  (use-package ido
    :ensure ido
    :init (progn
            (ido-mode t)
            )
    :bind (
           ("C-x C-i" . ido-imenu)
           ("C-x M-f" . ido-find-file-other-window)
           ("C-x f" . recentf-ido-find-file)
           )))

(when (use-package-p 'ido-anywhere)
  (use-package imenu-anywhere
    :ensure imenu-anywhere
    :bind (
           ("C-M-x C-M-i" . imenu-anywhere)
           )))

;; python-mode.el omitted

(when (use-package-p 'key-chord)
  (use-package key-chord
    :ensure key-chord
    :init (progn
            ;; (key-chord-define-global "hj" 'undo)
            (setq key-chord-one-key-delay 0.3
                  key-chord-two-key-delay 0.3)
            (key-chord-mode 1)
            (if (use-package-p 'evil)
                (progn
                  (key-chord-define evil-normal-state-map "jk" 'evil-force-normal-state)
                  (key-chord-define evil-visual-state-map "jk" 'evil-change-to-previous-state)
                  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
                  (key-chord-define evil-replace-state-map "jk" 'evil-normal-state)))
            ;; Quick toggle of two most recent buffers
            (fset 'quick-switch-buffer [?\C-x ?b return])
            (key-chord-define-global ",." 'quick-switch-buffer)
            )))

(when (use-package-p 'ibuffer)
  (use-package ibuffer
    :ensure ibuffer
    :init (progn
            (setq ibuffer-default-sorting-mode 'major-mode)
            )
    :bind (
           ("C-x C-b" . ibuffer-other-window)
           )))

(when (use-package-p 'ibuffer-vc)
  (use-package ibuffer-vc
    :ensure ibuffer-vc
    :init (progn
            (add-hook 'ibuffer-hook
                      (lambda ()
                        (ibuffer-vc-set-filter-groups-by-vc-root)
                        (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic))))
            )))

(when (use-package-p 'expand-region)
  (use-package expand-region
    :ensure expand-region
    :bind (
           ("C-'" . er/expand-region)
           ("C-M-'" . er/contract-region)
           )))

(when (use-package-p 'ace-jump-mode)
  (use-package ace-jump-mode
    :ensure ace-jump-mode
    :bind (
           ("C-c SPC" . ace-jump-mode)
           )))

(when (use-package-p 'multiple-cursors)
  (use-package multiple-cursors
    :ensure multiple-cursors
    :bind (
           ("C-S-c C-S-c" . mc/edit-lines)
           ("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)
           ("C-S-c C-e" . mc-edit-ends-of-lines)
           ("C-S-c C-a" . mc-edit-beginnings-of-lines)
           )))

;; fastnav omitted

(when (use-package-p 'yasnippet)
  (use-package yasnippet
    :ensure yasnippet
    :init (progn
            (yas-global-mode 1)
            (setq yas/root-directory "~/.emacs.d/yasnippet-snippets")
            (unless (file-directory-p yas/root-directory)
              (mkdir yas/root-directory))
            (yas/load-directory yas/root-directory)
            )))

(when (use-package-p 'browse-kill-ring)
  (use-package browse-kill-ring
    :ensure browse-kill-ring
    :bind (
           ("C-x C-y" . browse-kill-ring)
           )))

(when (use-package-p 'org)
  (use-package org
    :ensure org
    :init (progn
            (setq org-directory (expand-file-name "~/org")
                  org-default-notes-file (expand-file-name "notes.org" org-directory)
                  org-log-done 'note ;; or 'time
                  org-hide-leading-stars t
                  org-fast-tag-selection-single-key nil
                  org-treat-insert-todo-heading-as-state-change t
                  org-treat-S-cursor-todo-selection-as-state-change nil
                  org-export-htmlize-output-type 'css
                  org-completion-use-ido t
                  org-refile-use-outline-path t
                  org-outline-path-complete-in-steps nil
                  org-special-ctrl-a/e t
                  org-special-ctrl-k t
                  org-odd-levels-only t
                  org-log-into-drawer t
                  org-enforce-todo-dependencies t
                  org-enforce-todo-checkbox-dependencies t
                  org-return-follows-link t
                  ;; (t) quick selection key
                  ;; general form: (<enter>/<exit>)
                  ;; @ log note
                  ;; ! log timestamp
                  ;; | separates "done" states
                  ;; Since org-log-done is 'note, the "done" states don't need explicit @.
                  org-todo-keywords '((sequence "TODO(t)" "STARTED(s@)" "WAITING(w@/!)" "|" "DONE(d)")
                                      (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
                                      (sequence "|" "CANCELED(c)"))
                  org-tag-alist '(;; Nature
                                  (:startgroup . nil)
                                  ("@work" . ?w)
                                  ("@private" . ?p)
                                  (:endgroup . nil)
                                  ;; Context
                                  (:startgroup . nil)
                                  ("@telephone" . ?t)
                                  ("@email" . ?e)
                                  ("@in_person" . ?i)
                                  ("@web" . ?n)
                                  (:endgroup . nil)
                                  ;; Action
                                  (:startgroup . nil)
                                  ("@money_xfer" . ?m)
                                  ("@buy" . ?b)
                                  ("@read" . ?r)
                                  ("@plan" . ?l)
                                  (:endgroup . nil))
                  ;; More stuff to configure:
                  ;; org-todo-state-tags-triggers
                  ;; org-capture-templates
                  ;; all refile stuff
                  )
            (defun org-summary-todo (n-done n-not-done)
              "Switch entry to DONE when all subentries are done, to TODO otherwise."
              (let (org-log-done org-log-states)   ; turn off logging
                (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
            (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
            (use-package evil-org))
    :bind (("C-c l" . org-store-link)
           ("C-c a" . org-agenda)
           ("C-c c" . org-capture)
           ("C-c r" . org-remember)
           )))

(when (use-package-p 'zenburn-theme)
  (if (display-graphic-p)
      (use-package zenburn-theme
        :ensure zenburn-theme
        )))

(when (use-package-p 'auto-complete)
  (use-package auto-complete
    :ensure auto-complete
    :init (progn
            (require 'auto-complete-config)
            (ac-config-default)
            )))

;; Ensure we run an Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

(when (use-package-p 'setup-keys)
  (require 'setup-keys))

;; Conclude init by setting up specifics for the current user
(when (file-exists-p user-settings-dir)
  (mapc 'load
	(--filter (not (string= it user-package-config-file))
		  (directory-files user-settings-dir nil "^[^#].*el$"))))
