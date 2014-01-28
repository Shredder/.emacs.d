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
;;                ~/.emacs.d/users/<user>/use-package-config.el
;; and customize.
;; Add other .el files in that directory to further setup emacs. These files
;; will be loaded (in alphabetic order) at the end of this emacs config.
(setq use-evil-p nil
      use-org-p t
      use-yasnippet-p nil
      use-auto-complete-p nil
      use-browse-kill-ring-p t
      use-multiple-cursors-p nil
      use-ace-jump-mode-p t
      use-expand-region-p nil
      use-ibuffer-vc-p t
      use-ibuffer-p t
      use-key-chord-p t
      use-ido-p t
      use-smex-p t
      use-desktop-p t
      use-undo-tree-p t
      use-magit-p nil
      use-back-button-p t
      use-guide-key-p t
      use-better-defaults-p t
      use-ido-anywhere-p t
      use-setup-keys-p nil
      use-zenburn-theme-p t
      )

;;;; Path setup

;; Packages administered through package.el and el-get automatically have
;; their load-path set.

;; Define directories

;; Setup directory for config files part of this config
;; ~/.emacs.d/setup
(setq setup-dir (concat user-emacs-directory "setup"))

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))

;; Set path to local code (not installed via package.el or el-get)
;; ~/.emacs.d/site-lisp/
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Settings for currently logged in user go in
;; ~/.emacs.d/users/<user> (sourced at the end of this file)
(setq user-settings-dir
      (concat user-emacs-directory "users/" user-login-name))

;; Directory for backup files
(setq backup-dir
      (expand-file-name (concat user-emacs-directory "backups")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq user-package-conf (concat user-settings-dir "/use-package-config.el"))

;; Create directories
(setq my-dirs (list site-lisp-dir
                    user-settings-dir
                    backup-dir))

;; Create directories if they don't exist
(dolist (d my-dirs)
  (unless (file-directory-p d)
    (mkdir d t)))

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
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; /Configuration of paths

;;;; Package system

;; Initialization of package.el
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
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
(setq my-packages (append
                   '(evil-surround
                     evil-exchange
                     evil-org
                     )
                   (mapcar 'el-get-source-name el-get-sources)))

(dolist (p my-packages)
  (when (not (el-get-package-exists-p p))
    (el-get-install p))
  (el-get-do-init p))
;;; /el-get

;;;; Actual package configuration follows

;; Needed for defuns/buffer-defuns.el included in setup/setup-keys.el
(use-package s
  :ensure s)

(if use-better-defaults-p
    (use-package better-defaults
      :ensure better-defaults
      ))

(if use-evil-p
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

(if use-guide-key-p
    (use-package guide-key
      :ensure guide-key
      :init (progn
              (setq guide-key/guide-key-sequence '("C-x" "C-c")
                    guide-key/recursive-key-sequence-flag t
                    guide-key/idle-delay 0.5
                    )
              (guide-key-mode 1)
              )))

(if use-back-button-p
    (use-package back-button
      :ensure back-button
      :init (progn
              (back-button-mode 1)
              )))

(if use-magit-p
    (use-package magit
      :ensure magit
      :init (progn
              (autoload 'magit-status "magit")
              )
      :bind (
             ("C-x m" . magit-status)
             )))

(if use-undo-tree-p
    (use-package undo-tree
      :ensure undo-tree
      :init (progn
              (global-undo-tree-mode 1)
              ))

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

(if use-smex-p
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

(if use-ido-p
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

(if use-ido-anywhere-p
    (use-package imenu-anywhere
      :ensure imenu-anywhere
      :bind (
             ("C-M-x C-M-i" . imenu-anywhere)
             )))

;; python-mode.el omitted

(if use-key-chord-p
    (use-package key-chord
      :ensure key-chord
      :init (progn
              ;; (key-chord-define-global "hj" 'undo)
              (setq key-chord-one-key-delay 0.3
                    key-chord-two-key-delay 0.3)
              (key-chord-mode 1)
              (if use-evil-p
                  (progn
                    (key-chord-define evil-normal-state-map "jk" 'evil-force-normal-state)
                    (key-chord-define evil-visual-state-map "jk" 'evil-change-to-previous-state)
                    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
                    (key-chord-define evil-replace-state-map "jk" 'evil-normal-state)))
              ;; Quick toggle of two most recent buffers
              (fset 'quick-switch-buffer [?\C-x ?b return])
              (key-chord-define-global ",." 'quick-switch-buffer)
              )))

(if use-ibuffer-p
    (use-package ibuffer
      :ensure ibuffer
      :init (progn
              (setq ibuffer-default-sorting-mode 'major-mode)
              )
      :bind (
             ("C-x C-b" . ibuffer-other-window)
             )))

(if use-ibuffer-vc-p
    (use-package ibuffer-vc
      :ensure ibuffer-vc
      :init (progn
              (add-hook 'ibuffer-hook
                        (lambda ()
                          (ibuffer-vc-set-filter-groups-by-vc-root)
                          (unless (eq ibuffer-sorting-mode 'alphabetic)
                            (ibuffer-do-sort-by-alphabetic))))
              )))

(if use-expand-region-p
    (use-package expand-region
      :ensure expand-region
      :bind (
             ("C-'" . er/expand-region)
             ("C-M-'" . er/contract-region)
             )))

(if use-ace-jump-mode-p
    (use-package ace-jump-mode
      :ensure ace-jump-mode
      :bind (
             ("C-c SPC" . ace-jump-mode)
             )))

(if use-multiple-cursors-p
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

(if use-yasnippet-p
    (use-package yasnippet
      :ensure yasnippet
      :init (progn
              (yas-global-mode 1)
              (setq yas/root-directory "~/.emacs.d/yasnippet-snippets")
              (yas/load-directory yas/root-directory)
              )))

(if use-browse-kill-ring-p
    (use-package browse-kill-ring
      :ensure browse-kill-ring
      :bind (
             ("C-x C-y" . browse-kill-ring)
             )))

(if use-org-p
    (use-package org
      :ensure org
      :init (progn
              (setq org-directory "~/org"
                    org-default-notes-file (concat org-directory "/notes.org")
                    org-log-done 'time
                    org-hide-leading-stars t
                    org-fast-tag-selection-single-key nil
                    org-export-htmlize-output-type 'css
                    org-completion-use-ido t
                    org-special-ctrl-a/e t
                    org-special-ctrl-k t
                    org-odd-levels-only t
                    org-log-into-drawer t
                    org-todo-keywords '(
                                        (sequence "TODO(t)" "STARTED(s)" "WAITING(w@/!)" "|" "DONE(d!)")
                                        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
                                        (sequence "|" "CANCELED(c@)")
                                        )
                    org-tag-alist '(
                                    ;; Nature
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
                                    (:endgroup . nil)
                                    )
                    )
              (use-package evil-org)
              )
      :bind (
             ("C-c l" . org-store-link)
             ("C-c a" . org-agenda)
             ("C-c c" . org-capture)
             ("C-c r" . org-remember)
             )))

(if use-zenburn-theme-p
    (if (display-graphic-p)
        (use-package zenburn-theme
          :ensure zenburn-theme
          )))

(if use-auto-complete-p
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

(if use-setup-keys-p
    (require 'setup-keys))

;; Conclude init by setting up specifics for the current user
(when (file-exists-p user-settings-dir)
  (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
