;;;; Variables

(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil
      visual-line-fringe-indicators '(nil right-curly-arrow)
      make-backup-files nil
      auto-save-default nil
      initial-scratch-message ""
      gc-cons-threshold 20000000 ; see https://github.com/lewang/flx
      )

(setq-default indent-tabs-mode nil
	      tab-width 8
	      )

(set-face-attribute 'default nil :height 120)

;;;; Modes

(electric-pair-mode t)
;; (electric-indent-mode t)
(electric-layout-mode t)

;; (setq next-line-add-newlines t)

(transient-mark-mode 1)
(delete-selection-mode 1)
(show-paren-mode 1)
;; (global-linum-mode 1)

(column-number-mode 1)
;; (global-hl-line-mode 1)
(size-indication-mode t)
(blink-cursor-mode 0)

;;;; Hooks

(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'setup-general)
