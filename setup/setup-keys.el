(require 'bind-key)
(load "buffer-defuns")

(bind-key "C-x r q" 'save-buffers-kill-terminal)
(global-unset-key (kbd "C-x C-c"))

;; Completion
(bind-key "C-." 'hippie-expand-no-case-fold)
(bind-key "C-:" 'hippie-expand-lines)
(bind-key "C-," 'completion-at-point)

(bind-key "C-w" 'backward-kill-word)
(bind-key "C-x C-k" 'kill-region)

;; Repplace rectangle-text with inline-string-rectangle
(bind-key "C-x r t" 'inline-string-rectangle)

(bind-key "M-i" 'back-to-indentation)

(bind-key "C-<f10>" 'menu-bar-mode)


(define-key key-translation-map [?\C-h] [?\C-?])
(bind-key "<f1>" 'help-command)

(bind-key "M-h" 'kill-region-or-backward-word)

(bind-key "M-w" 'save-region-or-current-line)

(bind-key "C-x C-i" 'ido-imenu)

(bind-key "C-x 3" 'split-window-right-and-move-there-damnit)

(bind-key "<f1> a" 'apropos)

(bind-key "C-c C-e" 'eval-and-replace)

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(bind-key "M-&" 'query-replace-regexp)
(bind-key "C-c b" 'create-scratch-buffer)
(bind-key "C-c d" 'duplicate-current-line-or-region)

(bind-key "<C-S-down>" 'move-text-down)
(bind-key "<C-S-up>" 'move-text-up)

(bind-key "C-S-y" 'yank-unindented)

(bind-key "C-x C-r" 'rename-current-buffer-file)
(bind-key "C-x C-k" 'delete-current-buffer-file)

(bind-key "C-c o" 'occur)
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

(defun back-to-indentation-or-beginning () (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))
(bind-key "C-a" 'back-to-indentation-or-beginning)

(global-set-key (kbd "C-x +") 'text-scale-increase)
(global-set-key (kbd "C-x -") 'text-scale-decrease)

(provide 'setup-keys)
