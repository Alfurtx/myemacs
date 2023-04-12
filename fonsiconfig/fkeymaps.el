;;; fkeymaps.el --- Summary
;;; commentary:
;;; code:

(use-package which-key :init (which-key-setup-minibuffer) (which-key-mode))

(evil-define-key '(normal visual) 'global (kbd "<leader>pc") 'projectile-compile-project)
(evil-define-key '(normal visual) 'global (kbd "<leader>pp") 'projectile-switch-project)
(evil-define-key '(normal visual) 'global (kbd "<leader>pa") 'projectile-add-known-project)
(evil-define-key '(normal visual) 'global (kbd "<leader>pd") 'projectile-remove-known-project)
(evil-define-key '(normal visual) 'global (kbd "<leader>pf") 'find-private-files)

(evil-define-key '(normal visual) 'global (kbd "<leader>bd") 'kill-current-buffer)
(evil-define-key '(normal visual) 'global (kbd "<leader>bb") 'ido-switch-buffer)
(evil-define-key '(normal visual) 'global (kbd "<leader>be") 'eval-buffer)

(evil-define-key '(normal visual) 'global (kbd "<leader>g") 'magit)

(evil-define-key '(normal visual) 'global (kbd "<leader>m") 'call-last-kbd-macro)
(evil-define-key '(normal visual) 'global (kbd "<leader>k") 'compile)
(evil-define-key '(normal visual) 'global (kbd "<leader>.") 'dired-jump)
(evil-define-key '(normal visual) 'global (kbd "<leader>q") 'save-buffers-kill-emacs)
(evil-define-key '(normal visual) 'global (kbd "<leader>:") 'execute-extended-command)
(evil-define-key '(normal visual) 'global (kbd "<leader>SPC") 'ido-find-file)
(evil-define-key '(normal visual) 'global (kbd "<leader>f") 'ido-switch-buffer)

(evil-define-key '(normal visual) 'global (kbd "<leader>wh") 'move-to-left-window-or-vsplit)
(evil-define-key '(normal visual) 'global (kbd "<leader>wl") 'move-to-right-window-or-vsplit)
(evil-define-key '(normal visual) 'global (kbd "<leader>wj") 'move-to-bottom-window-or-hsplit)
(evil-define-key '(normal visual) 'global (kbd "<leader>wk") 'move-to-up-window-or-hsplit)
(evil-define-key '(normal visual) 'global (kbd "<leader>wd") 'delete-window)

(evil-define-key '(normal visual) 'global (kbd "<leader>lr") 'eglot-rename)

(provide 'fkeymaps)

;;; fkeymaps.el ends here
