;;; fevil.el --- Summary
;;; commentary:
;;; code:

(use-package evil
  :custom
  (evil-want-keybinding nil)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  :init 
  (evil-mode 1)
  (use-package evil-collection :init (evil-collection-init '(magit dired)))
  (use-package general :init (general-evil-setup 1))
  :config
  (evil-define-key '(normal insert) 'global (kbd "C-c") 'evil-normal-state)
  (evil-define-key '(normal visual) 'global "K" (kbd "10k"))
  (evil-define-key 'normal 'global "J" (kbd "10j"))
  (evil-define-key 'normal 'global "L" (kbd "10l"))
  (evil-define-key 'normal 'global "H" (kbd "10h"))
  (evil-set-leader 'normal (kbd "SPC")))

(provide 'fevil)
;;; fevil.el ends here
