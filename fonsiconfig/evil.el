(use-package evil
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
  (define-key evil-visual-state-map (kbd "C-c") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "J") (kbd "10j"))
  (define-key evil-normal-state-map (kbd "K") (kbd "10k"))
  (define-key evil-normal-state-map (kbd "L") (kbd "10l"))
  (define-key evil-normal-state-map (kbd "H") (kbd "10h"))
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init '(magit dired)))
(use-package general
  :config
  (general-evil-setup 1))
