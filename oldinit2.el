;;; init.el --- Summary
;;; commentary:
;;; code:

(add-to-list 'load-path "~/proyectos/myemacs/fonsiconfig/")

(require 'misc)
(require 'bootstrap)
(require 'garbagecollection)

(use-package naysayer-theme :config (load-theme 'naysayer t))
(add-to-list 'default-frame-alist '(font . "Liberation Mono-16"))
(set-face-attribute 'default t :font "Liberation Mono-16")

(require 'fhelm)
(require 'fevil)

(use-package projectile
  :init (projectile-mode 1)
  :config
  (setq projectile-project-search-path '("~/proyectos"))
  (setq projectile-completion-system 'ido))

(use-package magit
  :config (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(require 'feglot)

(use-package editorconfig :init (editorconfig-mode 1))
(use-package smartparens :init (smartparens-global-mode 1) (require 'smartparens-config))

(use-package dired-hide-dotfiles)
(use-package dired-single)
(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-alGhvF --group-directories-first")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (evil-define-key 'normal dired-mode-map "c" 'ido-find-file)
  (evil-define-key 'normal dired-mode-map "h" 'dired-single-up-directory)
  (evil-define-key 'normal dired-mode-map "l" 'dired-single-buffer))

(use-package zoom)

(require 'customfuncs)
(require 'fkeymaps)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(which-key zoom dired-single dired-hide-dotfiles smartparens editorconfig eglot magit general orderless evil-collection evil corfu ido-completing-read+ flx-ido naysayer-theme gcmh use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
