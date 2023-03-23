;;; init.el --- Summary
;;; commentary:
;;; code:

(load-file "~/proyectos/myemacs/fonsiconfig/misc.el")

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(setq package-user-dir "~/proyectos/myemacs/packages")

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(load-file "~/proyectos/myemacs/fonsiconfig/garbagecollection.el")

; (add-hook 'prog-mode-hook #'flymake-mode)

(use-package naysayer-theme
  :config
  (load-theme 'naysayer t)
  (set-face-attribute 'default nil :height 200 :font "LiberationMono")
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "#d1b897")
  (add-to-list 'default-frame-alist '(font . "LiberationMono-20")))

(load-file "~/proyectos/myemacs/fonsiconfig/ivy.el")

(use-package which-key
  :init
  (which-key-setup-minibuffer)
  (setq which-key-separator " → " )
  (which-key-mode))

(use-package flx)

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)))

(use-package all-the-icons)

(load-file "~/proyectos/myemacs/fonsiconfig/evil.el")

(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/proyectos")))

(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package golden-ratio
  :init
  (golden-ratio-mode 1)
  :config
  (setq golden-ratio-auto-scale t))

(use-package counsel-projectile
  :config (counsel-projectile-mode +1))

;;; Language Server Protocol
; (load-file "~/proyectos/myemacs/fonsiconfig/lsp.el")
; (use-package eglot
;   :config
;   (add-hook 'prog-mode-hook #'eglot-ensure))
; (use-package consult-eglot)

;;; Code completion framework
(use-package company
  :init
  (setq company-format-margin-function 'company-text-icons-margin)
  :config
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
  (define-key company-search-map (kbd "C-j") 'company-select-next)
  (define-key company-search-map (kbd "C-k") 'company-select-previous)
  (global-company-mode))
(use-package company-flx
  :after company
  :init
  (company-flx-mode +1))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package smartparens
  :config
  (smartparens-global-mode 1)
  (require 'smartparens-config))

(use-package dired-single)
(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-alGhvF --group-directories-first")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))


;;; CUSTOM WINDOW MANAGEMENT FUNCTIONS + OTHERS
(load-file "~/proyectos/myemacs/fonsiconfig/customfuncs.el")

;;; WHICH KEY KEYMAPS
(load-file "~/proyectos/myemacs/fonsiconfig/whichkeymaps.el")



(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("6ae64ffd3a4043be42c6d4e4e25c0498b1b2d7c4608c4f1dd908667f95a18bb4" "583277bd24a057630e73b5d72cd78d06f520a0accde5d9d8746d1f9598f38fd8" default))
 '(eglot-ignored-server-capabilities
   '(:hoverProvider :documentHighlightProvider :codeActionProvider :codeLensProvider :documentRangeFormattingProvider :documentOnTypeFormattingProvider :documentLinkProvider :colorProvider :foldingRangeProvider :executeCommandProvider))
 '(package-selected-packages
   '(dired-hide-dotfiles dired dired-single consult-eglot eglot company-flx naysayer-theme consult-lsp company-fuzzy company clang-format all-the-icons-ivy flx flycheck doom-modeline magit smartparens editorconfig lsp-ivy lsp-mode counsel-projectile which-key use-package smex ranger projectile ivy-rich ivy-posframe golden-ratio general gcmh evil-tutor evil-collection counsel auto-package-update all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-posframe-border ((t (:background "#ffffff")))))
