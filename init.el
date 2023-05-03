;;; init.el --- Summary
;;; commentary:
;;; code:

;; MISCELLANEOUS SETTINGS
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -2)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq visible-bell 1)
(setq ring-bell-function 'ignore)
(setq-default truncate-lines t)
(setq make-backup-files nil)
(setq scroll-conservatively 101)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3)))
(setq mouse-wheel-progressive-speed t)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-margin 4)
(global-hl-line-mode 1)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(setq mode-line-format (delete 'mode-line-modes mode-line-format))
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)
(setq compilation-scroll-output t)
(setq gdb-many-windows t)
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

;; USE PACKAGE BOOTSTRAP
(require 'package)
(if (eq system-type 'gnu/linux) (setq package-user-dir "~/.config/emacs/packages"))
(if (eq system-type 'windows-nt) (setq package-user-dir "~/.emacs.d/packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; THEMES
(use-package naysayer-theme :config (load-theme 'naysayer t))
(add-to-list 'default-frame-alist '(font . "Liberation Mono-16"))
(set-face-attribute 'default t :font "Liberation Mono-16")


;; EVIL VIM
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
  (evil-define-key '(insert normal) 'global (kbd "C-j") nil)
  (evil-define-key '(insert normal) 'global (kbd "C-k") nil)
  (evil-define-key '(visual insert) 'global (kbd "C-c") 'evil-normal-state)
  (evil-define-key '(normal visual) 'global "K" (kbd "10k"))
  (evil-define-key 'normal 'global "J" (kbd "10j"))
  (evil-define-key 'normal 'global "L" (kbd "10l"))
  (evil-define-key 'normal 'global "H" (kbd "10h"))
  (evil-set-leader '(normal emacs) (kbd "SPC")))


;; COMPLETION SYSTEM
(use-package imenu-anywhere)
(use-package ido
  :ensure nil
  :init (ido-mode) (ido-everywhere)
  :config
  (use-package flx-ido :init (flx-ido-mode 1))
  (use-package ido-completing-read+ :init (ido-ubiquitous-mode 1))
  :custom
  (ido-enable-flex-matching nil)
  (ido-case-fold t)
  (ido-use-virtual-buffers t)
  (ido-enable-flex-matching t)
  (ido-use-faces nil))
(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-echo-documentation t)
  :bind (:map corfu-map
	      ("C-j" . corfu-next)
	      ("C-k" . corfu-previous)
	      ("<return>" . corfu-insert)
	      ("<escape>" . corfu-quit)))


;; PROJECT MANAGEMENT SYSTEM
(use-package projectile
  :init (projectile-mode 1)
  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "dependencies")
  (add-to-list 'projectile-globally-ignored-directories "libs")
  :custom
  (projectile-project-search-path '("~/proyectos"))
  (projectile-completion-system 'ido))

;; MAGIT
(use-package magit :config (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

;; EGLOT (LSP)
(use-package eglot
  :custom
  (eglot-ignored-server-capabilites '(:signatureHelpProvider :documentHighlightProvider :codeActionProvider :codeLensProvider :foldingRangeProvider :inlayHintProvider))
  :config
  (add-hook 'c-mode-hook #'eglot-ensure)
  (add-hook 'c++-mode-hook #'eglot-ensure)
  (add-hook 'rustic-mode-hook #'eglot-ensure)
  (add-to-list 'eglot-server-programs '((c-mode c++-mode) . ("clangd" "-j=8" "--log=error" "--clang-tidy" "--header-insertion=never" "--background-index"))))

;; FILE MANAGER
(use-package dired-hide-dotfiles)
(use-package dired-single)
(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-alGhvF --group-directories-first")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (evil-define-key 'normal dired-mode-map "c" 'find-file)
  (evil-define-key 'normal dired-mode-map "h" 'dired-single-up-directory)
  (evil-define-key 'normal dired-mode-map "l" 'dired-single-buffer))

;; OTHERS (NO CATEGORY)
(use-package editorconfig :init (editorconfig-mode 1))
(use-package smartparens :init (smartparens-global-mode 1) (require 'smartparens-config))
(use-package avy)
(use-package glsl-mode)

;; ZOOM
(use-package zoom)

;; CUSTOM FUNCTIONS
(defun move-to-right-window-or-vsplit ()
  (interactive)
  (if (window-at-side-p nil 'right) (progn (split-window-right) (windmove-right)) (windmove-right))
  (zoom))
(defun move-to-bottom-window-or-hsplit ()
  (interactive)
  (if (window-at-side-p nil 'bottom) (progn (split-window-below) (windmove-down)) (windmove-down))
  (zoom))
(defun move-to-up-window-or-hsplit ()
  (interactive)
  (if (window-at-side-p nil 'top) (progn (split-window-below)) (windmove-up))
  (zoom))
(defun move-to-left-window-or-vsplit ()
  (interactive)
  (if (window-at-side-p nil 'left) (progn (split-window-right)) (windmove-left))
  (zoom))
(defun maybe-projectile-find-file ()
  (interactive)
  (call-interactively
   (if (projectile-project-p)
       #'projectile-find-file-dwim
       #'ido-find-find)))
(defun find-private-files ()
  (interactive)
  (if (eq system-type 'gnu/linux) (ido-find-file-in-dir "~/.config/emacs"))
  (if (eq system-type 'windows-nt) (ido-find-file-in-dir "~/.emacs.d"))
  )
(defun delete-window-and-balance ()
  (interactive)
  (delete-window)
  (zoom))


;; KEY MAPS
(use-package which-key :init (which-key-setup-minibuffer) (which-key-mode))
(evil-define-key '(normal visual) 'global (kbd "<leader>pc") 'projectile-compile-project)
(evil-define-key '(normal visual) 'global (kbd "<leader>pp") 'projectile-switch-project)
(evil-define-key '(normal visual) 'global (kbd "<leader>pa") 'projectile-add-known-project)
(evil-define-key '(normal visual) 'global (kbd "<leader>pd") 'projectile-remove-known-project)
(evil-define-key '(normal visual) 'global (kbd "<leader>ps") 'projectile-grep)
(evil-define-key '(normal visual) 'global (kbd "<leader>pf") 'find-private-files)

(evil-define-key '(normal visual) 'global (kbd "<leader>bd") 'kill-current-buffer)
(evil-define-key '(normal visual) 'global (kbd "<leader>bb") 'consult-buffer)
(evil-define-key '(normal visual) 'global (kbd "<leader>be") 'eval-buffer)

(evil-define-key '(normal visual) 'global (kbd "<leader>g") 'magit)
(evil-define-key '(normal visual) 'global (kbd "<leader>m") 'call-last-kbd-macro)
(evil-define-key '(normal visual) 'global (kbd "<leader>k") 'compile)
(evil-define-key '(normal visual) 'global (kbd "<leader>.") 'dired-jump)
(evil-define-key '(normal visual) 'global (kbd "<leader>q") 'save-buffers-kill-emacs)
(evil-define-key '(normal visual) 'global (kbd "<leader>:") 'execute-extended-command)
(evil-define-key '(normal visual) 'global (kbd "<leader>SPC") 'ido-find-file)
(evil-define-key '(normal visual) 'global (kbd "<leader>f") 'ido-switch-buffer)
(evil-define-key '(normal visual) 'global (kbd "<leader>e") 'avy-goto-word-1)
(evil-define-key '(normal visual) 'global (kbd "<leader>s") 'grep)

(evil-define-key '(normal visual) 'global (kbd "<leader>wh") 'move-to-left-window-or-vsplit)
(evil-define-key '(normal visual) 'global (kbd "<leader>wl") 'move-to-right-window-or-vsplit)
(evil-define-key '(normal visual) 'global (kbd "<leader>wj") 'move-to-bottom-window-or-hsplit)
(evil-define-key '(normal visual) 'global (kbd "<leader>wk") 'move-to-up-window-or-hsplit)
(evil-define-key '(normal visual) 'global (kbd "<leader>wm") 'maximize-window)
(evil-define-key '(normal visual) 'global (kbd "<leader>wd") 'delete-window-and-balance)

(evil-define-key '(normal visual) 'global (kbd "<leader>lr") 'eglot-rename)
(evil-define-key '(normal visual) 'global (kbd "<leader>ls") 'imenu-anywhere)

;; POPUP CUSTOMIZATION
(customize-set-variable
 'display-buffer-alist
 '(("\\*compilation\\*"
    (display-buffer-at-bottom)
    (window-height . 0.25))
   ("\\*grep\\*"
    (display-buffer-at-bottom)
    (window-height . 0.25))
   ))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-ignored-server-capabilities
   '(:signatureHelpProvider :documentHighlightProvider :codeActionProvider :codeLensProvider :foldingRangeProvider :inlayHintProvider) nil nil "Customized with use-package eglot")
 '(package-selected-packages
   '(which-key glsl-mode avy zoom smartparens editorconfig dired-single dired-hide-dotfiles eglot magit projectile corfu ido-completing-read+ flx-ido imenu-anywhere general evil-collection evil naysayer-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
