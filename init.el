;;; init.el --- Summary
;;; commentary:
;;; code:

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -2)
(set-fringe-mode 10)
(menu-bar-mode -1)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq inhibit-startup-message t
      native-comp-async-report-warnings-errors nil
      visible-bell nil
      ring-bell-function 'ignore
      make-backup-files nil
      scroll-conservatively 101
      mouse-wheel-scroll-amount '(3 ((shift) . 3))
      scroll-margin 2
      mode-line-format (delete 'mode-line-modes mode-line-format)
      compilation-scroll-output t
      gdb-many-windows t
      package-gnupghome-dir "elpa/gnupg"
      pixel-scroll-precision-mode 1)

(setq-default truncate-lines t
	      header-line-format mode-line-format
	      mode-line-format nil)

(require 'package)
(when (eq system-type 'gnu/linux) (setq package-user-dir "~/.config/emacs/packages"))
(when (eq system-type 'windows-nt) (setq package-user-dir "~/.emacs.d/packages"))
(when (eq system-type 'darwin) (setq package-user-dir "~/.config/emacs/packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/") ("org" . "https://orgmode.org/elpa/") ("gnu-devel" . "https://elpa.gnu.org/devel/") ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package naysayer-theme :init (load-theme 'naysayer t) (add-to-list 'default-frame-alist '(font . "Liberation Mono-14")) (set-face-attribute 'default t :font "Liberation Mono-14"))
(use-package evil :init (evil-mode 1) :custom (evil-want-keybinding nil) (evil-vsplit-window-right t) (evil-split-window-below t) (evil-emacs-state-modes nil) (evil-motion-state-modes nil) (evil-insert-state-modes nil) :config (evil-define-key '(insert normal) 'global (kbd "C-j") nil) (evil-define-key '(insert normal) 'global (kbd "C-k") nil) (evil-define-key '(visual insert) 'global (kbd "C-c") 'evil-normal-state) (evil-define-key '(normal visual) 'global "K" (kbd "10k")) (evil-define-key 'normal 'global "J" (kbd "10j")) (evil-define-key 'normal 'global "L" (kbd "10l")) (evil-define-key 'normal 'global "H" (kbd "10h")) (evil-set-leader '(normal motion emacs) (kbd "SPC")))
(use-package evil-collection :init (evil-collection-init '(magit dired vc-git vc-dir vc-annotate)))
(use-package general :init (general-evil-setup t))
(use-package imenu-anywhere)
(use-package ido :ensure nil :init (ido-mode) (ido-everywhere) :config (use-package flx-ido :init (flx-ido-mode 1)) (use-package ido-completing-read+ :init (ido-ubiquitous-mode 1)) :custom (ido-enable-flex-matching t) (ido-case-fold t) (ido-use-virtual-buffers t) (ido-use-faces nil))
(use-package magit :config (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))
(use-package dired-hide-dotfiles)
(use-package dired-single)
(use-package dired :ensure nil :config (setq dired-listing-switches "-alGhvF --group-directories-first") (setq dired-recursive-copies 'always) (setq dired-recursive-deletes 'always) (evil-define-key 'normal dired-mode-map "c" 'find-file) (evil-define-key 'normal dired-mode-map "h" 'dired-single-up-directory) (evil-define-key 'normal dired-mode-map "l" 'dired-single-buffer))
(use-package editorconfig :init (editorconfig-mode 1))
(use-package smartparens :init (smartparens-global-mode 1) (require 'smartparens-config))
(use-package avy)
(use-package glsl-mode)
(use-package which-key :init (which-key-setup-minibuffer) (which-key-mode))
(use-package darkroom :hook (org-mode . darkroom-mode))
(use-package org :config (setq org-hide-emphasis-markers t) (add-hook 'org-mode-hook 'visual-line-mode) (use-package org-bullets :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))
(use-package gcmh :init (gcmh-mode 1))

; (when (eq system-type 'gnu/linux)  (setq fonsi/project-search-path '("~/proyectos/")))
; (when (eq system-type 'windows-nt) (setq fonsi/project-search-path '("~/Work/")))

(defun fonsi/wright () (interactive) (if (window-at-side-p nil 'right) (progn (split-window-right) (windmove-right)) (windmove-right)))
(defun fonsi/wdown () (interactive) (if (window-at-side-p nil 'bottom) (progn (split-window-below) (windmove-down)) (windmove-down)))
(defun fonsi/wup   () (interactive) (if (window-at-side-p nil 'top) (progn (split-window-below)) (windmove-up)))
(defun fonsi/wleft  () (interactive) (if (window-at-side-p nil 'left) (progn (split-window-right)) (windmove-left)))

(defun fonsi/private () (interactive)
       (when (eq system-type 'gnu/linux) (find-file "~/.config/emacs"))
       (when (eq system-type 'windows-nt) (find-file "~/.emacs.d"))
       (when (eq system-type 'darwin) (find-file "~/.config/emacs"))
       )


(general-nvmap
  :prefix "SPC"
  "pc" 'project-compile
  "pr" 'project-shell-command
  "ps" 'project-find-regexp
  "pp" 'project-switch-project
  "bd" 'kill-current-buffer
  "be" 'eval-buffer
  "fp" 'fonsi/private
  "g" 'magit
  "m" 'call-last-kbd-macro
  "k" 'compile
  "." 'dired-jump
  ":" 'execute-extended-command
  "q" 'save-buffers-kill-emacs
  "SPC" 'ido-find-file
  "j" 'ido-switch-buffer
  "e" 'avy-goto-word-1
  "s" 'grep
  "wl" 'fonsi/wright
  "wj" 'fonsi/wdown
  "wk" 'fonsi/wup
  "wh" 'fonsi/wleft
  "wm" 'maximize-window
  "wd" 'delete-window
  "ls" 'imenu-anywhere
  )

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
 '(package-selected-packages
   '(gcmh darkroom org-bullets org-bullet avy dired-hide-dotfiles dired-single editorconfig evil-collection flx-ido general glsl-mode ido-completing-read+ imenu-anywhere magit naysayer-theme smartparens which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
