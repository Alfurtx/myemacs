;;; init.el --- Summary
;;; MYEMACS Init.el configuration file
;;; -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Alfonso Amorós
;;
;; Author: Alfonso Amorós <alfonso.alfurtx@gmail.com>
;; Maintainer: Alfonso Amorós <alfonso.alfurtx@gmail.com>
;; Created: junio 18, 2022
;; Modified: junio 18, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/fonsi/init
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;; Don't care, didn't ask
;;
;;
;;

;;; Code:

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -2)
(set-fringe-mode 10)

(menu-bar-mode -1)

(setq visible-bell 1)
(setq ring-bell-function 'ignore)
(setq-default truncate-lines 1)

;; (setq-default mode-line-format nil)
;; (setq mode-line-format nil)

(setq make-backup-files nil)

(setq scroll-conservatively 101) ;; value greater than 100 gets rid of half page jumping
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; how many lines at a time
(setq mouse-wheel-progressive-speed t) ;; accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-margin 4)

(global-hl-line-mode 1)
;; (set-face-attribute 'hl-line nil :inherit nil :background "midnight blue")
;; (push "/home/fonsi/proyectos/myemacs/themes" custom-theme-load-path)
;; (load-theme 'handmadehero t)
;; 
;; (setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
;; (make-face 'font-lock-fixme-face)
;; (make-face 'font-lock-note-face)
;; (mapc (lambda (mode)
;;         (font-lock-add-keywords
;;          mode
;;          '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
;;            ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
;;       fixme-modes)
;; (modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
;; (modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

(use-package naysayer-theme
  :config
  (load-theme 'naysayer t)
  (set-face-attribute 'default nil :height 170)
  ;; MODELINE customization
  (setq mode-line-format
	(delete 'mode-line-modes mode-line-format)))

(use-package doom-modeline
  :init
  ;; (doom-modeline-mode 1)
  ;; (setq doom-modeline-height 15)
  ;; (setq doom-modeline-modal-icon nil)
  ;; (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  )

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  ;; Use different regex strategies per completion command
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))


(use-package swiper)

(use-package which-key
  :init
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t
        which-key-separator " → " ))
(which-key-mode)

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode 1)) ;; this gets us descriptions in M-x.

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ;; ("C-M-j" . counsel-switch-buffer)
         ("C-M-l" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package all-the-icons)

(use-package smex)
(smex-initialize)

(use-package evil
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
  (define-key evil-visual-state-map (kbd "C-c") 'evil-normal-state)
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init '(magit dired)))
(use-package evil-tutor)

(use-package general
  :config
  (general-evil-setup 1))

(use-package ivy-posframe
  :init
  (setq ivy-posframe-parameters
	'((left-fringe . 8)
	  (internal-border-width . 2)
	  (right-fringe . 8)))
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  ;; (setq ivy-posframe-height-alist '((swiper . 20) (dmenu . 20) (t . 10)))
  :config
  (ivy-posframe-mode 1)
  :custom-face
  (ivy-posframe-border ((t (:background "#ffffff"))))
  ) ; 1 enables posframe-mode, 0 disables it.

;; Using garbage magic hack.
 (use-package gcmh
   :config
   (gcmh-mode 1))
;; Setting garbage collection threshold
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Silence compiler warnings as they can be pretty disruptive (setq comp-async-report-warnings-errors nil)

;; Silence compiler warnings as they can be pretty disruptive
(if (boundp 'comp-deferred-compilation)
    (setq comp-deferred-compilation nil)
    (setq native-comp-deferred-compilation nil))
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/proyectos")))

(use-package magit :ensure t)
(require 'magit)
(setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)

(use-package ranger
  :config
  (setq ranger-show-hidden t))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-udpate-hide-results t)
  :config
  (auto-package-update-maybe))

;; (use-package golden-ratio
;;   :init
;;   (golden-ratio-mode 1)
;;   :config
;;   (setq golden-ratio-auto-scale t))

(use-package counsel-projectile
  :config (counsel-projectile-mode +1))

(use-package lsp-mode
  :hook ((c++-mode . lsp)
	 (c-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-lens-enable nil)
  (setq lsp-clients-clangd-args
	'("-j=4"
	  "--background-index"
	  "--clang-tidy"
	  "--header-insertion=never")))

(use-package rustic
  :config
  (setq rustic-lsp-setup-p nil)
  (setq rustic-lsp-client nil)
  (remove-hook 'rustic-mode-hook 'flycheck-mode)
  (setq rustic-format-on-save t))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package flycheck
  :init (global-flycheck-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package smartparens
  :config
  (smartparens-global-mode 1)
  (require 'smartparens-config))

(use-package flx)

;; (use-package all-the-icons-ivy
;;   :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package clang-format
  :init (setq clang-format-style "file"))

(defun clang-format-on-save ()
       (when (or
              (eq major-mode 'c-mode)
              (eq major-mode 'c++-mode))
         (clang-format-buffer)))
  
(add-hook 'before-save-hook #'clang-format-on-save)

(use-package company
  :init
  (setq company-format-margin-function 'company-text-icons-margin)
  :config
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
  (define-key company-search-map (kbd "C-j") 'company-select-next)
  (define-key company-search-map (kbd "C-k") 'company-select-previous)
  (global-company-mode))

(use-package consult-lsp)

(nvmap :prefix "SPC"
  "l" '(:ignore t :wk "LSP")
  "l s" '(lsp-ivy-workspace-symbol :wk "Search symbol")
  "l r" '(lsp-rename :wk "Rename")
  "l d" '(consult-flycheck :wk "See diagnostics")
  )

(nvmap :prefix "SPC"
  "b" '(:ignore t :wk "BUFFERS")
  "b b" '(counsel-switch-buffer :wk "Ibuffer")
  "b d" '(kill-current-buffer :wk "Kill buffer"))

(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
  "e" '(:ignore t :wk "EVAL")
  "e b" '(eval-buffer :wk "Eval buffer")
  "e r" '(eval-region :wk "Eval region"))

(nvmap :prefix "SPC"
  "g" '(:ignore t :wk "GIT")
  "g g" '(magit :wk "Magit"))

(nvmap :prefix "SPC"
  "p" '(:ignore t :wk "PROJECTS")
  "p f" '(counsel-projectile-find-file :wk "Find file in project")
  "p p" '(counsel-projectile-switch-project :wk "Projects")
  "p a" '(projectile-add-known-project :wk "Add project")
  "p d" '(projectile-remove-known-project :wk "Remove project"))

(nvmap :prefix "SPC"
  "f" '(:ignore t :wk "FILES")
  "f f" '(counsel-file-jump :wk "Find file")
  "f r" '(counsel-recentf :wk "Recent files")
  "f p" '((lambda () (interactive) (counsel-find-file "~/proyectos/myemacs/")) :wk "Private Config"))

(nvmap :prefix "SPC"
  "SPC" '(counsel-find-file :wk "Explore files")
  "." '(ranger :wk "Open ranger")
  "m" '(call-last-kbd-macro :wk "Call Macro")
  "q" '(save-buffers-kill-emacs :wk "Quit")
  ":" '(counsel-M-x :wk "Find command"))

(nvmap :prefix "SPC"
  "c" '(:ignore t :wk "CODE")
  "c c" '(counsel-compile :wk "Compile")
  "c C" '(comment-or-uncomment-region :wk "Comment region"))

(nvmap :prefix "SPC"
  "w" '(:ignore t :wk "WINDOW")
  "w d"   '(evil-window-delete :which-key "Close window")
  "w n"   '(evil-window-new :which-key "New window")
  "w s"   '(evil-window-split :which-key "Horizontal split window")
  "w v"   '(evil-window-vsplit :which-key "Vertical split window")
  ;; Window motions
  "w h"   '(evil-window-left :which-key "Window left")
  "w j"   '(evil-window-down :which-key "Window down")
  "w k"   '(evil-window-up :which-key "Window up")
  "w l"   '(evil-window-right :which-key "Window right")
  "w +"   '(evil-window-increase-width :which-key "Window increase width")
  "w w"   '(evil-window-next :which-key "Goto next window"))

;; POPUP rules

(add-to-list 'display-buffer-alist '("\\*compilation\\*"
				      (display-buffer-in-side-window)
				      (side . left)
				      (slot . 1)
				      (window-width . shrink-window-if-larger-than-buffer)
				      (dedicated . t)))
(add-to-list 'display-buffer-alist '("\\*rustic-compilation\\*"
				      (display-buffer-in-side-window)
				      (side . left)
				      (slot . 1)
				      (window-width . shrink-window-if-larger-than-buffer)
				      (dedicated . t)))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("6ae64ffd3a4043be42c6d4e4e25c0498b1b2d7c4608c4f1dd908667f95a18bb4" "583277bd24a057630e73b5d72cd78d06f520a0accde5d9d8746d1f9598f38fd8" default))
 '(package-selected-packages
   '(rustic naysayer-theme consult-lsp company-fuzzy company clang-format all-the-icons-ivy flx flycheck doom-modeline magit smartparens editorconfig lsp-ivy lsp-mode counsel-projectile which-key use-package smex ranger projectile ivy-rich ivy-posframe golden-ratio general gcmh evil-tutor evil-collection counsel auto-package-update all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-posframe-border ((t (:background "#ffffff")))))
