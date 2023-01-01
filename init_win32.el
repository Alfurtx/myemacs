;;; Personal configuration -*- lexical-binding: t -*-

;; Save the contents of this file under ~/.emacs.d/init.el
;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you
;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide.

;; Setup package manager
(require 'package)
(add-to-list 'package-archives  '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives  '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives  '("elpa" . "https://elpa.gnu.org/packages/"))
(setq package-user-dir "~/.emacs.d/packages")
(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))
(unless (package-installed-p 'use-package)
   (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; EMACS SERVER STUFF
; (require 'server)
; (if (not server-running-p) (server-start))
; to quit emacs but not close the server so that next time we dont have to restart the server again
(defun quit-but-not-the-server ()
  "Exit server buffers and hide teh main Emacs window"
  (interactive)
  (server-edit)
  (make-frame-invisible nil t))

;; Miscellaneous stuff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(set-fringe-mode 10)
(setq visible-bell 1)
(setq ring-bell-function 'ignore)
(global-hl-line-mode 1)

;;; Project management
(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/Proyectos")))

;;; Completions
(use-package vertico
  :init
  (vertico-mode t)
  ; (vertico-flat-mode t)
  (use-package savehist
    :init
    (savehist-mode))
  (use-package emacs
    :init
    (setq enable-recursive-minibuffers t)
    (setq tab-always-indent 'complete))
  (use-package orderless
    :init
    (setq completion-styles '(substring orderless basic)
	  completion-category-defaults nil
	  completion-category-overrides '((file (styles partial-completion)))))
  )
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("C-DEL" . vertico-directory-delete-word)
	      ("C-j" . vertico-next)
	    ("C-k" . vertico-previous)
	      ("TAB" . vertico-next)
	      ("S-TAB" . vertico-previous))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  )

;; Pop-up completion
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-cycle t)
  (corfu-preselect-first t)
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
	("C-k" . corfu-previous)
  	("C-j" . corfu-next)
  	("S-TAB" . corfu-previous)
  	("TAB" . corfu-next)
  	([backtab] . corfu-previous)
  	([tab] . corfu-next))
  )

;;; Extended completion utilities
(use-package consult)
(use-package consult-projectile)

;; Automatically pair parentheses
(electric-pair-mode t)

;;; LSP Support
(use-package eglot)

;;; Git client
(use-package magit
  :ensure t
  :init
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

;;; Multiple modes support
(use-package json-mode
  :mode "\\.json\\'")
(use-package nasm-mode
  :mode "\\.asm\\'")
(use-package rust-mode
  :mode "\\.rs\\'")
(use-package typescript-mode
  :mode "\\.ts\\'")

;;; EditorConfig support
(use-package editorconfig
  :init
  (editorconfig-mode 1))

;;; Vim Emulation
(use-package general
  :config
  (general-evil-setup 1))
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
  (define-key evil-visual-state-map (kbd "C-c") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "J") (kbd "10j"))
  (define-key evil-normal-state-map (kbd "K") (kbd "10k"))
  (define-key evil-normal-state-map (kbd "L") (kbd "w"))
  (define-key evil-normal-state-map (kbd "H") (kbd "b"))
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init '(magit dired)))

;;; Naysayer Theme
(use-package naysayer-theme
  :config
  (load-theme 'naysayer t)
  (set-face-attribute 'default nil :height 130 :font "Liberation Mono")
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "#d1b897")
  (setq mode-line-format (delete 'mode-line-modes mode-line-format))
  (add-to-list 'default-frame-alist '(font . "Liberation Mono-13")))

;;; Golden ratio for window balancing
(use-package golden-ratio
  :config
  (golden-ratio-mode 1))

;;; Odin Mode (ISSUE it doesn't properly download, it seems to not exists in the sources for use-package)
;; (use-package odin-mode)

;;; CUSTOM FUNCTIONS FOR WINDOW MANAGEMENT
(defun move-to-right-window-or-vsplit ()
  (interactive)
  (if (window-at-side-p nil 'right)
      (progn
	(split-window-right)
	(windmove-right)
	)
    (windmove-right))
  (golden-ratio)
  )
(defun move-to-bottom-window-or-vsplit ()
  (interactive)
  (if (window-at-side-p nil 'bottom)
      (progn
	(split-window-below)
	(windmove-down)
	)
    (windmove-down))
  (golden-ratio)
  )
(defun move-to-up-window-or-vsplit ()
  (interactive)
  (if (window-at-side-p nil 'top)
      (progn
	(split-window-below)
	)
    (windmove-up))
  (golden-ratio)
  )
(defun move-to-left-window-or-vsplit ()
  (interactive)
  (if (window-at-side-p nil 'left)
      (progn
	(split-window-right)
	)
    (windmove-left))
  (golden-ratio)
  )

;;; WhichKey 
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
        which-key-separator " → " )
  :config
  (which-key-mode))
(nvmap :prefix "SPC"
  "l" '(:ignore t :wk "LSP")
  "l l" '(eglot :wk "m-x eglot")
  "l r" '(eglot-rename :wk "rename")
  "l f" '(eglot-format-buffer :wk "format")
  "l a" '(eglot-actions :wk "actions")
  "l d" '(flymake-show-project-diagnositcs :wk "diagnostics")
  "l i" '(imenu :wk "search symbol"))
(nvmap :prefix "SPC"
  "b" '(:ignore t :wk "BUFFERS")
  "b b" '(consult-buffer :wk "Ibuffer")
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
  "p p" '(consult-projectile-switch-project :wk "Switch to project")
  "p a" '(projectile-add-known-project :wk "Add known project")
  "p d" '(projectile-remove-known-project :wk "Add known project")
  "p f" '(consult-projectile-find-file :wk "Find file in project"))
(nvmap :prefix "SPC"
  "f" '(:ignore t :wk "FILES")
  "f f" '(consult-locate :wk "Find file")
  "f p" '((lambda () (interactive) (find-file "~/.emacs.d/")) :wk "Private Config"))
(nvmap :prefix "SPC"
  "q" '(:ignore t :wk "QUIT")
  "q q" '(quit-but-not-the-server :wk "Close frames but not kill emacs")
  "q s" '(save-buffers-kill-emacs :wk "Close all and KILL emacs server"))
(nvmap :prefix "SPC"
  "SPC" '(find-file :wk "Explore files")
  "m" '(call-last-kbd-macro :wk "Call Macro")
  "." '(dired :wk "Dired")
  ":" '(execute-extended-command :wk "M-x"))
(nvmap :prefix "SPC"
  "w" '(:ignore t :wk "WINDOW")
  "w d"   '(evil-window-delete :which-key "Close window")
  "w n"   '(evil-window-new :which-key "New window")
  "w s"   '(evil-window-split :which-key "Horizontal split window")
  "w v"   '(evil-window-vsplit :which-key "Vertical split window")
  ;; Window motions
  "w h"   '(move-to-left-window-or-vsplit :which-key "Window left")
  "w j"   '(move-to-bottom-window-or-vsplit :which-key "Window down")
  "w k"   '(move-to-up-window-or-vsplit :which-key "Window up")
  "w l"   '(move-to-right-window-or-vsplit :which-key "Window right")
  "w +"   '(evil-window-increase-width :which-key "Window increase width")
  "w w"   '(evil-window-next :which-key "Goto next window"))

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no #'y-or-n-p)
(setq make-backup-files nil)
;; (setq-default 'truncate-lines nil)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Miscellaneous code from ChatGPT
