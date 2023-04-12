;;; oldinit.el --- Summary
;;; commentary:
;;; code:

(add-to-list 'load-path "~/proyectos/myemacs/fonsiconfig/")

; (load-file "~/proyectos/myemacs/fonsiconfig/misc.el")
(require 'misc)

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

; (load-file "~/proyectos/myemacs/fonsiconfig/garbagecollection.el")
(require 'garbagecollection)

; (add-hook 'prog-mode-hook #'flymake-mode)

;; (use-package textsize
;;   :ensure t
;;   :commands textsize-mode
;;   :config
;;   (setq textsize-default-points 18)
;;   :init (textsize-mode))

(use-package naysayer-theme
  :config
  (load-theme 'naysayer t)
  (set-face-attribute 'default nil :height 180 :font "Iosevka Custom")
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "#d1b897")
  (add-to-list 'default-frame-alist '(font . "Iosevka Custom-18")))


;; Ligatures (Currently only works for Iosevka)
(use-package ligature
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))


(load-file "~/proyectos/myemacs/fonsiconfig/ivy.el")

(use-package which-key
  :init
  (which-key-setup-minibuffer)
  (setq which-key-separator " → " )
  (which-key-mode))

(use-package all-the-icons)

; (load-file "~/proyectos/myemacs/fonsiconfig/evil.el")
(require 'fevil)

(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (with-eval-after-load 'ivy
    (setq projectile-completion-system 'ivy))
  (setq projectile-project-search-path '("~/proyectos")))

(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

; (use-package golden-ratio
;   :disabled t
;   :init
;   (golden-ratio-mode 1)
;   :config
;   (setq golden-ratio-auto-scale t))
(use-package zoom
  :config (setq zoom-mode t))

;;; Language Server Protocol
; (load-file "~/proyectos/myemacs/fonsiconfig/lsp.el")
(use-package eglot
  :config
  (setq completion-category-defaults nil)
  (add-hook 'c-mode-hook #'eglot-ensure)
  (add-hook 'c++-mode-hook #'eglot-ensure)
  (add-hook 'rustic-mode-hook #'eglot-ensure)
  (add-to-list 'eglot-server-programs
	       '((c-mode c++-mode) . ("clangd"
				      "-j=8"
				      "--log=error"
				      "--clang-tidy"
				      "--header-insertion=never"
				      "--background-index"))))
  ; (add-to-list 'elgot-server-programs '(c-mode c++-mode . ("clangd" "-j=8")))
; (use-package consult)
(use-package consult-eglot)

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
    "c" 'find-file
    "h" 'dired-up-directory
    "l" 'dired-find-file))
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package visual-fill-column)
(global-visual-fill-column-mode 1)
(setq-default visual-fill-column-width 150)
(setq-default visual-fill-column-center-text t)

;;; CUSTOM WINDOW MANAGEMENT FUNCTIONS + OTHERS
(load-file "~/proyectos/myemacs/fonsiconfig/customfuncs.el")

;;; WHICH KEY KEYMAPS
(load-file "~/proyectos/myemacs/fonsiconfig/whichkeymaps.el")

;;; oldinit.el ends here
