;;; package --- summary
;;; commentary:
;;; code:

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("<return>" . ivy-alt-done)
         ("TAB" . ivy-next-line)
         ("S-TAB" . ivy-previous-line)
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
  (setq enable-recursive-minibuffers t))

(use-package ivy-rich
  :after ivy
  ; :custom
  ; (ivy-virtual-abbreviate 'full)
  ; (ivy-rich-switch-buffer-align-virtual-buffer t)
  ; (ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode 1)) ;; this gets us descriptions in M-x.

(use-package ivy-posframe
  :init
  (setq ivy-posframe-parameters
	'((left-fringe . 8)
	  (internal-border-width . 2)
	  (right-fringe . 8)))
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  :config
  (ivy-posframe-mode 1)
  :custom-face
  (ivy-posframe-border ((t (:background "#ffffff")))))

(use-package flx)

(use-package orderless
  :after ivy
  :ensure t
  :config
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
  (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package counsel
  :after ivy
  :config
  (setq counsel-find-file-ignore-regexp "dependencies.*\\|deps.*\\|dep.*\\|libraries.*\\|lib.*\\|libs.*")
  :bind (("M-x" . counsel-M-x)))

(use-package counsel-projectile
  :after counsel
  :config (counsel-projectile-mode +1))

(use-package ivy-xref
  :ensure t
  :after ivy
  :init
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(provide 'ivy)
;;; ivy.el ends here
