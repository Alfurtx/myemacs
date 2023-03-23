;;; package --- summary
;;; commentary:
;;; code:

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

(global-visual-line-mode)

;; setup modeline up top
(setq mode-line-format (delete 'mode-line-modes mode-line-format))
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)

(provide 'misc)

;;; misc.el ends here
