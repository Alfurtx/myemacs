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
(setq-default truncate-lines nil)
(setq make-backup-files nil)
(setq scroll-conservatively 101) ;; value greater than 100 gets rid of half page jumping
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; how many lines at a time
(setq mouse-wheel-progressive-speed t) ;; accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-margin 4)

(global-hl-line-mode 1)
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
; (global-visual-line-mode)
;; setup modeline up top
(setq mode-line-format (delete 'mode-line-modes mode-line-format))
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)
(setq compilation-scroll-output t)
(setq gdb-many-windows t)
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

(provide 'misc)

;;; misc.el ends here
