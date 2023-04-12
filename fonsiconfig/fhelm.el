;;; fhelm.el --- Summary
;;; commentary:
;;; code:

; SETUP IDO
(use-package flx-ido)

(ido-mode)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t
      ido-case-fold t
      ido-use-virtual-buffers t
      ido-enable-flex-matching t
      ido-use-faces nil)

(use-package ido-completing-read+
  :init (ido-ubiquitous-mode 1)
  :config (setq ido-ubiquitous-max-items 10000
		ido-cr+-max-items 10000))

; SETUP CORFU
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-auto t)
  :bind
  (:map corfu-map
	("C-j" . corfu-next)
	("C-k" . corfu-previous))
  :init
  (global-corfu-mode))

(provide 'fhelm)

;;; fhelm.el ends here
