;;; feglot.el --- Summary
;;; commentary:
;;; code:

(use-package eglot
  :config
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

(provide 'feglot)
;;; feglot.el ends here
