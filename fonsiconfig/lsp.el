;;; package --- summary
;;; commentary:
;;; code:

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

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package consult-lsp)

(provide 'lsp)
;;; lsp.el ends here
