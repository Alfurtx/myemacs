;;; package --- summary
;;; commentary:
;;; code:

(add-to-list 'display-buffer-alist '("\\*compilation\\*"
				      (inhibit-same-window . t)
				      (display-buffer-in-side-window)
				      (side . right)
				      (slot . 1)
				      (window-width . shrink-window-if-larger-than-buffer)
				      (dedicated . t)))

(provide 'popup)

;;; popup.el ends here
