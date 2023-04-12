;;; customfuncs.el --- Summary
;;; commentary:
;;; code:

(defun move-to-right-window-or-vsplit ()
  (interactive)
  (if (window-at-side-p nil 'right)
      (progn
	(split-window-right)
	(windmove-right)
	)
    (windmove-right))
  (zoom)
  )
(defun move-to-bottom-window-or-hsplit ()
  (interactive)
  (if (window-at-side-p nil 'bottom)
      (progn
	(split-window-below)
	(windmove-down)
	)
    (windmove-down))
  (zoom)
  )
(defun move-to-up-window-or-hsplit ()
  (interactive)
  (if (window-at-side-p nil 'top)
      (progn
	(split-window-below)
	)
    (windmove-up))
  (zoom)
  )
(defun move-to-left-window-or-vsplit ()
  (interactive)
  (if (window-at-side-p nil 'left)
      (progn
	(split-window-right)
	)
    (windmove-left))
  (zoom)
  )

(defun maybe-projectile-find-file ()
  (interactive)
  (call-interactively
   (if (projectile-project-p)
       #'projectile-find-file-dwim
       #'ido-find-file)))

(defun find-private-files ()
  (interactive)
  (find-file "~/proyectos/myemacs/init.el"))


(provide 'customfuncs)
;;; customfuncs.el ends here
