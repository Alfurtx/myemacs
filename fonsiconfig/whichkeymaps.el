;;; whichkeymaps.el --- summary
;;; commentary:
;;; code:

(nvmap :prefix "SPC"
  "l" '(:ignore t :wk "LSP")
  "l s" '(consult-imenu-multi :wk "Search symbol")
  "l r" '(eglot-rename :wk "Rename")
  "l d" '(flymake-show-project-diagnostics :wk "See diagnostics")
  )

(nvmap :prefix "SPC"
  "b" '(:ignore t :wk "BUFFERS")
  "b b" '(counsel-switch-buffer :wk "Ibuffer")
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
  "p f" '(counsel-projectile-find-file :wk "Find file in project")
  "p c" '(projectile-compile-project :wk "Compile project")
  "p p" '(counsel-projectile-switch-project :wk "Switch projects")
  "p a" '(projectile-add-known-project :wk "Add project")
  "p d" '(projectile-remove-known-project :wk "Remove project"))

(nvmap :prefix "SPC"
  "f" '(:ignore t :wk "FILES")
  "f f" '(counsel-file-jump :wk "Find file")
  "f r" '(counsel-recentf :wk "Recent files")
  "f p" '((lambda () (interactive) (counsel-find-file "~/proyectos/myemacs/")) :wk "Private Config"))

(nvmap :prefix "SPC"
  "SPC" '(maybe-projectile-find-file :wk "Explore files")
  "." '(dired-jump :wk "Browse files")
  "m" '(compile :wk "Shell command")
  "q" '(save-buffers-kill-emacs :wk "Quit")
  "s" '(swiper-all :wk "Search on all open buffers")
  ":" '(counsel-M-x :wk "Find command"))

(nvmap :prefix "SPC"
  "w" '(:ignore t :wk "WINDOW")
  "w d"   '(evil-window-delete :which-key "Close window")
  "w n"   '(evil-window-new :which-key "New window")
  "w s"   '(evil-window-split :which-key "Horizontal split window")
  "w v"   '(evil-window-vsplit :which-key "Vertical split window")
  "w m"   '(delete-other-windows :which-key "Maximize current window")
  ;; Window motions
  "w h"   '(move-to-left-window-or-vsplit :which-key "Window left")
  "w j"   '(move-to-bottom-window-or-hsplit :which-key "Window down")
  "w k"   '(move-to-up-window-or-hsplit :which-key "Window up")
  "w l"   '(move-to-right-window-or-vsplit :which-key "Window right")
  "w +"   '(evil-window-increase-width :which-key "Window increase width")
  "w w"   '(evil-window-next :which-key "Goto next window"))

(provide 'whichkeymaps)
;;; whichkeymaps.el ends here
