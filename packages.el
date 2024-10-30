;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! org-modern)

(package! org-ql)

(package! org-super-agenda)

(package! ellama)

(package! gptel)

(package! aider
  :recipe (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :pin "fe6148c0648f07293a047d1c687944d33c40f4e4")

(package! kconfig-mode
  :recipe (:host github :repo "delaanthonio/kconfig-mode")
  :pin "cd87b71c8c1739d026645ece0bbd20055a7a2d4a")

(package! systemrdl-mode
  :recipe (:host github :repo "luisgutz/emacs-system-rdl")
  :pin "b6889528a67cd169326422bfbdd6cdd031cbd09b")

(package! ement)
