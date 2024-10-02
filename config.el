;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'tsdh-dark)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq calendar-week-start-day 1) ; 0 is Sunday, 1 is Monday

;; Org mode configuration
(after! org
  (add-to-list 'org-modules 'ol-gnus)
  (setq org-modern-label-border nil)
  (global-org-modern-mode)
  (setq org-agenda-prefix-format
        '((agenda . "   %i %-12c%?-2t %-12s %-6e")  ; Agenda items: icon, category, time, and extra info, estimate
          (todo .   "   %i %-12:c %-12:t %s")   ; TODO items: icon, category, time (if any), and extra info
          (tags .   "   %i %-12:c %-12:t %s")   ; Tagged items: icon, category, time (if any), and extra info
          (search . "   %i %-12:c %s")))      ; Search results: icon, category, and extra info
  (setq org-agenda-custom-commands '(("N" "TODOs without Deadlines or Schedules"
                                      todo "TODO" ((org-agenda-skip-function '(org-agenda-skip-entry-if
                                                                               'scheduled 'deadline))))))
  (setq org-capture-templates '(("f" "Fstart entry" entry (file "fstart.org")
                                 "* TODO %?\n  %i\n  From: %a\n  %t" :empty-lines 1
                                 )
                                ("p" "Prive entry" entry (file "prive.org")
                                 "* TODO %?\n  %i\n  From: %a\n  %t" :empty-lines 1
                                 )
                                ("w" "Work entry" entry (file "work.org")
                                 "* TODO %?\n  %i\n  From: %a\n  %t" :empty-lines 1
                                 )))
  (add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:todo "STRT")
          (:name "Important"
           :priority "A")
          (:name "Quick Picks"
           :effort< "0:30")
          (:priority<= "B"
           :scheduled future
           :order 1)
          (:auto-category)))
  (custom-set-faces '(org-super-agenda-header ((t (:foreground "dark orange" :weight bold :height: 1.3)))))
  (setq org-super-agenda-header-separator "\n---\n")
  )

;; Use gnome GPG
(after! gnus-agent
  (setq epg-pinentry-mode 'nil)
  )

;; Make sure comments don't continue on the next linees
(setq-hook! 'rust-mode-hook comment-line-break-function nil)

;; Ellama setup
(after! ellama
  (setopt ellama-keymap-prefix "C-c z")  ;; keymap for all ellama functions
  (setopt ellama-language "English")
  (require 'llm-openai)
  (setopt ellama-provider
	  (make-llm-ollama
	   ;; this model should be pulled to use it
	   ;; value should be the same as you print in terminal during pull
	   :chat-model "qwen2.5:14b"
	   :embedding-model "qwen2.5:14b"
	   ))
  (setq llm-warn-on-nonfree nil
        ellama-providers
        '(("gpt4o" . (make-llm-openai
                      :key (getenv "OPENAI_API_KEY")
                      :chat-model "gpt-4o"
                      ))
          ("deepseek-chat" . (make-llm-openai-compatible
                              :key (getenv "DEEPSEEK_API_KEY")
                              :url "https://api.deepseek.com/"
                              :chat-model "deepseek-chat"))
          )
        )
  )

;; Show types in lsp-mode
(after! lsp-mode
  (setq lsp-inlay-hint-enable t)
  )

;;  Don't autoformat on C
(setq +format-on-save-enabled-modes
      '(not c-mode  ; Clang-format not good enoug
        ))
