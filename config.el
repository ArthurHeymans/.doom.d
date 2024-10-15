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
  ;; (setq org-modern-label-border nil)
  ;; (global-org-modern-mode) I don't like it so much

  ;; Agenda prefix
  (setq org-agenda-prefix-format
        '((agenda . "   %i %-12c%?-2t %-12s %-6e")  ; Agenda items: icon, category, time, and extra info, estimate
          (todo .   "   %i %-12:c %-12:t %s")   ; TODO items: icon, category, time (if any), and extra info
          (tags .   "   %i %-12:c %-12:t %s")   ; Tagged items: icon, category, time (if any), and extra info
          (search . "   %i %-12:c %s")))      ; Search results: icon, category, and extra info

  ;; Agenda items that are not sceduled
  (setq org-agenda-custom-commands '(("N" "TODOs without Deadlines or Schedules"
                                      todo "TODO" ((org-agenda-skip-function '(org-agenda-skip-entry-if
                                                                               'scheduled 'deadline))))
                                     ("R" "Finish review process"
                                      todo "REVIEW")))

  ;; Org capture templates
  (setq org-capture-templates '(("f" "Fstart entry" entry (file "fstart.org")
                                 "* TODO %?\n  %i\n  From: %a\n  %t" :empty-lines 1
                                 )
                                ("p" "Prive entry" entry (file "prive.org")
                                 "* TODO %?\n  %i\n  From: %a\n  %t" :empty-lines 1
                                 )
                                ("w" "Work entry" entry (file "work.org")
                                 "* TODO %?\n  %i\n  From: %a\n  %t" :empty-lines 1
                                 )))

  ;; Org super agenda setup
  (add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:todo "DONE"
           :name "DONE: good job!"
           :order 1)
          (:todo "REVIEW"
           :name "In review: poke if needed"
           :order 2)
          (:todo "STRT"
           :order 3)
          (:name "Important"
           :priority "A"
           :order 4)
          (:name "Quick Picks"
           :effort< "0:30"
           :order 5)
          (:priority<= "B"
           :scheduled future
           :order 6)))
  ;; Set a different color for super agenda headers & separator
  (custom-set-faces '(org-super-agenda-header ((t (:foreground "dark orange" :weight bold :height: 1.3)))))
  (setq org-super-agenda-header-separator "\n---\n")

  ;; Indent each org mode level a bit more for a better overview
  (setq org-indent-indentation-per-level 4)

  ;; Add REVIEW to doom emacs defaults
  ;; Define the function to add a new keyword
  (defun append-org-todo-keyword (new-keyword)
    "Append a NEW-KEYWORD to the first sequence in `org-todo-keywords`."
    (let ((current-keywords (car org-todo-keywords)))
      (when (eq (car current-keywords) 'sequence)
        (setq org-todo-keywords
              (cons (append current-keywords (list new-keyword))
                    (cdr org-todo-keywords))))))
  ;; Call the (function )
  (append-org-todo-keyword "REVIEW")
  ;; Set a few custom faces
  (setq org-todo-keyword-faces
        (append org-todo-keyword-faces
        '(("REVIEW" . (:foreground "light blue" :weight bold))
          ("DONE" . (:foreground "green" :weight bold :strike-through t)))))

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
  (setq lsp-inlay-hint-enable t
        lsp-file-watch-threshold 3000)
  )

;;  Don't autoformat on C
(setq +format-on-save-enabled-modes
      '(not c-mode  ; Clang-format not good enoug
        ))

(defun get-ollama-models ()
  "Fetch the list of installed Ollama models."
  (let* ((output (shell-command-to-string "ollama list"))
         (lines (split-string output "\n" t))
         models)
    (dolist (line (cdr lines))  ; Skip the first line
      (when (string-match "^\\([^[:space:]]+\\)" line)
        (push (match-string 1 line) models)))
    (nreverse models)))

(use-package! gptel
  :config
  (setq! gptel-api-key (getenv "OPENAI_API_KEY")
         gptel-default-mode 'org-mode
         gptel-model "gpt-4o")
  ;; DeepSeek offers an OpenAI compatible API
  (gptel-make-openai "DeepSeek"       ;Any name you want
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key (getenv "DEEPSEEK_API_KEY")
    :models '("deepseek-chat" "deepseek-coder"))
  (gptel-make-anthropic "Claude"          ;Any name you want
    :stream t                             ;Streaming responses
    :key (getenv "ANTHROPIC_API_KEY"))
  (gptel-make-ollama "Ollama"             ;Any name of your choosing
    :host "localhost:11434"               ;Where it's running
    :stream t                             ;Stream responses
    :models (get-ollama-models))          ;List of models
  (gptel-make-openai "llama-cpp"          ;Any name
    :stream t                             ;Stream responses
    :protocol "http"
    :host "localhost:8081"                ;Llama.cpp server location
    :models '("qwen2.5-coder"))                    ;Any names, doesn't matter for Llama
  )

;; Add aider to path
(let ((local-bin (expand-file-name "~/.local/bin")))
  (setq exec-path (append exec-path (list local-bin)))
  (setenv "PATH" (concat local-bin path-separator (getenv "PATH"))))

(after! indent-bars
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.15)
   indent-bars-pattern "."
   indent-bars-width-frac 0.1
   indent-bars-pad-frac 0.1
   indent-bars-zigzag nil
   indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1) ; blend=1: blend with BG only
   indent-bars-highlight-current-depth '(:blend 0.5) ; pump up the BG blend on current
   indent-bars-display-on-blank-lines t)
  )
