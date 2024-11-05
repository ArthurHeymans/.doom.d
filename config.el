;; -*- lexical-binding: t; -*-

(require 'auth-source)

(defun get-auth-info (&rest params)
  (let ((match (car (apply 'auth-source-search params))))
    (if match
        (let ((secret (plist-get match :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (error "Password not found for %S" params))))

(setq doom-theme 'tsdh-dark
      display-line-numbers-type t
      calendar-week-start-day 1
      doom-font (font-spec :family "FiraCode Nerd Font" :size 15)
      )
(doom/set-frame-opacity 85)

(after! org

(setq org-directory "~/org/")

(setq org-indent-indentation-per-level 4)

(defun append-org-todo-keyword (new-keyword)
  "Append a NEW-KEYWORD to the first sequence in `org-todo-keywords`."
  (let ((current-keywords (car org-todo-keywords)))
    (when (eq (car current-keywords) 'sequence)
      (setq org-todo-keywords
            (cons (append current-keywords (list new-keyword))
                  (cdr org-todo-keywords))))))

;; Call the (function )
(append-org-todo-keyword "REVIEW")

(setq org-todo-keyword-faces
      (append org-todo-keyword-faces
              '(("REVIEW" . (:foreground "light blue" :weight bold))
                ("DONE" . (:foreground "green" :weight bold :strike-through t)))))
(custom-set-faces '(org-drawer ((t (:foreground "grey" :weight bold :height: 1.3)))))

(add-to-list 'org-modules 'ol-gnus)

(setq org-agenda-prefix-format
      '((agenda . "   %i %-12c%?-2t %-12s %-6e")  ; Agenda items: icon, category, time, and extra info, estimate
        (todo .   "   %i %-12:c %-12:t %s")   ; TODO items: icon, category, time (if any), and extra info
        (tags .   "   %i %-12:c %-12:t %s")   ; Tagged items: icon, category, time (if any), and extra info
        (search . "   %i %-12:c %s")))      ; Search results: icon, category, and extra info

(add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)
(setq org-super-agenda-groups
      '((:log t)  ; Automatically named "Log"
        (:name "Schedule"
         :time-grid t)
        (:name "Today"
         :scheduled today)
        (:habit t)
        (:name "Due today"
         :deadline today)
        (:name "Overdue"
         :deadline past)
        (:name "Due soon"
         :deadline future)
        (:name "Scheduled earlier"
         :scheduled past)
        )
      )
(custom-set-faces '(org-super-agenda-header ((t (:foreground "dark orange" :weight bold :height: 1.3)))))
(setq org-super-agenda-header-separator "\n---\n")

)

(after! gnus-agent
  (setq epg-pinentry-mode 'nil)
  )

(setq-hook! 'rust-mode-hook comment-line-break-function nil)

(after! lsp-mode
  (setq lsp-inlay-hint-enable t
        lsp-file-watch-threshold 3000)
  )

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

(defmacro add-openrouter-model (name model-url)
  `(cons ,name  (make-llm-openai-compatible
                     :key (get-auth-info
                           :host "openrouter.ai"
                           :user "apikey")
                     :url "https://openrouter.ai/api/v1"
                     :chat-model ,model-url)))

(require 'json)
(require 'url)
(defun fetch-openrouter-models ()
  (with-current-buffer
      (url-retrieve-synchronously "https://openrouter.ai/api/v1/models")
    (goto-char url-http-end-of-headers)
    (let* ((json-object-type 'alist)
           (json-data (json-read))
           (models (alist-get 'data json-data)))
      (mapcar (lambda (model)
                (cons (alist-get 'name model)
                      (alist-get 'id model)))
              models))))

(after! ellama
  (setopt ellama-keymap-prefix "C-c z")
  (setopt ellama-language "English")
  (require 'llm-openai)
  (setq ellama-sessions-directory "~/.emacs.d/.local/cache/ellama-sessions")
  (setq llm-warn-on-nonfree nil)
  (setq ellama-providers
        `(("gpt4o" . (make-llm-openai
                        :key (get-auth-info
                              :host "api.openai.com"
                              :user "apikey")
                        :chat-model "gpt-4o"))
          ,@(mapcar (lambda (model)
                     (add-openrouter-model (car model) (cdr model)))
                    (fetch-openrouter-models))
          ("deepseek-chat" . (make-llm-openai-compatible
                              :key (get-auth-info
                                    :host "api.deepseek.com"
                                    :user "apikey")
                              :url "https://api.deepseek.com/"
                              :chat-model "deepseek-chat"))))
  (setopt ellama-provider (eval (cdr (assoc "NVIDIA: Llama 3.1 Nemotron 70B Instruct" ellama-providers))))
)

(defun get-ollama-models ()
  "Fetch the list of installed Ollama models."
  (let* ((output (shell-command-to-string "ollama list"))
         (lines (split-string output "\n" t))
         models)
    (dolist (line (cdr lines))  ; Skip the first line
      (when (string-match "^\\([^[:space:]]+\\)" line)
        (push (match-string 1 line) models)))
    (nreverse models)))

(after! gptel
  (setq!
         gptel-default-mode 'org-mode
         gptel-model "gpt-4o")
  ;; DeepSeek offers an OpenAI compatible API
  (gptel-make-openai "DeepSeek"       ;Any name you want
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key (gptel-api-key-from-auth-source "api.deepseek.com")
    :models '("deepseek-chat" "deepseek-coder"))
  (gptel-make-openai "OpenRouter"               ;Any name you want
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key (gptel-api-key-from-auth-source "openrouter.ai")
    :models (mapcar (lambda (model)
                      (cdr model))
               (fetch-openrouter-models)))
  (gptel-make-anthropic "Claude"          ;Any name you want
    :stream t                             ;Streaming responses
    :key (gptel-api-key-from-auth-source "api.anthropic.com"))
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

(let ((local-bin (expand-file-name "~/.local/bin")))
  (setq exec-path (append exec-path (list local-bin)))
  (setenv "PATH" (concat local-bin path-separator (getenv "PATH"))))

(use-package! systemrdl-mode)

(map! :leader
      :desc "Run consult-ripgrep"
      "gr" #'consult-ripgrep)

(after! circe
  (defun my-nickserv-password (server)
    (get-auth-info :user "avph" :machine "irc.libera.chat"))

  (setq circe-network-options
         '(("Libera Chat"
            :nick "avph"
            :sasl-username "avph"
            :sasl-password my-nickserv-password
            :channels ("#flashprog")))))
