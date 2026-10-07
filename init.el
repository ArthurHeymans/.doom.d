;;; init.el -*- lexical-binding: t; -*-
;; Enabled Doom modules. Run `doom sync' after changing this list.
;; Package recipes and configuration live in config.org.

(doom! :completion
       (corfu +orderless +icons)
       (vertico +icons)

       :ui
       doom
       dashboard
       (emoji +unicode +github +ascii)
       hl-todo
       indent-guides
       modeline
       ophints
       (popup +defaults)
       unicode
       (vc-gutter +pretty)
       vi-tilde-fringe
       window-select

       :editor
       file-templates
       fold
       (format +lsp)
       multiple-cursors
       snippets
       (whitespace +guess +trim)

       :emacs
       (dired +icons +dirvish)
       electric
       (ibuffer +icons)
       tramp
       (undo +tree)
       vc

       :term
       eshell
       vterm
       ghostel

       :checkers
       (syntax +childframe +flymake +icons)
       (spell +flyspell +everywhere +hunspell)
       grammar

       :tools
       debugger
       direnv
       (docker +lsp +tree-sitter)
       editorconfig
       (eval +overlay)
       llm
       lookup
       (lsp +eglot)
       (magit +forge)
       pdf
       tree-sitter
       upload

       :os
       (:if (featurep :system 'macos) macos)
       tty

       :lang
       (cc +lsp +tree-sitter)
       emacs-lisp
       (go +lsp +tree-sitter)
       (graphql +lsp +tree-sitter)
       (json +tree-sitter +lsp)
       (markdown +grip +tree-sitter)
       (nix +lsp +tree-sitter)
       (org +dragndrop +hugo +present +pretty)
       (rust +lsp +tree-sitter)
       (scheme +guile)
       (sh +lsp)
       (web +lsp +tree-sitter)
       (yaml +lsp +tree-sitter)

       :email
       (mu4e +org +gmail +mbsync)

       :app
       calendar
       irc
       (rss +org +youtube)

       :config
       literate
       (default +bindings +smartparens))
