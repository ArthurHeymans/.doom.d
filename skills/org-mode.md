# Skill: Working with Org-mode and Org-node Programmatically

Use this skill when an AI agent needs to programmatically create, read, modify,
search, or link org-mode notes, or query the agenda. All operations go through
Emacs in `--batch` mode. The knowledge graph is powered by **org-node** (not
org-roam), backed by **org-mem** for indexing.

## Environment

| Item | Value |
|---|---|
| Org directory | `~/org/` |
| Roam-style notes | `~/org/roam/` |
| Daily notes | `~/org/roam/daily/` |
| Emacs binary | `emacs` (GNU Emacs 30.x) |
| Doom directory | `~/.emacs.d/` |
| Package repos | `~/.emacs.d/.local/straight/repos/` |
| Indexing backend | org-mem (watches `~/org/`) |
| Node system | org-node (org-roam-compatible on-disk format) |

## Node File Format

Every roam-style note follows this template:

```org
:PROPERTIES:
:ID:       <uuid>
:END:
#+title: <Title>
#+filetags: <optional space-separated tags>

<body>
```

- Filenames: `YYYYMMDDHHMMSS-slug.org` (e.g. `20250314235430-llm.org`).
- Slug: lowercase, underscores for spaces.
- Daily notes: `YYYY-MM-DD.org` in `~/org/roam/daily/`.
- Every node **must** have a `:ID:` property (UUID) to be indexed by org-node.
- Links between nodes: `[[id:<uuid>][description]]`.

## Emacs Batch Preamble

Every command in this skill uses this shell invocation:

```bash
emacs --batch \
  -l ~/.emacs.d/early-init.el \
  -l ~/.emacs.d/lisp/doom.el \
  -l ~/.emacs.d/lisp/doom-start.el \
  --eval '(progn <BODY>)'
```

All `<BODY>` elisp in the sections below is wrapped inside that `(progn ...)`.
The preamble loads Doom's core but not user config or packages. Packages and
config variables must be set up explicitly in `<BODY>` as shown.

### Package load-path setup

Required whenever org-node, org-mem, or any other straight.el package is used:

```elisp
(let ((repos-dir (expand-file-name "~/.emacs.d/.local/straight/repos/")))
  (dolist (dir (directory-files repos-dir t "^[^.]"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))
```

### TODO keyword setup

Doom's `(after! org ...)` blocks do not fire in batch mode. The TODO keyword
sequence **must** be set explicitly, or keywords like HOLD, STRT, PROJ, etc.
will not be recognised (they'll be treated as heading text):

```elisp
(setq org-todo-keywords
      '((sequence
         "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)"
         "WAIT(w)" "HOLD(h)" "IDEA(i)"
         "|"
         "DONE(d)" "KILL(k)" "REVIEW")
        (sequence
         "[ ](T)" "[-](S)" "[?](W)"
         "|"
         "[X](D)")))
```

### Agenda file setup

Agenda files are built dynamically: every `.org` file under `~/org/` whose
`#+filetags:` line contains `todo`, plus four fixed calendar files. Replicate
this in batch mode with:

```elisp
(setq org-directory "~/org/")

(defun roam-extra:todo-files ()
  "Return org files under `org-directory' with a todo filetag."
  (let (files)
    (dolist (f (directory-files-recursively
                (expand-file-name org-directory) "\\.org$"))
      (with-temp-buffer
        (insert-file-contents f nil 0 2048)
        (when (re-search-forward "^#\\+filetags:.*\\btodo\\b" nil t)
          (push f files))))
    (nreverse files)))

(defvar default-org-agenda-files
  '("9e_work.org" "cal.org" "aheymans_cal.org" "gedeelde_cal.org"))

(setq org-agenda-files
      (append (roam-extra:todo-files)
              (mapcar (lambda (f) (expand-file-name f org-directory))
                      default-org-agenda-files)))
```

### Org-node / org-mem initialization

Required for graph queries (node search, backlinks). Add **after** the
load-path setup:

```elisp
(require 'org-id)
(require 'org-mem)
(require 'org-node)
(setq org-mem-watch-dirs (list (expand-file-name "~/org/")))
(org-id-locations-load)
(org-mem-updater-mode 1)
(org-node-cache-mode 1)
(org-node-cache-ensure t t)
```

`org-node-cache-ensure` prints "Org-node waiting for org-mem..." several
times while indexing. This is normal and takes a few seconds.

---

## Reading Notes

### Read a file's org structure

```elisp
(require 'org)
(find-file (expand-file-name "~/org/roam/20250314235430-llm.org"))
(org-mode)
(message "Title: %s" (org-get-title))
(message "Tags: %s" (org-get-tags))
(message "ID: %s" (org-entry-get (point-min) "ID"))
(org-map-entries
 (lambda ()
   (message "  L%d [%s] %s"
            (org-current-level)
            (or (org-get-todo-state) "")
            (org-get-heading t t t t)))
 nil 'file)
```

### Export a node to plain text (for AI consumption)

```elisp
(require 'org)
(require 'ox)
(require 'ox-ascii)
(find-file (expand-file-name "~/org/roam/SOME_FILE.org"))
(org-mode)
(princ (org-export-as 'ascii nil nil t))
```

---

## Creating Notes

### Create a new node

```elisp
(require 'org)
(require 'org-id)
(let* ((new-id (org-id-new))
       (timestamp (format-time-string "%Y%m%d%H%M%S"))
       (slug "my_topic")
       (title "My Topic")
       (filename (expand-file-name
                  (concat timestamp "-" slug ".org")
                  "~/org/roam/")))
  (with-temp-file filename
    (insert ":PROPERTIES:\n")
    (insert (format ":ID:       %s\n" new-id))
    (insert ":END:\n")
    (insert (format "#+title: %s\n\n" title)))
  (message "%s" new-id))
```

### Create a daily note

```elisp
(require 'org)
(require 'org-id)
(let* ((daily-dir (expand-file-name "~/org/roam/daily/"))
       (date-str (format-time-string "%Y-%m-%d"))
       (filename (expand-file-name (concat date-str ".org") daily-dir)))
  (unless (file-exists-p filename)
    (with-temp-file filename
      (insert ":PROPERTIES:\n")
      (insert (format ":ID:       %s\n" (org-id-new)))
      (insert ":END:\n")
      (insert (format "#+title: %s\n\n" date-str)))
    (message "Created: %s" filename)))
```

### Create a node and link it to an existing one

```elisp
(require 'org)
(require 'org-id)
(let* ((target-id "8d1ea5cb-cba2-4d57-951e-961d55a6b394")
       (target-title "llm")
       (new-id (org-id-new))
       (timestamp (format-time-string "%Y%m%d%H%M%S"))
       (slug "my_new_note")
       (filename (expand-file-name
                  (concat timestamp "-" slug ".org")
                  "~/org/roam/")))
  (with-temp-file filename
    (insert ":PROPERTIES:\n")
    (insert (format ":ID:       %s\n" new-id))
    (insert ":END:\n")
    (insert "#+title: My New Note\n\n")
    (insert (format "Related to [[id:%s][%s]].\n" target-id target-title)))
  (message "Created %s with ID %s" filename new-id))
```

---

## Modifying Notes

### Append a heading to an existing node

```elisp
(require 'org)
(find-file (expand-file-name "~/org/roam/SOME_FILE.org"))
(org-mode)
(goto-char (point-max))
(insert "\n* New heading added by agent\n\n")
(insert "Content goes here.\n")
(save-buffer)
```

### Change a TODO state (by heading text)

Requires the TODO keyword setup from the preamble.

```elisp
(require 'org)
;; <TODO-KEYWORD-SETUP>
(find-file (expand-file-name "~/org/roam/SOME_FILE.org"))
(org-mode)
(goto-char (point-min))
(re-search-forward "Heading text to find")
(org-back-to-heading)
(org-todo "DONE")
(save-buffer)
```

### Change a TODO state (by org-id via org-node)

More reliable than text search. Requires org-node initialization and TODO
keyword setup.

```elisp
;; <LOAD-PATH-SETUP>
(require 'org)
(require 'org-id)
(require 'org-mem)
(require 'org-node)
;; <TODO-KEYWORD-SETUP>
;; <ORG-NODE-INIT>

(let* ((target-id "SOME-UUID")
       (node (org-mem-entry-by-id target-id)))
  (when node
    (find-file (expand-file-name (org-mem-entry-file node)))
    (org-mode)
    (goto-char (org-mem-entry-pos node))
    ;; For subtree nodes, ensure we're at the heading
    (when (> (org-mem-entry-level node) 0)
      (org-back-to-heading))
    (org-todo "DONE")
    (save-buffer)))
```

### Schedule or set deadline

```elisp
(require 'org)
;; <TODO-KEYWORD-SETUP>
(find-file (expand-file-name "~/org/roam/SOME_FILE.org"))
(org-mode)
(goto-char (point-min))
(re-search-forward "Target heading")
(org-back-to-heading)
(org-schedule nil "2026-04-15")   ;; or: (org-deadline nil "2026-04-20")
(save-buffer)
```

### Set properties, tags, aliases on a heading

```elisp
(require 'org)
(require 'org-id)
(find-file (expand-file-name "~/org/roam/SOME_FILE.org"))
(org-mode)
(goto-char (point-min))
(re-search-forward "Target heading")
(org-back-to-heading)
(org-id-get-create)                                         ;; add ID if missing
(org-set-property "ROAM_ALIASES" "\"Alias One\" \"Alias Two\"")
(org-set-property "ROAM_REFS" "https://example.com")
(org-set-property "EFFORT" "1:30")
(org-set-tags ":tag1:tag2:")
(save-buffer)
```

---

## Agenda Queries

All agenda queries require the TODO keyword setup and the agenda file setup
from the preamble. The pattern is always `org-map-entries` over `'agenda`
scope, which iterates all headings matching a match string across every file
in `org-agenda-files`.

### Weekly agenda view (calendar + scheduled items)

Produces the same textual output as pressing `a` in the agenda dispatcher:

```elisp
(require 'org)
(require 'org-agenda)
;; <TODO-KEYWORD-SETUP>
;; <AGENDA-FILE-SETUP>
(org-batch-agenda "a")
```

### Global TODO list

Same as pressing `t` in the agenda dispatcher:

```elisp
(require 'org)
(require 'org-agenda)
;; <TODO-KEYWORD-SETUP>
;; <AGENDA-FILE-SETUP>
(org-batch-agenda "t")
```

### List all active TODO items (structured)

```elisp
(require 'org)
(require 'org-agenda)
;; <TODO-KEYWORD-SETUP>
;; <AGENDA-FILE-SETUP>
(org-map-entries
 (lambda ()
   (let ((todo (org-get-todo-state))
         (heading (org-get-heading t t t t))
         (scheduled (org-entry-get nil "SCHEDULED"))
         (deadline (org-entry-get nil "DEADLINE"))
         (priority (org-entry-get nil "PRIORITY"))
         (effort (org-entry-get nil "EFFORT"))
         (tags (org-get-tags))
         (file (file-name-nondirectory (buffer-file-name))))
     (message "[%s]%s %s  (file: %s, sched: %s, deadline: %s, effort: %s, tags: %s)"
              todo
              (if priority (format " [#%s]" priority) "")
              heading file
              (or scheduled "-")
              (or deadline "-")
              (or effort "-")
              (if tags (mapconcat #'identity tags ":") "-"))))
 "TODO={TODO\\|NEXT\\|STRT\\|HOLD\\|WAIT\\|PROJ\\|LOOP\\|IDEA}" 'agenda)
```

### Unscheduled TODOs (no SCHEDULED and no DEADLINE)

These are items that need attention -- they have a TODO state but no date
attached, so they will never appear in a calendar view:

```elisp
(require 'org)
(require 'org-agenda)
;; <TODO-KEYWORD-SETUP>
;; <AGENDA-FILE-SETUP>
(let ((count 0))
  (org-map-entries
   (lambda ()
     (let ((scheduled (org-entry-get nil "SCHEDULED"))
           (deadline (org-entry-get nil "DEADLINE")))
       (when (and (not scheduled) (not deadline))
         (setq count (1+ count))
         (message "[%s] %s  (file: %s, tags: %s)"
                  (org-get-todo-state)
                  (org-get-heading t t t t)
                  (file-name-nondirectory (buffer-file-name))
                  (let ((tags (org-get-tags)))
                    (if tags (mapconcat #'identity tags ":") "-"))))))
   "TODO={TODO\\|NEXT\\|STRT\\|WAIT\\|PROJ\\|IDEA}" 'agenda)
  (message "Total unscheduled: %d" count))
```

### Overdue items (scheduled in the past, still open)

Items that were scheduled before today but are not yet done. Excludes
repeating tasks (those with `.+` or `++` in the timestamp):

```elisp
(require 'org)
(require 'org-agenda)
;; <TODO-KEYWORD-SETUP>
;; <AGENDA-FILE-SETUP>
(let ((count 0))
  (org-map-entries
   (lambda ()
     (let ((scheduled (org-entry-get nil "SCHEDULED")))
       (when (and scheduled
                  (not (string-match-p "\\." scheduled))
                  (time-less-p (org-time-string-to-time scheduled)
                               (current-time)))
         (setq count (1+ count))
         (message "[%s] %s  (sched: %s, file: %s)"
                  (org-get-todo-state)
                  (org-get-heading t t t t)
                  scheduled
                  (file-name-nondirectory (buffer-file-name))))))
   "TODO={TODO\\|NEXT\\|STRT\\|WAIT\\|PROJ}" 'agenda)
  (message "Total overdue: %d" count))
```

### Items with upcoming deadlines

```elisp
(require 'org)
(require 'org-agenda)
;; <TODO-KEYWORD-SETUP>
;; <AGENDA-FILE-SETUP>
(org-map-entries
 (lambda ()
   (let ((deadline (org-entry-get nil "DEADLINE")))
     (when deadline
       (message "[%s] %s  (deadline: %s, file: %s)"
                (org-get-todo-state)
                (org-get-heading t t t t)
                deadline
                (file-name-nondirectory (buffer-file-name))))))
 "TODO={TODO\\|NEXT\\|STRT\\|WAIT\\|PROJ}" 'agenda)
```

### HOLD items (paused tasks to review)

```elisp
(require 'org)
(require 'org-agenda)
;; <TODO-KEYWORD-SETUP>
;; <AGENDA-FILE-SETUP>
(org-map-entries
 (lambda ()
   (message "[HOLD] %s  (file: %s)"
            (org-get-heading t t t t)
            (file-name-nondirectory (buffer-file-name))))
 "TODO=\"HOLD\"" 'agenda)
```

---

## Graph Queries (org-node / org-mem)

These require the full org-node initialization from the preamble.

### List all nodes

```elisp
;; <LOAD-PATH-SETUP>
;; <ORG-NODE-INIT>
(maphash (lambda (id entry)
           (message "%s | %s | %s"
                    (org-mem-entry-id entry)
                    (org-mem-entry-title entry)
                    (file-name-nondirectory (org-mem-entry-file entry))))
         org-mem--id<>entry)
```

### Search nodes by title

```elisp
;; <LOAD-PATH-SETUP>
;; <ORG-NODE-INIT>
(maphash (lambda (id entry)
           (when (string-match-p "search-term"
                                 (downcase (org-mem-entry-title entry)))
             (message "%s | %s | %s"
                      (org-mem-entry-id entry)
                      (org-mem-entry-title entry)
                      (org-mem-entry-file entry))))
         org-mem--id<>entry)
```

### Get a node by ID

```elisp
;; <LOAD-PATH-SETUP>
;; <ORG-NODE-INIT>
(let ((node (org-mem-entry-by-id "8d1ea5cb-cba2-4d57-951e-961d55a6b394")))
  (when node
    (message "Title: %s" (org-mem-entry-title node))
    (message "File: %s" (org-mem-entry-file node))
    (message "Tags: %s" (org-mem-entry-tags node))
    (message "TODO: %s" (org-mem-entry-todo-state node))
    (message "Level: %d" (org-mem-entry-level node))
    (message "Properties: %s" (org-mem-entry-properties node))))
```

### Get backlinks to a node

```elisp
;; <LOAD-PATH-SETUP>
;; <ORG-NODE-INIT>
(let ((node (org-mem-entry-by-id "TARGET-UUID")))
  (when node
    (let ((backlinks (org-mem-id-links-to-entry node)))
      (if backlinks
          (dolist (bl backlinks)
            (message "Backlink: %s" bl))
        (message "No backlinks")))))
```

### Get all known files

```elisp
;; <LOAD-PATH-SETUP>
;; <ORG-NODE-INIT>
(dolist (f (org-mem-all-files))
  (message "%s" f))
```

---

## Org-mem / Org-node API Reference

### Node accessors (take an entry struct)

| Function | Returns |
|---|---|
| `(org-mem-entry-id ENTRY)` | UUID string |
| `(org-mem-entry-title ENTRY)` | Title string |
| `(org-mem-entry-file ENTRY)` | File path |
| `(org-mem-entry-level ENTRY)` | Heading level (0 = file-level) |
| `(org-mem-entry-pos ENTRY)` | Character position |
| `(org-mem-entry-lnum ENTRY)` | Line number |
| `(org-mem-entry-tags ENTRY)` | All tags (inherited + local) |
| `(org-mem-entry-tags-local ENTRY)` | Local tags only |
| `(org-mem-entry-tags-inherited ENTRY)` | Inherited tags only |
| `(org-mem-entry-todo-state ENTRY)` | TODO keyword or nil |
| `(org-mem-entry-scheduled ENTRY)` | Scheduled timestamp or nil |
| `(org-mem-entry-deadline ENTRY)` | Deadline timestamp or nil |
| `(org-mem-entry-closed ENTRY)` | Closed timestamp or nil |
| `(org-mem-entry-priority ENTRY)` | Priority character or nil |
| `(org-mem-entry-properties ENTRY)` | Alist of properties |
| `(org-mem-entry-olpath ENTRY)` | Outline path list |
| `(org-mem-entry-roam-refs ENTRY)` | ROAM_REFS list |
| `(org-mem-entry-roam-aliases ENTRY)` | ROAM_ALIASES list |
| `(org-mem-file-title ENTRY)` | Title of file containing node |

### Lookup functions

| Function | Returns |
|---|---|
| `(org-mem-entry-by-id ID)` | Entry struct or nil |
| `(org-mem-all-files)` | List of all known file paths |
| `(org-mem-id-links-to-entry ENTRY)` | List of link structs to this node |
| `(org-mem-roam-reflinks-to-entry ENTRY)` | List of reflink structs |

### Cache table

All nodes live in hash table `org-mem--id<>entry` (keys = ID strings,
values = entry structs). Iterate with `maphash`.

### TODO keyword reference

| Active states | `TODO` `PROJ` `LOOP` `STRT` `WAIT` `HOLD` `IDEA` |
|---|---|
| **Done states** | **`DONE` `KILL` `REVIEW`** |
| **Checkbox states** | **`[ ]` `[-]` `[?]` `[X]`** |

---

## Tangle

```bash
emacs --batch -l org \
  --eval '(org-babel-tangle-file "config.org")'
```

---

## Important Notes

- **org-node replaces org-roam** in this setup. Do not use org-roam commands.
- **org-mem scans saved files on disk.** New/modified nodes appear in the index only after save.
- **TODO keywords must be set explicitly in batch mode.** Without it, HOLD/STRT/PROJ/etc. are not recognised and get mangled into heading text.
- **Agenda files must be set explicitly in batch mode.** The `roam-extra:update-todo-files` advice only fires in interactive Emacs.
- **Batch mode warnings are normal.** "Org-mem will be very slow unless compiled" and "Org-node waiting for org-mem..." are expected and harmless.
- **Do not edit `config.el` directly.** Modify `config.org` and re-tangle.
- **`org-log-done` is `'time`.** Completing a TODO to DONE automatically adds a `CLOSED: [timestamp]` line and logs to the `LOGBOOK` drawer.
