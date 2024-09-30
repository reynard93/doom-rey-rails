;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :completion
       company           ; the ultimate code completion backend
       vertico           ; the search engine of the future

       :ui
       deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       (modeline +light)
       window-select     ; visually switch windows
       ;; nav-flash         ; blink cursor line after big motions
       ophints           ; highlight the region an operation acts on
       ;; (popup +defaults)   ; tame sudden yet inevitable temporary windows
       vc-gutter         ; vcs diff in the fringe
       (treemacs +lsp)      ; a project drawer, like neotree but cooler maybe responsible for ts tripping
       ;; workspaces        ; tab emulation, persistence & separate workspaces
       ;; indent-guides     ; highlighted indent columns

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       ;; file-templates    ; auto-snippets for empty files
       (fold +onsave)              ; (nigh) universal code folding
       format          ; automated prettiness
       multiple-cursors  ; editing in many places at once
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to

       :emacs
       ibuffer         ; interactive buffer management
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       (undo + tree)              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget

       :tools
       (eval +overlay)     ; run code, run (also, repls)
       lookup              ; navigate your code and its documentation
       lsp                 ; M-x vscode
       ;; magit             ; a git porcelain for Emacs ; more used to lg through eee
       ;;ansible
       ;;biblio            ; Writes a PhD for you (citation needed)
       ;; (debugger)          ; FIXME stepping through code, to help you add bugs
       ;;direnv
       docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       ;;gist              ; interacting with github gists
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       tty               ; improve the terminal Emacs experience

       :lang
       emacs-lisp        ; drown in parentheses
       (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       markdown          ; writing docs for people to ignore
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       (web +lsp)               ; the tubes
       (yaml +lsp)
       (json +lsp)              ; At least it ain't XML
       (python +lsp +pyright +pyenv) ; beautiful is better than ugly
       (ruby +rails +lsp +rbenv)     ; Change from rbenv to something else if you use another thing
       (org +roam2)
       rest              ; Emacs as a REST client
       rust                ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;gdscript          ; the language you waited for
       ;;(go +lsp)         ; the hipster dialect
       ;;(graphql +lsp)    ; Give queries a REST
       ;;(haskell +lsp)    ; a language that's lazier than I am
       ;;(java +lsp)       ; the poster child for carpal tunnel syndrome
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;lua               ; one-based indices? one-based indices
       ;;(mu4e +org +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;twitter           ; twitter client https://twitter.com/vnought
       ;;calendar
       ;;emms
       everywhere        ; *leave* Emacs!? You must be joking
       (rss +org)        ; emacs as an RSS reader

       :config
       (default +bindings +smartparens))

(if (file-exists-p (expand-file-name "user/init.el" doom-user-dir))
    (load (expand-file-name "user/init.el" doom-user-dir))
  (progn
    (shell-command "cp ~/.doom.d/user/examples/init.el ~/.doom.d/user/init.el")
    (load (expand-file-name "user/init.el" doom-user-dir))))
