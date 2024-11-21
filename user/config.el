;; MOST IMPORTANT CONFIG - Set your project folders
(setq projectile-project-search-path '("~/Desktop/formflow-mono", "~/org"))

(setq user-full-name "Reynard Lee Yu Ling"
      user-mail-address "reynard.lee@thoughtworks.com.sg")

;; If you use macos with rbenv on homebrew, add it, uncomment it
;; (setq rbenv-executable "/usr/local/opt/rbenv/bin/rbenv") ;; Rosetta (intel emulation)
;; (setq rbenv-executable "/opt/homebrew/bin/rbenv") ;; Arm (normal brew)

;; Set your theme
(setq doom-theme 'doom-one)

(setq doom-font (font-spec :family "BerkeleyMono Nerd Font" :size 14 :weight 'bold) ;; You can change to regular if you prefer
      doom-variable-pitch-font (font-spec :family "BerkeleyMono Nerd Font" :size 14))
;; My recommendation is JetBrains Mono.  Use M-x reload-user-settings to see the font change.
;; (setq doom-font (font-spec :family "JetBrains Mono" :size 13 :weight 'bold) ;; You can change to regular if you prefer
;;       doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 13))

(setq display-line-numbers-type t)

(setq mac-option-modifier 'super)
;; TERMINAL MANAGEMENT STUFF ;;
;; Predefined commands
;; You can switch to any terminal with SPC l
;; You can search the commands with SPC o t
;; You can send any text to any terminal by selecting and pressing SPC l
;; You can quickly execute a define command with SPC j + the keybinding you defined.

(require 'which-key) ;; Needed for which-key to work
(after! which-key
  ;;                         | Name              | command                      | Keybinding |
  (+add-command-to-term-list '("Docker Compose" . "docker-compose up") "u") ;; SPC j u

  ;; Example asking something
  (+add-command-to-term-list '("Add Yarn Package" . (concat "yarn add " (read-string "Package name: "))) "ya") ;; SPC j y a

  ;; Example of dynamic command (using buffer name as example)
  (+add-command-to-term-list '("Rspec on file" . (concat "bundle exec rspec " (buffer-file-name))) "sv") ;; SPC j s v
  (+add-command-to-term-list '("Rspec on line" . (concat "bundle exec rspec " (buffer-file-name) ":" (format "%s" (line-number-at-pos)))) "ss") ;; SPC j s s

  ;; Getting text and executing a command
  ;; (+add-command-to-term-list '("Brownie Test" . (concat "brownie test -k " (save-excursion (search-backward "def test_") (forward-word 2) (thing-at-point 'symbol t)))) "bt") ;; SPC j b t

  ;; Running scripts of a specific folder
  ;; (+add-command-to-term-list '("Brownie Run Script" . (concat "brownie run " (read-file-name "scripts/") " " (read-string "Extra parameters: " nil "commands"))) "br") ;; SPC j b r

  ;; Creating terminal layouts: SPC T
  ;; It will create a new workspace with all terminals listed
  ;;                         | Layout Name    | Commands to execute                |
  (+add-layout-to-term-list '("Rails" . '("rails console" "rails server" nil)))
  (+add-layout-to-term-list '("React" . '("yarn start" nil)))
  (+add-layout-to-term-list '("Next JS" . '("yarn dev" "cowsay 'Have an nice work'" nil)))
  )

;; Harpoon separate by branch? (Harpoon leader key: ,)
(setq harpoon-separate-by-branch nil)

;; Ignoring some folders on search
(after! projectile
  (setq projectile-globally-ignored-directories '("reynardtw" "flow-typed" "node_modules" "~/.emacs.d/.local/" ".idea" ".vscode" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".ccls-cache" ".cache" ".clangd")))

;; Add your custom searches (in rails folders)
(after! projectile-rails
  (doom-emacs-on-rails-add-custom-projectile-finder "services" "app/services/"  "\\(.+\\)\\.rb$" "app/services/${filename}.rb" "rt")
  (doom-emacs-on-rails-add-custom-projectile-finder "admin" "app/admin/"  "\\(.+\\)\\.rb$" "app/admin/${filename}.rb" "rt")
  (doom-emacs-on-rails-add-custom-projectile-finder "contracts" "app/contracts/"  "\\(.+\\)\\.rb$" "app/contracts/${filename}.rb" "rq"))

;; fix your identation level for stuff?
(setq js-indent-level 2)
(setq ts-indent-level 2)
(setq typescript-indent-level 2)
(setq ruby-indent-level 2)
(setq standard-indent 2)

;; Rubocop on current file command
(setq rubocop-on-current-file-command "bundle exec rubocop -A ") ;; SPC =

;; Disable Rubocop or any other lint if you want.  Linter list on: SPC h v flycheck-checkers
;; (add-hook 'ruby-mode-hook
;;  (lambda ()
;;    (setq-local flychech-checker nil)
;;    (setq-local flycheck-disabled-checkers '(ruby-reek lsp ruby-rubylint ruby-rubocop))) 1000)

;; Do you use minitest instead o rspec
;; (use-minitest "_test")

;; Use sorbet instead solargraph?
;; (after! lsp-mode
;;   (setq lsp-disabled-clients '(ruby-ls solargraph))
;;   (setq lsp-sorbet-use-bundler t))

;; Stop some boring warnings
(setq warning-minimum-level :emergency)

;; Start projectile with magit, uncomment below:
;; (after! projectile
;;   (defun open-projectile-with-magit (&optional DIRECTORY CACHE)
;;     (interactive)
;;     (magit-status DIRECTORY)
;;     (if (fboundp 'magit-fetch-from-upstream)
;;         (call-interactively #'magit-fetch-from-upstream)
;;       (call-interactively #'magit-fetch-current)))
;;   (setq +workspaces-switch-project-function #'open-projectile-with-magit))

;; Build your own file switches here
;; (after! projectile-rails
;;   ;; Example: switch from app/contracts/{resource}.rb to app/services/{resource} and vice-versa
;;   (defun projectile-rails-find-contract ()
;;     "Switch from contract to service and vice versa."
;;     (interactive)
;;     (if (string-match-p "app/contracts" (buffer-file-name)) (find-file (replace-regexp-in-string "contract" "service" (replace-regexp-in-string "_contracts" "_services" (buffer-file-name))))
;;       (find-file (replace-regexp-in-string "service" "contract" (replace-regexp-in-string "_services" "_contracts" (buffer-file-name))))))
;;   (map! :leader "rQ" #'projectile-rails-find-contract) ;; Uncomment to bind to SPC r q
;;   )


;; Want to use DOCKER?
;; First, configure you docker variables:

;; (load (expand-file-name "modules/docker.el" doom-private-dir))

;; (setq ruby-docker-compose-command "docker-compose") ;; You docker-compose command (tip: you can use "cd ../; docker-compose")
;; (setq ruby-docker-rails-server-command "up") ;' To start rails server with SPC r R (docker-compose is implicit)
;; (setq ruby-docker-rails-console-command "run {{container}} rails console") ;; to start rails console (docker-compose is implicit)

;; (setq ruby-docker-rubocop-command "run {{container}} rubocop -a ") ;; Command to run rubocop on current file with SPC =
;; (setq ruby-docker-compose-cwd "/app/")
;; (setq ruby-docker-compose-container "web")

;; Tip here:  You can use M-x rbenv-use and select one version that has solargraph.  You can also install with apt or brew.
;; (setq ruby-docker-disable-solargraph nil) ;; If you want to disable solargraph, change to t.  PS:  You can use solargraph by removing .ruby-version of your project and using from rbenv.
;; (use-ruby-docker)
;;
;; End Docker

;; Change Javascript autoformat
(setq-hook! 'rjsx-mode-hook +format-with 'prettier)
(setq-hook! 'typescript-tsx-mode-hook +format-with 'prettier)
(setq-hook! 'typescript-mode-hook +format-with 'prettier)
;; read My new keybinds don't https://github.com/doomemacs/doomemacs/blob/35865ef5e89442e3809b8095199977053dd4210f/docs/faq.org#my-new-keybinds-dont-work

(defun yabai-move-on-error (direction move-fn)
  (interactive)
  (condition-case nil
      (funcall move-fn)
    (user-error (start-process "yabai" nil "yabai" "-m" "window" "--focus" direction))))

(defun yabai-window-left ()
  (interactive)
  (yabai-move-on-error "west" #'windmove-left))

(defun yabai-window-right ()
  (interactive)
  (yabai-move-on-error "east" #'windmove-right))

(defun yabai-window-up ()
  (interactive)
  (yabai-move-on-error "north" #'windmove-up))

(defun yabai-window-down ()
  (interactive)
  (yabai-move-on-error "south" #'windmove-down))

(defun yabai-fullscreen ()
  (interactive)
  (start-process "yabai" nil "yabai" "-m" "window" "--toggle" "zoom-fullscreen"))

(setq mac-option-modifier 'alt)
(map! "A-h" #'yabai-window-left
      "A-j" #'yabai-window-down
      "A-k" #'yabai-window-up
      "A-l" #'yabai-window-right
      "A-f" #'yabai-fullscreen)

(map! :map evil-normal-state-map
      :ngivo "C-h" nil
      :ngivo "C-l" nil)

(setq auto-save-default t)
;;org-mode
(setq org-superstar-headline-bullets-list '("✿" "✸" "◉" "⁖" "○" ))
(setq org-log-done 'time)
(setq org-cycle-include-plain-lists t)
(setq org-export-coding-system 'utf-8)
(setq org-src-fontify-natively t)
(setq org-html-doctype "html5")
(setq org-html-xml-declaration nil)
(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "†")("#+END_SRC" . "†")("#+begin_src" . "†") ("#+end_src" . "†") (">=" . "≥")("=>" . "⇨")))
(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)
(setq org-adapt-indentation nil)

;; elfeed
(setq elfeed-db-directory "~/org/elfeed")
(setq rmh-elfeed-org-files (list "~/org/elfeed/elfeed.org"))
(add-hook! 'elfeed-search-mode-hook #'elfeed-update)

;; from christiantietze prevent compilation buffer from being killed
;; (defmacro ct/display-in-side-window (side)
;;   `(defun ,(intern (concat "display-in-side-window--" (symbol-name side))) (&optional buffer)
;;      (interactive "b")
;;      (when-let* ((buffer (or buffer (current-buffer)))
;;                  (display-buffer-overriding-action '((display-buffer-in-side-window)
;;                                                      (dedicated . t)
;;                                                      (side . ,side)
;;                                                      (window-parameters . ((no-delete-other-windows . t))))))
;;        (display-buffer buffer))))
;; creating the functions
;; (ct/display-in-side-window bottom)
;; (ct/display-in-side-window left)
;; (ct/display-in-side-window right)
;; https://discourse.doomemacs.org/t/how-to-re-bind-keys/56

(use-package! embark
  :bind
  (("C-c a" . embark-act)))

(use-package! unity
  :defer t
  :config (add-hook 'after-init-hook #'unity-mode))

;; https://silosneeded.com/en/2024/04/configuring-dape
(use-package! dape
  :init
  (setq dape-buffer-window-arrangement 'right)
  :config
  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook 'kill-buffer)
  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root)
  )

;; https://raine.ing/posts/emacs-dape/
(map! :map dape-mode-map
      :leader
      :prefix ("d" . "dape")
      :desc "dape hydra" "h" #'hydra-dape/body

      :desc "dape debug"   "s" #'dape
      :desc "dape quit"    "q" #'dape-quit
      :desc "dape restart" "r" #'dape-restart

      :desc "dape breakpoint toggle"     "b" #'dape-breakpoint-toggle
      :desc "dape breakpoint remove all" "B" #'dape-breakpoint-remove-all
      :desc "dape breakpoint log"        "l" #'dape-breakpoint-log

      :desc "dape continue" "c" #'dape-continue
      :desc "dape next"     "n" #'dape-next
      :desc "dape step in"  "i" #'dape-step-in
      :desc "dape step out" "o" #'dape-step-out

      :desc "dape eval" "e" #'dape-evaluate-expression)

(after! hydra

  (defhydra hydra-dape (:color pink :hint nil)
    "
^Dape Hydra^
------------------------------------------------
_n_: Next       _e_: Eval    _Q_: Disconnect
_i_: Step In
_o_: Step Out
_c_: Continue
_r_: Restart

"
    ("n" #'dape-next)
    ("i" #'dape-step-in)
    ("o" #'dape-step-out)
    ("c" #'dape-continue)
    ("e" #'dape-evaluate-expression)
    ("r" #'dape-restart)
    ("q" nil "Quit" :color blue)
    ("Q" #'dape-quit :color blue))
  )


;; # 5678
(add-to-list 'dape-configs
             `(ruby-rdbg
               modes (ruby-ts-mode ruby-mode)
               host "127.0.0.1"
               port 5680
               command "rdbg"
               ))

;; below still does not work
;; found the port by running the tests just once
(add-to-list 'dape-configs
             `(rspec-rdbg
               modes (ruby-ts-mode ruby-mode)
               port 5680
               prefix-local "~/Desktop/formflow-mono/packages/app"
               command "rdbg -A"
               :cwd "~/Desktop/formflow-mono/packages/app"
               ))

(use-package! ready-player
  :init
  (when (memq system-type '(darwin))
    (set-fontset-font t nil "SF Pro Display" nil 'append))
  :config
  (ready-player-add-to-auto-mode-alist))



;; ──────────────────────────────── Transparency ───────────────────────────────
(set-frame-parameter (selected-frame) 'alpha 96)
(add-to-list 'default-frame-alist '(alpha . 96))

;; (set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;; (set-frame-parameter (selected-frame) 'alpha <both>)

;; Use the following snippet after you’ve set the alpha value
(defun toggle-transparency ()
  "Crave for transparency!"
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         96 '(100 . 100)))))

(use-package! consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  :bind
  (("C-c r" . consult-ripgrep)))

(defun org-roam-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult_ripgrep_args "rg --null --ignore-case --type org --line-buffered --color=never --max-columns=500 --no-heading --line-number"))
    (consult-ripgrep org-roam-directory)))
(global-set-key (kbd "C-c f s") 'org-roam-search)

;; https://elpa.gnu.org/packages/vertico-posframe.html
;; would not work well with vertico-multiform dk what's that
(use-package! vertico-posframe
  :init
  (vertico-posframe-mode 1))

;; make fonts look better with anti-aliasing
(setq mac-allow-anti-aliasing t)

;; Integrate with MacOS clipboard
(setq select-enable-clipboard t)

;; https://sunyour.org/post/telega.el%E9%98%B6%E6%AE%B5%E6%80%A7%E9%85%8D%E7%BD%AE%E5%B0%8F%E7%BB%93/
;; https://blog.liuliancao.com/posts/emacs-xia-telega-de-anzhuang-he-shiyong/
;; Requires installing tdlib
;; https://tdlib.github.io/td/build.html?language=Other
;; (use-package! telega
;;   :commands (telega)
;;   :defer t
;;   :bind ("C-c t" . #'telega)
;;   :init
;;   (unless (display-graphic-p) (setq telega-use-images nil))
;;   (setq telega-server-libs-prefix "~/td/tdlib")
;;   :hook
;;   ('telega-root-mode . #'evil-emacs-state)
;;   ('telega-chat-mode . #'evil-emacs-state)
;;   ('telega-chat-mode . #'yas-minor-mode)
;;   ('telega-chat-mode . (lambda ()
;;                          (set-company-backend! 'telega-chat-mode
;;                            (append '(telega-company-emoji
;;                                      telega-company-username
;;                                      telega-company-hashtag)
;;                                    (when (telega-chat-bot-p telega-chatbuf--chat)
;;                                      '(telega-company-botcmd))))
;;                          (company-mode 1)))
;;   ('telega-chat-pre-message . #'telega-msg-ignore-blocked-sender)
;;   :config
;;   ;; what are these proxies for?
;;   ;; (setq telega-proxies
;;   ;;       (list '(:server "127.0.0.1" :port 1086 :enable t
;;   ;;               :type (:@type "proxyTypeSocks5"))))
;;   (set-evil-initial-state! '(telega-root-mode telega-chat-mode) 'emacs)
;;   (setq telega-chat-reply-prompt "<<< "
;;         telega-chat-edit-prompt "+++ "
;;         telega-chat-use-markdown-version nil
;;         telega-animation-play-inline t
;;         telega-emoji-use-images nil
;;         telega-sticker-set-download t)
;;   (set-popup-rule! "^\\*Telega Root"
;;                    :side 'right :size 100 :quit nil :modeline t)
;;   (set-popup-rule! "^◀\\(\\[\\|<\\|{\\).*\\(\\]\\|>\\|}\\)"
;;                    :side 'right :size 100 :quit nil :modeline t)
;;   (telega-mode-line-mode 1)
;;   )

(setq dape-configs-adapter-dir (file-name-as-directory (concat user-emacs-directory "debug-adapters")))

;; https://duncanlock.net/blog/2023/05/27/debugging-with-an-existing-browser-instance-or-brave-in-vscode/
(setq dape-configs-port 9229)
;; (add-to-list 'dape-configs
;;              `(js-debug-chrome
;;                modes (js-mode js-ts-mode)
;;                ;; command "node"
;;                ;; command-cwd ,(file-name-concat dape-configs-adapter-dir "js-debug" "dist")
;;                ;; command-args ("src/dapDebugServer.js" ,(format "%d" dape-configs-port))
;;                port dape-configs-port
;;                :type "chrome"
;;                :runtimeExecutable "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser.exe"
;;                :trace t
;;                :url ,(lambda ()
;;                        (read-string "Url: "
;;                                     "http://localhost:3000"))
;;                :webRoot dape-cwd-fn
;;                :outputCapture "console"))
(setq dape-debug t)
(setq dape-stack-trace-levels 4)
(add-to-list 'dape-configs
	     `(js-debug
	       modes (js-mode js-ts-mode)
               command "node"
               command-cwd ,(file-name-concat dape-configs-adapter-dir "js-debug")
               command-args ("src/dapDebugServer.js" ,(format "%d" dape-configs-port))
	       port dape-configs-port
               :name "DEBUG"
               ;; :sourceMaps nil
               ;; :pauseForSourceMap nil
               ;; :userDataDir nil ;; REMOVED this and finally start
	       :type "pwa-chrome"
               :trace nil
               :resolveSourceMapLocations ["!${workspaceFolder}/packages/app/node_modules/**",
                                           "!/node_modules/**"]
               ;; :skipFiles "[\"**/node_modules/**\"]" ;; what is the difference between skipFIles and resolveSourceMap
	       :url ,(lambda ()
		       (read-string "Url: "
				    "http://applicant.localhost:3000")) ;; get from RAILS_host
	       :webRoot ,(lambda ()
			   (read-string "Root: "
				        (funcall dape-cwd-fn)))
                                        ;:outputCapture "console"
               ))

;; from noteYoda Section
;; https://dotdoom.rgoswami.me/config.html#text-3
(use-package! org-ref
  :init
  (with-eval-after-load 'ox
    (defun my/org-ref-process-buffer--html (backend)
      "Preprocess `org-ref' citations to HTML format.

  Do this only if the export backend is `html' or a derivative of
  that."
      ;; `ox-hugo' is derived indirectly from `ox-html'.
      ;; ox-hugo <- ox-blackfriday <- ox-md <- ox-html
      (when (org-export-derived-backend-p backend 'html)
        (org-ref-process-buffer 'html)))
    (add-to-list 'org-export-before-parsing-hook #'my/org-ref-process-buffer--html))
  :config
  (setq
   org-ref-completion-library 'org-ref-ivy-cite
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
   ;; bibtex-completion-bibliography (list (concat (getenv "HOME") "/GDrive/zotLib.bib")
                                        ;; )
   bibtex-completion-notes-path (concat (getenv "HOME") "/org/bibnotes.org")
   org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
   org-ref-notes-directory (concat (getenv "HOME") "/org/")
   org-ref-notes-function 'orb-edit-notes
   ))

;; Magit is just rough for me and really slow
(use-package! eee
  :config

  ;; Should have wezterm or alacritty installed, more terminal application is supporting...
  ;; Issues and pull requests are welcome
  ;; (setq ee-terminal-command "wezterm")

  ;; Define key bindings using the `map!` macro
  (map! :leader                      ;; Use `space` as the leader key
        "f z" #'ee-find               ;; Space f for ee-find
        "g g" #'ee-lazygit            ;; Space g for ee-lazygit
        "y" #'ee-yazi-project       ;; Space y for ee-yazi-project
        "Y" #'ee-yazi               ;; Space f for ee-yazi with C-x C-f
        "r /" #'ee-rg             ;; C-S-f for ee-rg
        "M-f" #'ee-line)            ;; M-f for ee-line in normal mode
  )

; First, we'll define our source and destination directories
(defvar org-docx-source-dir "~/org/leetcode/")  ; Your org files location
(defvar org-docx-export-dir "~/org-exports/")  ; Where .docx files will go
(defvar org-gdoc-export-dir "~/org-exports/")  ; Where .docx files will go
(defvar gdrive-sync-dir org-docx-export-dir)  ; Same as export dir in this case

;; Ensure directories exist
(unless (file-directory-p org-docx-export-dir)
  (make-directory org-docx-export-dir t))
;; https://www.kostaharlan.net/posts/google-docs-to-org-mode/
;; Function to convert .org to .docx using Pandoc
(defun convert-org-to-docx (org-file)
  "Convert an org file to docx format using Pandoc with absolute path handling."
  (message "Starting conversion of: %s" org-file)
  (let* ((org-file-absolute (expand-file-name org-file))
         (basename (file-name-base org-file-absolute))
         (docx-file (expand-file-name (concat basename ".docx") org-docx-export-dir)))
    (message "Will export from: %s" org-file-absolute)
    (message "Will export to: %s" docx-file)
    (if (executable-find "pandoc")
        (let* ((default-directory (file-name-directory org-file-absolute))
               (result (call-process "pandoc" nil "*Org-Docx-Export*" t
                                   org-file-absolute
                                   "-o" docx-file)))
          (if (= 0 result)
              (message "Successfully converted %s to %s" org-file-absolute docx-file)
            (progn
              (message "Error: Pandoc conversion failed with code %d" result)
              (with-current-buffer "*Org-Docx-Export*"
                (message "Pandoc output: %s" (buffer-string)))
              (switch-to-buffer "*Org-Docx-Export*"))))
      (message "Error: Pandoc not found in system PATH"))))

(defun convert-org-to-gdoc (org-file)
  "Convert an org file to HTML format optimized for Google Docs import."
  (let* ((org-file-absolute (expand-file-name org-file))
         (basename (file-name-base org-file-absolute))
         (html-file (expand-file-name (concat basename ".html") org-gdoc-export-dir)))
    ;; Using pandoc with options optimized for Google Docs
    (call-process "pandoc" nil nil nil
                 org-file-absolute
                 "-o" html-file
                 "--to=html5"
                 "--standalone"
                 "--css="  ; No CSS to keep it clean for Google Docs
                 "--wrap=none"
                 "--metadata" "pagetitle=''")
    html-file))

;; Function to sync with Google Drive using rclone
(defun sync-to-gdrive ()
  "Sync export directory to Google Drive using rclone."
  (interactive)
  (message "Starting sync to Google Drive...")
  (let* ((output-buffer (generate-new-buffer " *rclone-output*"))
         (command (format "rclone sync %s gdrive:Leetcode"
                         (directory-file-name gdrive-sync-dir)))
         (sentinel-fn
          (lambda (process event output-buf)
            (when (string= event "finished\n")
              (if (= 0 (process-exit-status process))
                  (progn
                    (message "Sync to Google Drive completed successfully")
                    (when (buffer-live-p output-buf)
                      (kill-buffer output-buf)))
                (progn
                  (when (buffer-live-p output-buf)
                    (with-current-buffer output-buf
                      (message "Rclone error: %s" (buffer-string)))
                    (kill-buffer output-buf))))))))
    (make-process
     :name "rclone-sync"
     :buffer output-buffer
     :command (list "bash" "-c" command)
     :sentinel (lambda (process event)
                (funcall sentinel-fn process event output-buffer)))))
;; (defun sync-to-gdrive ()
;;   "Sync export directory to Google Drive using rclone."
;;   (interactive)
;;   (message "Synching to Gdrive %s" (directory-file-name gdrive-sync-dir)) ; Add this line
;;   (async-shell-command
;;    (format "rclone sync %s gdrive:Leetcode"
;;            (directory-file-name gdrive-sync-dir))))

;; Auto-export hook function
(defun org-auto-export-docx-on-save ()
  "Export org to docx after save if in the correct directory."
  (when (and buffer-file-name
             (string-prefix-p (expand-file-name org-docx-source-dir)
                            (buffer-file-name)))
    (message "Converting to docx: %s" buffer-file-name)
    (convert-org-to-docx buffer-file-name)
    (sync-to-gdrive)))

;; Add to org-mode-hook
;; (add-hook 'org-mode-hook 'org-auto-export-docx-on-save)
;; Use after! for Doom-specific configuration
  ;; Add to local after-save-hook for org files
;; this only works for org/leetcode directory which i have configured
(add-hook! 'after-save-hook #'org-auto-export-docx-on-save)

;; Optional: Command to manually trigger sync
(defun force-org-docx-sync ()
  "Manually convert all org files and sync to GDrive with detailed logging."
  (interactive)
  (message "Starting forced sync of all org files...")
  (let ((org-files (directory-files-recursively org-docx-source-dir "\\.org$")))
    (if org-files
        (progn
          (message "Found %d org files to process" (length org-files))
          (dolist (org-file org-files)
            (message "Processing: %s" org-file)
            (convert-org-to-docx org-file)))
      (message "No .org files found in %s" org-docx-source-dir))))

(defun test-org-docx-setup ()
  "Test the org-to-docx setup and report status."
  (interactive)
  (let ((report "Org-to-DOCX Setup Test Results:\n"))
    ;; Check source directory
    (setq report (concat report
                        (format "\nSource Directory (%s):" org-docx-source-dir)
                        (if (file-directory-p org-docx-source-dir)
                            " ✓ EXISTS"
                            " ✗ MISSING")))

    ;; Check export directory
    (setq report (concat report
                        (format "\nExport Directory (%s):" org-docx-export-dir)
                        (if (file-directory-p org-docx-export-dir)
                            " ✓ EXISTS"
                            " ✗ MISSING")))

    ;; Check pandoc installation
    (setq report (concat report
                        "\nPandoc Installation:"
                        (if (executable-find "pandoc")
                            (format " ✓ FOUND (%s)"
                                    (shell-command-to-string "pandoc --version | head -n 1"))
                            " ✗ NOT FOUND")))

    ;; Count org files
    (let ((org-files (directory-files-recursively org-docx-source-dir "\\.org$")))
      (setq report (concat report
                          (format "\nOrg Files Found: %d" (length org-files))
                          "\nFiles:\n"
                          (mapconcat 'identity org-files "\n"))))

    ;; Display report in new buffer
    (with-current-buffer (get-buffer-create "*Org-Docx-Test*")
      (erase-buffer)
      (insert report)
      (display-buffer (current-buffer)))))

;; from the auto-hide package
(global-auto-hide-mode)
