;; MOST IMPORTANT CONFIG - Set your project folders
(setq projectile-project-search-path '("~/Desktop/formflow-mono", "~/Projects"))

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
  (setq projectile-globally-ignored-directories '("flow-typed" "node_modules" "~/.emacs.d/.local/" ".idea" ".vscode" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".ccls-cache" ".cache" ".clangd")))

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

;; Google Tradutor, source and target languages
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "pt")


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
(map!  "A-h" #'yabai-window-left
       "A-j" #'yabai-window-down
       "A-k" #'yabai-window-up
       "A-l" #'yabai-window-right
       "A-f" #'yabai-fullscreen)

(map! :map evil-normal-state-map
      :ngivo "C-h" nil
      :ngivo "C-l" nil)

;; (map! :after evil
;;       :map evil-org-mode-map
;;       :ngivo "s-h" nil
;;       :ngivo "s-j" nil
;;       :ngivo "s-k" nil
;;       :ngivo "s-l" nil
;;       :ngivo "s-f" nil)

;; using for yabai left
;; (map!
;;  "M-h" nil)

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
(defmacro ct/embark-display-in-side-window (side)
  `(defun ,(intern (concat "display-in-side-window--" (symbol-name side))) (&optional buffer)
     (interactive "b")
     (when-let* ((buffer (or buffer (current-buffer)))
                 (display-buffer-overriding-action '((display-buffer-in-side-window)
                                                     (dedicated . t)
                                                     (side . ,side)
                                                     (window-parameters . ((no-delete-other-windows . t))))))
       (display-buffer buffer))))
;; creating the functions
(ct/embark-display-in-side-window bottom)
(ct/embark-display-in-side-window left)
(ct/embark-display-in-side-window right)
;; https://discourse.doomemacs.org/t/how-to-re-bind-keys/56
(map! :after embark
      :map embark-buffer-map
      (:prefix ("s" . "side")
               "b" #'display-in-side-window--bottom
               "l" #'display-in-side-window--left
               "r" #'display-in-side-window--right))
(use-package! embark
  :bind
  (("C-c a" . embark-act)))

(use-package! unity
  :defer t
  :config (add-hook 'after-init-hook #'unity-mode))

(use-package! dape
  :config
  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook 'kill-buffer)
  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root)
  (setq dape-buffer-window-arrangement 'gud)
  ;; Projectile users
  ;; (setq dape-cwd-fn 'projectile-project-root)
  )

(add-to-list 'dape-configs
             `(rdbg-attach-rails
               ;; prefix-local "/Users/reynardtw/formflow-mono/packages/app/"
               ;; prefix-remote "/usr/app/"
               port 5678
               :request "attach"
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

(defun elaiza-claude-get-api-key ()
  "Get Claude API key from auth-source, create if needed."
  (let* ((auth-source-creation-defaults
          '((description . "Claude API key")))
         (auth-source-creation-prompts
          '((secret . "Claude API key for %h: ")))
         (auth-info (nth 0 (auth-source-search
                            :max 1
                            :host "api.anthropic.com"
                            :user "elaiza"
                            :create t))))
    (if auth-info (auth-info-password auth-info)
      (error "Could not retrieve API key\nSave machine api.anthropic.com port https login elaiza password <your-api-key> in ~/.authinfo.gpg"))))

(use-package! gptel
  :config
  ;; OPTIONAL configuration
  (setq
   gptel-model "claude-3-sonnet-20240229" ;  "claude-3-opus-20240229" also available
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t :key (elaiza-claude-get-api-key))))


(use-package! codeium
  :init
  ;; use globally
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  :config
  (setq use-dialog-box nil) ;; do not use popup boxes
  ;; get codeium status in the modeline
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  ;; You can overwrite all the codeium configs!
  ;; for example, we recommend limiting the string sent to codeium for better performance
  (defun my-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  ;; if you change the text, you should also change the cursor_offset
  ;; warning: this is measured by UTF-8 encoded bytes
  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq codeium/document/text 'my-codeium/document/text)
  (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

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
