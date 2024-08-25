;;; lsp.el -*- lexical-binding: t; -*-

;; improve LSP
(after! lsp-mode
  (setq lsp-auto-guess-root t)
  (setq lsp-solargraph-symbols nil)
  (setq lsp-solargraph-folding nil)
  ;; from binchen https://blog.binchen.org/posts/how-to-speed-up-lsp-mode.html
  ;; ;; more updated https://github.com/redguardtoo/emacs.d?tab=readme-ov-file#set-up-lsp-mode
  ;; https://ianyepan.github.io/posts/emacs-ide/
  ;; enable log only for debug
  (setq lsp-log-io nil)
  ;; use `evil-matchit' instead
  (setq lsp-enable-folding nil)
  ;; no real time syntax check
  (setq lsp-diagnostic-package :none)
  ;; handle yasnippet by myself
  (setq lsp-enable-snippet nil)
  ;; turn off for better performance
  (setq lsp-enable-symbol-highlighting nil)
  ;; use find-fine-in-project instead
  (setq lsp-enable-links nil)
  ;; auto restart lsp
  (setq lsp-restart 'auto-restart)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-idle-delay 0.5)
  ;; don't watch 3rd party javascript libraries
  (push "[/\\\\][^/\\\\]*\\.\\(json\\|html\\|jade\\)$" lsp-file-watch-ignored)
  ;; don't ping LSP language server too frequently
  (defvar lsp-on-touch-time 0)
  (defun my-lsp-on-change-hack (orig-fun &rest args)
    ;; do NOT run `lsp-on-change' too frequently
    (when (> (- (float-time (current-time))
                lsp-on-touch-time) 120) ;; 2 mins
      (setq lsp-on-touch-time (float-time (current-time)))
      (apply orig-fun args)))
  (advice-add 'lsp-on-change :around #'my-lsp-on-change-hack)
  )

(after! lsp-mode
  (setq lsp-disabled-clients '(emmet-ls))
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  (setq lsp-ui-sideline-show-code-actions t)); note that lsp-ui might cause slowdown)
