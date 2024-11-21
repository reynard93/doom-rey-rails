(package! harpoon)
(package! ob-diagrams :recipe (:host github :repo "emacsbliss/ob-diagrams"))

;; (package! telega)
(package! evil-tutor :pin "4e124cd3911dc0d1b6817ad2c9e59b4753638f28")
(package! robe :disable t)

(package! rails-routes :pin "eab995a9297ca5bd9bd4f4c2737f2fecfc36def0")
(package! ruby-json-to-hash :pin "383b22bb2e007289ac0dba146787d02ff99d4415")
(package! rails-i18n :pin "8e87e4e48e31902b8259ded28a208c2e7efea6e9")
(package! multi-line :pin "625c608443f98bb34b4d5600d52c198509fb64d0")
(package! ruby-refactor :pin "e6b7125878a08518bffec6942df0c606f748e9ee")

(package! string-inflection :pin "fd7926ac17293e9124b31f706a4e8f38f6a9b855")

(package! hydra)
(package! unity :recipe (:host github :repo "elizagamedev/unity.el" :branch "main"))
(package! pg :recipe (:host github :repo "emarsden/pg-el" :branch "main"))
(package! pgmacs :recipe (:host github :repo "emarsden/pgmacs" :branch "main")) ; keeps hanging
(package! dape :recipe (:host github :repo "svaante/dape" :branch "master"))
(package! embark)
(package! ready-player :recipe (:host github :repo "xenodium/ready-player" :branch "main"))
(package! dslide
  :recipe (:host github
           :repo "positron-solutions/dslide"))

;; for opening magit buffers within org documents use orgit which mpi might alrdy have or i need to (require 'orgit)

(package! gptel)

(package! ob-http :recipe (:host github :repo "ag91/ob-http" :branch "master"))
;; (package! codeium :recipe (:host github :repo "Exafunction/codeium.el"))

(package! vertico-posframe)
(package! plz)
(package! dired+)

;; https://github.com/Lambda-Emacs/lambda-emacs
;; packages required for teaching
(package! org-appear)
(package! org-autolist)
(package! ox-hugo)
(package! ox-pandoc)

;; https://mclare.blog/posts/how-i-org/
(package! org-cliplink)
(package! org-download)
(package! org-modern)
(package! org-mime)
(package! org-pomodoro)
(package! org-projectile)
(package! org-ql)
(package! org-rich-yank)
(package! org-protocol-capture-html)


(package! org-ref)

;; don't specify files won't pull in the shell scripts
(package! eee :recipe (:host github :repo "eval-exec/eee.el" :branch "main" :files ("*.el" "*.sh")))

(package! auto-hide :recipe (:host github :repo "ultronozm/auto-hide.el" :branch "main"))
