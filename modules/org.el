(map! :map org-mode-map :ni "C-<enter>" #'org-insert-heading)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("p" . "src python"))
(add-to-list 'org-structure-template-alist '("j" . "src java"))
(add-to-list 'org-structure-template-alist '("k" . "src kotlin"))
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("rb" . "src ruby"))
