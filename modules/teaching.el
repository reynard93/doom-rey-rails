;;; teaching.el -*- lexical-binding: t; -*-
;; https://www.colinmclear.net/posts/teaching-notes/
;; https://github.com/mclear-tools/dotemacs/blob/master/cpm-setup-teaching.el
;; Useful functions specifically for teaching or teaching-related work
;; Setup files for defined custom classes are in .local

;; show markup at point -- this should be part of org!
(use-package! org-appear
  :commands (org-appear-mode)
  :custom
  (org-appear-autoemphasis  t)
  (org-appear-autolinks nil)
  (org-appear-autosubmarkers t))

;; mainly for erd diagrams
(use-package! ob-diagrams
  :custom
  ;; this node package is just NOT working tried different electron versions
  ;; (ob-diagrams-cli "~/.nvm/versions/node/v21.7.3/bin/diagrams")
  (ob-diagrams-erd-cli "/Users/reynardtw/.local/bin/erd"))

;; better org-present?
;; (use-package! dslide)

;; Better list behavior
(use-package! org-autolist)
(add-hook! 'org-mode-hook #'org-appear-mode)
(add-hook! 'org-mode-hook #'org-autolist-mode)

(after! ob-ditaa
  (progn
    ;; http://pages.sachachua.com/.emacs.d/Sacha.html
    (setq org-ditaa-jar-path (expand-file-name
                              "ditaa.jar"
                              (concat user-emacs-directory "software/")))))
(after! ob-plantuml
  (progn
    ;; http://pages.sachachua.com/.emacs.d/Sacha.html
    (setq org-plantuml-jar-path (expand-file-name
                                 "plantuml.jar"
                                 (concat user-emacs-directory "software/")))))

;; Drag and drop images to Emacs org-mode. Courtesy of abo-abo.
;; https://github.com/abo-abo/org-download.

(use-package! org-download
  :commands (org-download-yank org-download-screenshot org-download-image)
  :custom
  (org-download-method 'directory)
  (org-download-image-dir (concat org-directory "org-pictures/"))
  (org-download-image-latex-width 500)
  (org-download-timestamp "%Y-%m-%d"))

;;; Org Export Extensions
;;;; Ox-Pandoc
;; Export w/pandoc
(use-package! ox-pandoc
  :if (executable-find "pandoc")
  :custom
  (org-pandoc-command (executable-find "pandoc"))
  (org-pandoc-options '((standalone .  t)))
  (org-pandoc-options-for-docx '((standalone . nil)))
  (org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
  (org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))
  (org-pandoc-format-extensions '(org+smart)))
;;;; Ox-Pandoc Export Menu Options
;; Set pandoc export options
(setq org-pandoc-menu-entry
      '(
        (?4 "to html5 and open." org-pandoc-export-to-html5-and-open)
        (?$ "as html5." org-pandoc-export-as-html5)
        (?5 "to html5-pdf and open." org-pandoc-export-to-html5-pdf-and-open)
        (?% "to html5-pdf." org-pandoc-export-to-html5-pdf)
        (?8 "to opml." org-pandoc-export-to-opml)
        (?9 "to opml and open." org-pandoc-export-to-opml-and-open)
        (?< "to slideous and open." org-pandoc-export-to-slideous-and-open)
        (?, "as slideous." org-pandoc-export-as-slideous)
        (?= "to ms-pdf and open." org-pandoc-export-to-ms-pdf-and-open)
        (?- "to ms-pdf." org-pandoc-export-to-ms-pdf)
        (?b "to beamer-pdf and open." org-pandoc-export-to-beamer-pdf-and-open)
        (?B "to beamer-pdf." org-pandoc-export-to-beamer-pdf)
        (?c "to context-pdf and open." org-pandoc-export-to-context-pdf-and-open)
        (?C "to context-pdf." org-pandoc-export-to-context-pdf)
        ;;(?d "to docbook5." org-pandoc-export-to-docbook5)
        (?d "to docbook5 and open." org-pandoc-export-to-docbook5-and-open)
        (?D "as docbook5." org-pandoc-export-as-docbook5)
        (?e "to epub3 and open." org-pandoc-export-to-epub3-and-open)
        (?E "to epub3." org-pandoc-export-to-epub3)
        (?g "to gfm and open." org-pandoc-export-to-gfm-and-open)
        (?G "as gfm." org-pandoc-export-as-gfm)
        ;;(?h "to html4." org-pandoc-export-to-html4)
        (?h "to html4 and open." org-pandoc-export-to-html4-and-open)
        (?H "as html4." org-pandoc-export-as-html4)
        ;;(?i "to icml." org-pandoc-export-to-icml)
        (?i "to icml and open." org-pandoc-export-to-icml-and-open)
        (?I "as icml." org-pandoc-export-as-icml)
        ;;(?j "to json." org-pandoc-export-to-json)
        (?j "to json and open." org-pandoc-export-to-json-and-open)
        (?J "as json." org-pandoc-export-as-json)
        ;; (?k "to markdown." org-pandoc-export-to-markdown)
        (?k "to markdown and open." org-pandoc-export-to-markdown-and-open)
        (?K "as markdown." org-pandoc-export-as-markdown)
        (?l "to latex-pdf and open." org-pandoc-export-to-latex-pdf-and-open)
        (?L "to latex-pdf." org-pandoc-export-to-latex-pdf)
        ;;(?m "to man." org-pandoc-export-to-man)
        (?m "to man and open." org-pandoc-export-to-man-and-open)
        (?M "as man." org-pandoc-export-as-man)
        ;;(?n "to native." org-pandoc-export-to-native)
        (?n "to native and open." org-pandoc-export-to-native-and-open)
        (?N "as native." org-pandoc-export-as-native)
        (?o "to odt and open." org-pandoc-export-to-odt-and-open)
        (?O "to odt." org-pandoc-export-to-odt)
        (?p "to pptx and open." org-pandoc-export-to-pptx-and-open)
        (?P "to pptx." org-pandoc-export-to-pptx)
        ;;(?q "to commonmark." org-pandoc-export-to-commonmark)
        ;;(?q "to commonmark and open." org-pandoc-export-to-commonmark-and-open)
        ;;(?Q "as commonmark." org-pandoc-export-as-commonmark)
        ;;(?r "to rtf." org-pandoc-export-to-rtf)
        (?r "to rtf and open." org-pandoc-export-to-rtf-and-open)
        (?R "as rtf." org-pandoc-export-as-rtf)
        ;;(?s "to s5." org-pandoc-export-to-s5)
        ;;(?s "to s5 and open." org-pandoc-export-to-s5-and-open)
        ;;(?S "as s5." org-pandoc-export-as-s5)
        ;;(?t "to texinfo." org-pandoc-export-to-texinfo)
        ;;(?t "to texinfo and open." org-pandoc-export-to-texinfo-and-open)
        ;;(?T "as texinfo." org-pandoc-export-as-texinfo)
        ;;(?u "to dokuwiki." org-pandoc-export-to-dokuwiki)
        (?u "to dokuwiki and open." org-pandoc-export-to-dokuwiki-and-open)
        (?U "as dokuwiki." org-pandoc-export-as-dokuwiki)
        ;; (?v "to revealjs." org-pandoc-export-to-revealjs)
        (?v "to revealjs and open." org-pandoc-export-to-revealjs-and-open)
        (?V "as revealjs." org-pandoc-export-as-revealjs)
        ;;(?w "to mediawiki." org-pandoc-export-to-mediawiki)
        (?w "to mediawiki and open." org-pandoc-export-to-mediawiki-and-open)
        (?W "as mediawiki." org-pandoc-export-as-mediawiki)
        (?x "to docx and open." org-pandoc-export-to-docx-and-open)
        (?X "to docx." org-pandoc-export-to-docx)
        ;;(?y "to slidy." org-pandoc-export-to-slidy)
        (?y "to slidy and open." org-pandoc-export-to-slidy-and-open)
        (?Y "as slidy." org-pandoc-export-as-slidy)
        ;;(?z "to dzslides." org-pandoc-export-to-dzslides)
        (?z "to dzslides and open." org-pandoc-export-to-dzslides-and-open)
        (?Z "as dzslides." org-pandoc-export-as-dzslides)))

;;;; Ox-Hugo
;; Export to Hugo with Org
;; https://github.com/kaushalmodi/ox-hugo
(use-package! ox-hugo
  :config
  ;; https://ox-hugo.scripter.co/doc/org-cite-citations/
  ;; Modify this to have a different header for references
  (plist-put org-hugo-citations-plist :bibliography-section-heading "References"))

;;;;; Batch Export Files with Org-Hugo
;; mark files and then batch export them with this command
;; requires dired+
(defun hugo-export-multi ()
  (interactive)
  (diredp-do-apply/eval-marked 'org-hugo-export-wim-to-md '(4)))

;; TODO not working atm
;; (map! :after dired :map dired-mode-map "C-+" #'hugo-export-multi)
;;;;; Org-Hugo Links
;; New link type for Org-Hugo internal links
(defun org-hugo-link-complete ()
  "Create link with Hugo ref shortcode"
  (concat "{{% ref " (file-relative-name (read-file-name "File: ")) " %}}"))

(defun org-hugo-follow (link)
  (find-file (expand-file-name link)))

(with-eval-after-load 'org
  (org-link-set-parameters "hugo"
                           :complete 'org-hugo-link-complete
                           :follow 'org-hugo-follow))

;;; Org Latex PDF Process

;;; Org Latex Classes
(setq org-latex-classes '(("beamer" "\\documentclass[presentation]{beamer}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                          ("article" "\\documentclass[11pt]{article}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}" . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                          ("report" "\\documentclass[11pt]{report}"
                           ("\\part{%s}" . "\\part*{%s}")
                           ("\\chapter{%s}" . "\\chapter*{%s}")
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                          ("book" "\\documentclass[11pt]{book}"
                           ("\\part{%s}" . "\\part*{%s}")
                           ("\\chapter{%s}" . "\\chapter*{%s}")
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

                          ;; notes
                          ("org-notes" "\\documentclass[12pt]{article}
                           [NO-DEFAULT-PACKAGES]
                           [EXTRA]
                           \\input{/Users/roambot/bin/lisp-projects/org-latex-classes/notes-setup-file.tex}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}" . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

                          ;; beamer handout
                          ("beamer-handout" "\\documentclass[12pt]{article}
                           [NO-DEFAULT-PACKAGES]
                           [EXTRA]
                           \\input{/Users/roambot/bin/lisp-projects/org-latex-classes/handout-setup-file.tex}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}" . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

                          ;; beamer presentation
                          ("beamer-presentation" "\\documentclass[presentation]{beamer}
                           [NO-DEFAULT-PACKAGES]
                           [PACKAGES]
                           \\usepackage{pgfpages}
                           [EXTRA]
                           \\setbeameroption{show notes on second screen=right}
                           \\setbeamertemplate{note page}{\\pagecolor{yellow!5}\\insertnote}
                           \\input{/Users/roambot/bin/lisp-projects/org-latex-classes/unl-beamer-preamble.tex}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

                          ;; beamer slides only
                          ("beamer-slides-no-notes" "\\documentclass[handout]{beamer}
                           [NO-DEFAULT-PACKAGES]
                           [EXTRA]
                           \\setbeameroption{hidenotes}
                           \\input{/Users/roambot/bin/lisp-projects/org-latex-classes/unl-beamer-preamble.tex}
                           [PACKAGES]"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;;; Slide Filter Notes
;; Allow reveal.js notes to work in beamer
;; Filter out notes in specified format backends
;; See
;; https://joonro.github.io/Org-Coursepack/Lectures/04%20Creating%20Content%20for%20Slides%20and%20Handouts.html#speaker-notes
(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t) string) t))

(defun my/process-NOTES-blocks (text backend info)
  "Filter NOTES special blocks in export."
  (cond
   ;; Allow export of notes in latex
   ;; ((eq backend 'latex)
   ;;  (if (string/starts-with text "\\begin{NOTES}") ""))
   ((eq backend 'rst)
    (if (string/starts-with text ".. NOTES::") ""))
   ((eq backend 'html)
    (if (string/starts-with text "<div class=\"NOTES\">") ""))
   ((eq backend 'beamer)
    (let ((text (replace-regexp-in-string "\\\\begin{NOTES}" "\\\\note{" text)))
      (replace-regexp-in-string "\\\\end{NOTES}" "}" text)))
   ))

(eval-after-load 'ox '(add-to-list
                       'org-export-filter-special-block-functions
                       'my/process-NOTES-blocks))

;;; Slides
;; I got the tag based selective export idea from J Kitchin
;; https://kitchingroup.cheme.cmu.edu/blog/2013/12/08/Selectively-exporting-headlines-in-org-mode/

;; FIXME: Right now aync export doesn't work with let...

;;;; Org export to slides w/notes

(defun cpm/org-export-beamer-presentation ()
  "Export subtree to beamer PDF"
  (interactive)
  (let ((org-export-exclude-tags '("handout")))
    (save-excursion
      (goto-char (point-min))
      (org-beamer-export-to-pdf nil t nil nil '(:latex-class "beamer-presentation")))))

;; (org-open-file (org-beamer-export-to-pdf nil t nil nil '(:latex-class "beamer-presentation")))))


(defun cpm/org-export--file-beamer-presentation ()
  "Export file to beamer pdf"
  (interactive)
  (let ((org-export-exclude-tags '("noexport" "handout")))
    (save-excursion
      (goto-char (point-min))
      (org-beamer-export-to-pdf nil nil nil nil '(:latex-class "beamer-presentation")))))

;;;; Org export to slides w/o notes
(defun cpm/org-export-beamer-no-notes ()
  "Export org subtree slide content to useful custom style handout (PDF) form"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (org-open-file (org-beamer-export-to-pdf nil t nil nil '(:latex-class "beamer-slides-no-notes")))))

(defun cpm/org-export--file-beamer-no-notes ()
  "Export org file slide content to useful custom style handout (PDF) form"
  (interactive)
  (let ((org-export-exclude-tags '("slides")))
    (save-excursion
      (goto-char (point-min))
      (org-beamer-export-to-pdf nil nil nil nil '(:latex-class "beamer-slides-no-notes")))))


;;; Handouts

(defun cpm/org-export-beamer-handout ()
  "Export subtree content to PDF handout. Handout uses a distinctive quote style."
  (interactive)
  (let ((org-latex-default-quote-environment "quote-b")
        (org-latex-compiler "xelatex")
        (org-export-exclude-tags '("slides")))
    (org-narrow-to-subtree)
    (save-excursion
      (goto-char (point-min))
      (org-latex-export-to-pdf nil t nil nil '(:latex-class "beamer-handout")))
    (widen)))

(defun cpm/org-export--file-beamer-handout ()
  "Export file content to PDF handout. Handout uses a distinctive quote style."
  (interactive)
  (let ((org-latex-default-quote-environment "quote-b")
        (org-latex-compiler "xelatex")
        (org-export-exclude-tags '("slides" "noexport")))
    (org-latex-export-to-pdf nil nil nil nil '(:latex-class "beamer-handout"))))

;;; Notes

;;;; Org to PDF Notes
(defun cpm/org-export-pdf-notes ()
  "Export subtree of notes to PDF file. Note uses a distinctive quote style."
  (interactive)
  (let ((org-latex-default-quote-environment "quote-b")
        (org-latex-compiler "xelatex")
        (org-export-exclude-tags '("noexport")))
    (org-narrow-to-subtree)
    (save-excursion
      (goto-char (point-min))
      (org-latex-export-to-pdf nil t nil nil '(:latex-class "org-notes")))
    (widen)))

(defun cpm/org-export--file-pdf-notes ()
  "Export file notes to PDF file. Note uses a distinctive quote style."
  (interactive)
  (let ((org-latex-default-quote-environment "quote-b")
        (org-latex-compiler "xelatex")
        (org-export-exclude-tags '("noexport")))
    (save-excursion
      (goto-char (point-min))
      (org-latex-export-to-pdf nil nil nil nil '(:latex-class "org-notes")))))

(defun cpm/cleanup-pdf-notes()
  "Move notes to static directory & cleanup other files"
  (interactive)
  (eshell-command "rm -f *.tex && mv -f *.pdf static/materials/handouts/"))
