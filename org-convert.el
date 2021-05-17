#!/usr/bin/env -S emacs --script
(package-initialize)

(require 'ox)
(require 'ox-html)
(require 'htmlize)
(require 'font-lock)
(require 'erlang)

;; Source code colouring
(custom-set-faces
 '(default ((t (:foreground "#ffffff" :background "black"))))
 '(font-lock-builtin-face ((t (:foreground "#729fcf"))))
 '(font-lock-comment-face ((t (:foreground "#888a85"))))
 '(font-lock-constant-face ((t (:foreground "#8ae234"))))
 '(font-lock-doc-face ((t (:foreground "#888a85"))))
 '(font-lock-keyword-face ((t (:foreground "#729fcf" :bold t))))
 '(font-lock-string-face ((t (:foreground "#ad7fa8" :italic t))))
 '(font-lock-type-face ((t (:foreground "#8ae234" :bold t))))
 '(font-lock-variable-name-face ((t (:foreground "#eeeeaa"))))
 '(font-lock-warning-face ((t (:bold t :foreground "#f57900"))))
 '(font-lock-function-name-face ((t (:foreground "#edd400" :bold t :italic t)))))

(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t) (gnuplot . t) (plantuml . t) (dot . t)))

(setq make-backup-files nil)

(setq htmlize-use-rgb-map 'force)

;;;; Settings for export:

;; Remove bloat:
(setq org-html-validation-link nil
      org-html-head-include-default-style nil
      org-html-head-extra nil
      org-html-preamble nil
      org-html-head-include-scripts nil
      org-export-time-stamp-file nil)

;; Remove features I don't need:
(setq org-auto-sitemap nil
      org-export-with-toc nil
      org-export-with-author nil
      org-export-with-creator nil
      org-export-with-email nil)

;; Export options:
(setq org-html-link-home "index.html")
(setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"default.css\"/>
<link href=\"atom.xml\" type=\"application/atom+xml\" rel=\"alternate\" title=\"k32's notebook\"/>
<meta http-equiv=\"Permissions-Policy\" content=\"interest-cohort=()\"/>")
(setq org-export-with-section-numbers t)
;(setq org-html-htmlize-output-type 'css)

;; Important settings for getting dates in the Atom feed right:
(setq org-export-with-date t)

;;;;; Do export:
(let* ((orig-file (car command-line-args-left))
       (org-file (concat "tmp/" (file-name-nondirectory orig-file)))
       (out-file (file-truename (car (cdr command-line-args-left)))))
  (print (format "Transform %s -> %s" org-file out-file))
  (make-directory "tmp" t)
  ;; Create temporary file:
  (when (file-exists-p org-file)
    (delete-file org-file))
  (copy-file orig-file org-file)
  ;; Open and convert temp. file:
  (find-file org-file)
  (org-html-export-as-html)
  (write-file out-file))
