#!/usr/bin/env -S emacs --script
(package-initialize)

(require 'ox)
(require 'ox-html)
(require 'htmlize)
(require 'font-lock)
(require 'erlang)

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

(setq org-html-validation-link nil)
(setq org-auto-sitemap nil)
(setq org-sitemap-title "k32's notebook")
(setq org-sitemap-filename "index.org")
(setq org-sitemap-sort-files 'chronologically)
(setq org-html-link-home "index.html")
(setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"default.css\"/>
<link href=\"atom.xml\" type=\"application/atom+xml\" rel=\"alternate\" title=\"k32's notebook\"/>
<meta http-equiv=\"Permissions-Policy\" content=\"interest-cohort=()\"/>")
(setq org-html-head-include-default-style nil)
(setq org-html-head-extra nil)
(setq org-html-preamble nil)
(setq org-html-head-include-scripts nil)
(setq org-html-postamble nil)
;(setq org-html-htmlize-output-type 'css)
(setq org-export-with-toc nil)
(setq org-export-with-author nil)
(setq org-export-with-email nil)
(setq org-export-with-section-numbers nil)

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
