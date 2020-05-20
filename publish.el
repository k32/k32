(require 'package)
(package-initialize)

(require 'ox-publish)
(require 'htmlize)
(require 'webfeeder)
(require 'font-lock)
(require 'erlang)

(setq make-backup-files nil)

(defun my-format-ref (refs) "org")
(advice-add 'org-export-format-reference :override 'my-format-ref)

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

(setq htmlize-use-rgb-map 'force)
(setq out-dir "./out")

(setq org-publish-project-alist
      `(("posts"
         :base-directory "./posts"
         :base-extension "org"
         :publishing-function org-html-publish-to-html
         :publishing-directory ,out-dir
         :html-validation-link nil
         :auto-sitemap nil
         :sitemap-title "k32's notebook"
         :sitemap-filename "index.org"
         :sitemap-sort-files chronologically
         :html-link-home "index.html"
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"default.css\"/>"
         :html-head-include-default-style nil
         :html-head-extra nil
         :html-preamble nil
         :html-head-include-scripts nil
         :html-postamble nil
         :html-htmlize-output-type css
         :with-toc nil
         :with-author nil
         :with-email nil
         :with-section-numbers nil)
        ("static"
         :publishing-function org-publish-attachment
         :publishing-directory "./out"
         :base-directory "./static"
         :base-extension "css")
        ("blog"
         :components ("posts" "static"))))

(org-publish-all t)

(webfeeder-build
 "atom.xml"
 out-dir
 "https://blog.erlang.moe/"
 (directory-files out-dir nil "[0-9]\\{4\\}.*html$")
 :title "My homepage"
 :description "A collection of articles in RSS"
 :builder 'webfeeder-make-rss)

(with-temp-file (concat out-dir "/CNAME")
  (insert "blog.erlang.moe"))
