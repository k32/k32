#!/usr/bin/env -S emacs --script
(package-initialize)

(require 'webfeeder)

(setq out-dir (car command-line-args-left))

(webfeeder-build
 "atom.xml"
 out-dir
 "https://blog.erlang.moe/"
 (directory-files out-dir nil "[0-9]\\{4\\}.*html$")
 :title "k32's notebook"
 :description "A collection of articles in RSS"
 :builder 'webfeeder-make-rss)
