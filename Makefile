.PHONY: render
render: out/.git
	emacs -Q -batch -l publish.el

out/.git:
	git clone git@github.com:k32/k32.github.io.git out

out/.last_update: out/*
	cd out && git add -A && git commit -m "publish"
	touch out/.last_update

.PHONY: publish
publish: out/.last_update
	cd out && git push

.PHONY: clean
clean:
	rm ~/.org-timestamps/*
	rm -rf out
