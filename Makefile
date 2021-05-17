POSTS = $(patsubst posts/%.org,out/%.html,$(wildcard posts/*.org))
OUTDIR := out
TMPDIR := tmp

.PHONY: all
all: $(OUTDIR) $(POSTS) $(OUTDIR)/atom.xml static

$(OUTDIR):
	git clone git@github.com:k32/k32.github.io.git $(OUTDIR)

## Posts:
$(OUTDIR)/%.html: posts/%.org ./org-convert.el | $(OUTDIR)
	./org-convert.el $< $@

$(OUTDIR)/atom.xml: $(POSTS)
	./make-feed.el $(OUTDIR)

## Static:
PNGS ?= $(patsubst posts/%, $(OUTDIR)/%, $(wildcard $(TMPDIR)/*.png))
$(OUTDIR)/%.png: $(TMPDIR)/%.png | $(POSTS)
	convert -strip $< $@

SVGS ?= $(patsubst $(TMPDIR)/%, $(OUTDIR)/%, $(wildcard $(TMPDIR)/*.svg))
$(OUTDIR)/%.svg: $(TMPDIR)/%.svg cleanup-svg.xsl | $(POSTS)
	xsltproc cleanup-svg.xsl $< > $@

STATIC ?= $(patsubst static/%, $(OUTDIR)/%, $(wildcard static/*))
$(OUTDIR)/%: static/% | $(OUTDIR)
	cp $< $@

.PHONY: static
static: $(PNGS) $(SVGS) $(STATIC)

# Special targets
out/.last_update: out/*
	cd out && git add -A && git commit -m "publish"
	touch out/.last_update

.PHONY: publish
publish: all out/.last_update
	cd $(OUTDIR) && git push

.PHONY: clean
clean:
	rm -rf $(OUTDIR) || true
	rm -rf $(TMPDIR) || true
