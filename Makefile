EMACS= /usr/bin/emacs --batch
INITFILE=~/.emacs.d/init.el
EMACSPATH=~/.emacs.d

install:
	@$(EMACS) --load $(INITFILE)

clean:
	@rm $(EMACSPATH)/elpa/*

upgrade:
	@$(EMACS) --load $(INITFILE) --eval '(maple-package-upgrade)'

compile:
	@rm $(EMACSPATH)/cache/autoloads.pkg.el*
	@$(EMACS) --load $(INITFILE) --eval '(maple-package-force-initialize)'

all: install upgrade compile

.DEFAULT_GOAL := all
.PHONY: all install clean upgrade compile
