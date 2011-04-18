# Automatically Generated Makefile by EDE.
# For use with: make
#
# DO NOT MODIFY THIS FILE OR YOUR CHANGES MAY BE LOST.
# EDE is the Emacs Development Environment.
# http://cedet.sourceforge.net/ede.shtml
#

top=
ede_FILES=Project.ede Makefile

wy_SEMANTIC_GRAMMAR=wisent-dot.wy
COGRE_LISP=cogre.el cogre-uml.el cogre-periodic.el cogre-ascii.el cogre-srecode.el cogre-convert.el cogre-layout.el cogre-semantic.el
EMACSFLAGS=-batch --no-site-file
EMACS=emacs
LOADPATH= ../ede/ ../common/ ../semantic/ ../eieio/\
    ../semantic/wisent/  ../srecode/ ../speedbar/ ../semantic/bovine/
wy_SEMANTIC_GRAMMAR_EL=wisent-dot-wy.el
LOADDEFS=cogre-loaddefs.el
LOADDIRS=.
init_LISP=cogre-load.el
ELISPPRELOAD= cedet-compat
dot_LISP=wisent-dot.el cogre-dot-mode.el
Mode_LISP=cogre-mode.el
info_TEXINFOS=cogre.texi
MAKEINFO=makeinfo
misc_MISC=INSTALL ChangeLog
hacks_LISP=picture-hack.el
utests_LISP=cogre-utest.el
icons_MISC=cogre-class.xpm cogre-hasa.xpm cogre-isa.xpm cogre-package.xpm cogre-node.xpm cogre-instance.xpm cogre-arrow.xpm cogre-link.xpm cogre-note.xpm
VERSION=1.0
DISTDIR=$(top)COGRE-$(VERSION)



all: wy autoloads init dot Mode cogre.info misc hacks COGRE utests icons Tests templates

.PHONY: wy
wy: $(wy_SEMANTIC_GRAMMAR)
	@echo "(add-to-list 'load-path nil)" > grammar-make-script
	@for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> grammar-make-script; \
	done;
	@echo "(require 'semantic-load)" >> grammar-make-script
	@echo "(require 'semantic-grammar)" >> grammar-make-script
	"$(EMACS)" -batch --no-site-file -l grammar-make-script -f semantic-grammar-batch-build-packages $^

.PHONY: autoloads
autoloads: 
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(require 'cedet-autogen)" >> $@-compile-script
	"$(EMACS)" -batch --no-site-file -l $@-compile-script -f cedet-batch-update-autoloads $(LOADDEFS) $(LOADDIRS)

.PHONY: init
init: $(init_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	for preload in ${ELISPPRELOAD}; do \
	   echo "(load \"$$preload\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" $(EMACSFLAGS) -l $@-compile-script -f batch-byte-compile $^

.PHONY: dot
dot: $(dot_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	for preload in ${ELISPPRELOAD}; do \
	   echo "(load \"$$preload\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" $(EMACSFLAGS) -l $@-compile-script -f batch-byte-compile $^

.PHONY: Mode
Mode: $(Mode_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	for preload in ${ELISPPRELOAD}; do \
	   echo "(load \"$$preload\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" $(EMACSFLAGS) -l $@-compile-script -f batch-byte-compile $^

cogre.info: $(info_TEXINFOS)
	$(MAKEINFO) $<

misc: 
	@

.PHONY: hacks
hacks: $(hacks_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" $(EMACSFLAGS) -l $@-compile-script -f batch-byte-compile $^

.PHONY: COGRE
COGRE: $(COGRE_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	for preload in ${ELISPPRELOAD}; do \
	   echo "(load \"$$preload\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" $(EMACSFLAGS) -l $@-compile-script -f batch-byte-compile $^

.PHONY: utests
utests: $(utests_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	for loadpath in . ${LOADPATH}; do \
	   echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	done;
	for preload in ${ELISPPRELOAD}; do \
	   echo "(load \"$$preload\")" >> $@-compile-script; \
	done;
	@echo "(setq debug-on-error t)" >> $@-compile-script
	"$(EMACS)" $(EMACSFLAGS) -l $@-compile-script -f batch-byte-compile $^

icons: 
	@

.PHONY:Tests
Tests:
	$(MAKE) -C tests

.PHONY:templates
templates:
	$(MAKE) -C templates

tags: 
	$(MAKE) -C tests/ $(MFLAGS) $@
	$(MAKE) -C templates/ $(MFLAGS) $@


clean:
	rm -f *.elc *.html *.info*

.PHONY: dist

dist: $(wy_SEMANTIC_GRAMMAR_EL) autoloads cogre.info
	mkdir $(DISTDIR)
	cp $(wy_SEMANTIC_GRAMMAR) $(wy_SEMANTIC_GRAMMAR_EL) cogre-loaddefs.el $(init_LISP) $(dot_LISP) $(Mode_LISP) $(info_TEXINFOS) cogre.info* $(misc_MISC) $(hacks_LISP) $(COGRE_LISP) $(utests_LISP) $(icons_MISC) $(ede_FILES) $(DISTDIR)
	$(MAKE) -C tests $(MFLAGS) DISTDIR=$(DISTDIR)/tests dist
	$(MAKE) -C templates $(MFLAGS) DISTDIR=$(DISTDIR)/templates dist

Makefile: Project.ede
	@echo Makefile is out of date!  It needs to be regenerated by EDE.
	@echo If you have not modified Project.ede, you can use 'touch' to update the Makefile time stamp.
	@false



# End of Makefile
