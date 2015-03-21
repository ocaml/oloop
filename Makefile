PKGNAME	    = $(shell oasis query name)
PKGVERSION  = $(shell oasis query version)
PKG_TARBALL = $(PKGNAME)-$(PKGVERSION).tar.gz

DISTFILES   = README.md _oasis _tags Makefile \
  $(wildcard $(addprefix lib/, *.ml *.mli))

all byte native: configure
	ocaml setup.ml -build

configure: setup.data
setup.data: setup.ml
	ocaml $< -configure --enable-tests

setup.ml: _oasis
	oasis setup -setup-update dynamic
	-touch $@

test doc install uninstall reinstall: all
	ocaml setup.ml -$@


tar: $(DISTFILES)
	mkdir $(PKGNAME)-$(PKGVERSION)
	cp --parents -r $(DISTFILES) $(PKGNAME)-$(PKGVERSION)/
#	Create a setup.ml independent of oasis
	oasis setup
	tar -zcvf $(PKG_TARBALL) $(PKGNAME)-$(PKGVERSION)
	$(RM) -rf $(PKGNAME)-$(PKGVERSION)

clean:
	ocaml setup.ml -$@
	$(RM) $(PKG_TARBALL) $(wildcard setup.data tests/*.txt) \
	  $(wildcard lib/oloop_conf.ml lib/oloop_ocaml.ml)

.PHONY: all byte native configure doc test install uninstall reinstall \
  clean tar
