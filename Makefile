PKGNAME	    = $(shell oasis query name)
PKGVERSION  = $(shell oasis query version)
PKG_TARBALL = $(PKGNAME)-$(PKGVERSION).tar.gz

DISTFILES   = README.md _oasis _tags Makefile \
  $(wildcard $(addprefix lib/, *.ml *.mli))

OPAM_FILES = oloop.install $(addprefix opam/, descr findlib opam)

all byte native: configure $(OPAM_FILES)
	ocaml setup.ml -build

configure: setup.data
setup.data: setup.ml
	ocaml $< -configure --enable-tests
#	Use the local executable oloop-top in order to enable tests
#	without installing the library:
	echo "let default_toplevel = \"./oloop-top.byte\"" \
	  >> lib/oloop_conf.ml

setup.ml: _oasis
	oasis setup -setup-update dynamic
	-touch $@

test doc install uninstall reinstall: all
	ocaml setup.ml -$@

opam $(OPAM_FILES): _oasis
	oasis2opam --local

tar: $(DISTFILES)
	mkdir $(PKGNAME)-$(PKGVERSION)
	cp --parents -r $(DISTFILES) $(PKGNAME)-$(PKGVERSION)/
#	Create a setup.ml independent of oasis
	cd $(PKGNAME)-$(PKGVERSION) && oasis setup
	tar -zcvf $(PKG_TARBALL) $(PKGNAME)-$(PKGVERSION)
	$(RM) -rf $(PKGNAME)-$(PKGVERSION)

clean:
	rm -rf \
          _build *.bak *.byte *.native \
          setup.data setup.log setup.ml \
          lib/META lib/oloop.mldylib lib/oloop.mllib \
          lib/oloop_conf.ml lib/oloop_ocaml.ml \
          lib/oloop_rule.ml lib/oloop_types.ml \
          app/app_conf.ml \
          $(PKG_TARBALL)


.PHONY: all byte native configure doc test install uninstall reinstall \
  opam clean tar
