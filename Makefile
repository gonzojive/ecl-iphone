# Generated automatically from Makefile.in by configure.
# DIST: This is the distribution Makefile for ECL.  configure can
# DIST: make most of the changes to this file you might want, so try
# DIST: that first.

# make all	to compile and build Emacs.
# make install	to install it.
# make TAGS	to update tags tables.
#
# make clean  or  make mostlyclean
#      Delete all files from the current directory that are normally
#      created by building the program.	 Don't delete the files that
#      record the configuration.  Also preserve files that could be made
#      by building, but normally aren't because the distribution comes
#      with them.
#
#      Delete `.dvi' files here if they are not part of the distribution.
# 
# make distclean
#      Delete all files from the current directory that are created by
#      configuring or building the program.  If you have unpacked the
#      source and built the program without creating any other files,
#      `make distclean' should leave only the files that were in the
#      distribution.
# 
# make realclean
#      Delete everything from the current directory that can be
#      reconstructed with this Makefile.  This typically includes
#      everything deleted by distclean.

SHELL = /bin/sh
MAKE = make  # BSD doesn't have it as a default.
MACHINE = freebsd

# ========================= Last release ================================

VERSION=0.2
WWW=http://ecls.sourceforge.net/

# ==================== Things `configure' Might Edit ====================

CC=gcc
CPP=gcc -E
LN_S=ln -s
CFLAGS=-g -O2

#  Where to find the source code.
#  This is set by the configure script's `--srcdir' option.
srcdir=/home/worm/src/ecls/src

bindir=/home/worm/bin
infodir=/home/worm/info
mandir=/home/worm/man/man1
libdir=/home/worm/lib/ecls

#  What to release
TAR_CONTENTS=Makefile.in Copy* README.1st README.orig doc \
	configure site.lsp src/c src/cmp src/crs src/clos src/lsp src/doc \
	src/h src/etc src/gmp src/config* src/install.sh src/Makefile.in \
	src/util contrib/ src/clx src/tk src/gc src/*.in src/gabriel

# ==================== Utility Programs for the Build ====================

#  Allow the user to specify the install program.
INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644

# ============================= Build ==============================

all: build/Makefile
	cd build; $(MAKE)
.PHONY:	all

Makefile: Makefile.in build/config.status
	(cd build; ./config.status)

# ==================== Installation ====================

install: build/Makefile
	(cd build; make install)
uninstall:
	(cd build; make uninstall)

# ==================== Documentation ====================

info:
	(cd ${srcdir}/doc; $(MAKE) info)
dvi:
	(cd ${srcdir}/doc; $(MAKE) dvi)

# ==================== Cleaning up and miscellanea ====================

#   `clean'
#        Delete all files from the current directory that are normally
#        created by building the program.  Don't delete the files that
#        record the configuration.  Also preserve files that could be made
#        by building, but normally aren't because the distribution comes
#        with them.
#   
clean:
	cd build; $(MAKE) clean

#   `distclean'
#        Delete all files from the current directory that are created by
#        configuring or building the program.  If you have unpacked the
#        source and built the program without creating any other files,
#        `make distclean' should leave only the files that were in the
#        distribution.

distclean: clean
	rm -fr build/config.status
	rm -f Makefile

#   `realclean'
#        Delete everything from the current directory that can be
#        reconstructed with this Makefile.
#        One exception, however: `make realclean' should not delete
#        `configure' even if `configure' can be remade using a rule in the
#        Makefile.  More generally, `make realclean' should not delete
#        anything that needs to exist in order to run `configure' and then
#        begin to build the program.
realclean: distclean

TAGS tags:
	(cd ${srcdir}/src; \
	 etags c/*.[cd] h/*.h)

check:
	@echo "We don't have any tests for ECL yet."

TAR_DIR=ecls-$(VERSION)

doc: build/doc/index.html
	-mkdir doc
	cp build/doc/*.html doc
build/doc/index.html:
	cd build/doc; make

source-dist: ecls.tgz ecls-tests.tgz

ecls.tgz: doc
	-rm -rf $(TAR_DIR)
	mkdir $(TAR_DIR) $(TAR_DIR)/src && \
	for i in $(TAR_CONTENTS); do cp -rf $$i $(TAR_DIR)/$$i; done && \
	tar -cz --exclude '*~' --exclude '#*' --exclude 'CVS' -f ecls.tgz $(TAR_DIR)
	-rm -rf $(TAR_DIR)
ecls-tests.tgz:
	-rm -rf $(TAR_DIR)
	mkdir $(TAR_DIR) && \
	mkdir $(TAR_DIR)/src && \
	cp -rf src/ansi-tests $(TAR_DIR)/src && \
	tar -cz --exclude '*~' --exclude '#*' --exclude 'CVS' -f ecls-tests.tgz $(TAR_DIR);
	-rm -rf $(TAR_DIR)

binary-dist: all
	su -c "rm -rf tmp"
	mkdir tmp
	for i in $(bindir) $(infodir) $(mandir) $(libdir); do \
	(echo $$i; IFS="/"; for k in tmp/$$i; do echo $$k; (test -d $$k || mkdir $$k); chmod 755 $$k; cd $$k; done); \
	done
	PREFIX=`pwd`/tmp; cd build; make install PREFIX="$${PREFIX}"
	su -c "chown -R root.root tmp && cd tmp; tar czf ../ecls-$(VERSION)-$(MACHINE).tgz * && cd .. && rm -rf tmp"

dist:
	cd dist; make-dist
