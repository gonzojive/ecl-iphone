CLISP=clisp -ansi -norc
CMUCL_SAFE=/usr/lib/cmucl/lisp-safe.core
CMUCL_NORMAL=/usr/lib/cmucl/lisp-normal.core
CMUCL_SMALL=/usr/lib/cmucl/lisp-small.core
SBCL=sbcl --sysinit /dev/null --userinit /dev/null --noprogrammer
ECL=ecl
CMUCL=lisp -nositeinit -noinit -batch
RM=rm -fr
MV=mv -f
MKDIR=mkdir -p

all: cmucl-normal cmucl-safe cmucl-small sbcl clisp
	echo ok

.PHONY: clisp sbcl cmucl-normal cmucl-safe cmucl-small clean

clean:
	$(RM) -rf clisp sbcl cmucl-normal cmucl-small cmucl-safe ecl

PRE_TEST=$(MKDIR) $@ ; \
	$(RM) *.erg $@/*;

POST_TEST=$(MV) *.erg $@ ; \
	ls -l $@ ; \
	cat $@/state-info.erg > $@-ERROR-IDS ; \
	grep -h '^Bugid: ' $@/*.erg  | cut -d ' ' -f 2 >> $@-ERROR-IDS ; \
	true;

clisp:
	$(PRE_TEST)
	time $(CLISP) -i tests.lisp < /dev/null && true
	$(RM) *.fas *.lib ;
	$(POST_TEST)

sbcl:
	$(PRE_TEST)
	time $(SBCL) --eval '(load "tests")'
	$(RM) *.x86f *.fasl ;
	$(POST_TEST)

cmucl-normal:
	$(PRE_TEST)
	time $(CMUCL) -core $(CMUCL_NORMAL) -load tests.lisp
	$(RM) *.x86f ;
	$(POST_TEST)

cmucl-small:
	$(PRE_TEST)
	time $(CMUCL) -core $(CMUCL_SMALL) -load tests.lisp
	$(RM) *.x86f ;
	$(POST_TEST)

cmucl-safe:
	$(PRE_TEST)
	time $(CMUCL) -core $(CMUCL_SAFE) -load tests.lisp
	$(RM) *.x86f ;
	$(POST_TEST)

ecl:
	$(PRE_TEST)
	echo '(load "tests.lisp")' | $(ECL) 
	$(RM) *.fas *.lib ;
	$(POST_TEST)
