LISP = clisp
RM = rm -f

all : clean tests compare

clean : force
	$(RM) *.erg

tests : force
	$(LISP) -i tests.lsp -x '(run-all-tests)'

compare : force
	(echo *.erg | grep '\*') >/dev/null || (echo "Test failed." ; echo "To see which tests failed, type" ; echo "    cat *.erg" ; exit 1)
	echo "Test passed."

force :

