FILES=  boyer browse  ctak dderiv deriv destru-mod destru  div2 fft-mod \
	fft fprint fread frpoly  puzzle-mod puzzle stak \
	tak-mod tak takl takr tprint traverse triang-mod triang

REPEAT ?= 1
LISP ?= ecl
RESULTS ?= times

all:
	make compile
	@ echo >> $(RESULTS)
	@ echo "--------------   SESSION   ------------------" >> $(RESULTS)
	@ echo >> $(RESULTS)
	@ echo "      " For $(LISP) Common Lisp  >> $(RESULTS)
	@ date >> $(RESULTS)
	make -i test

compile:
	for v in $(FILES) ; do \
	echo "(load \"make-declare.lsp\")(si::proclaim-file \"$$v.cl\")" \
	"(compile-file \"$$v.cl\")" | $(LISP) ; done


test:
	for v in $(FILES) ; do \
	echo "(load \"test-help.lsp\")(do-test \"$$v\" \"$(RESULTS)\")" \
	| $(LISP); \
	done

clean:
	rm -f core *.data *.c a.out *.o *.lbin *.bin *.*fasl *~ *.x86f *.fas *.lib *#

