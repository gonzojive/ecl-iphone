# Abstract
ECL-iphone is a version of Embedable Common Lisp that compiles for the ARM architecture and iPhone OS.
Currently, this library successfully compiles and runs on the iPhone, allowing you to run Common
Lisp programs on iPhone OS.  This works for phones that are NOT jailbroken (and presumably those that
are).

See the origin blog post announcing ECL for iphone [here](http://lambdajive.wordpress.com/2009/03/27/common-lisp-on-iphone-ecl-comes-through-at-last/ "Common Lisp on iPhone: ECL comes through at last!").

# Installation
cd ecl
sh make-iphone

The library files should be installed in install_iPhoneOS/lib/.  If they are not, then something 
probably went wrong during the installation process.  The build script is pretty dumb, so it will
keep going even if individual steps encounter errors.  So if you get problems look for the error
highest up in the output.

# Missing Features
THIS VERSION OF ECL does not have any extra bells and whistles.  It is built without threads,
bignum support, and ASDF.  If you add any of these features, let me or the ECL list know so we can
all share the wealth.

## ASDF
ASDF is essential for modern lisp programming, but it is currently missing. The reason is that ECL's
method of compiling (running gcc) is unavailable from iPhone itself.  As a result, the functions
COMPILE and COMPILE-FILE do not currently work.  ASDF requires these operations, and so ASDF
does not work.

Some notes on the ASDF issue from the ecl mailing list:

Red Daly:
> However, I still don?t have all the bells and whisltes working correctly.
> My current problem is getting ASDF to properly compile into ECL.  I cannot
> get ECL to build, even for the i386 Darwin non-cross-compiled version of
> ECL, when I have ASDF enabled and the ?disable-shared flag set in the
> configuration script.  [...]
> Any idea what?s going on here?  The error message doesn?t really tell me
> much about what?s going wrong.  I am even more confused about why the
> ?disable-shared flag makes any difference.

Juan Jose Garcia-Ripoll (the main ECL man):
?disable-shared means you cannot compile code and load it. That is
why ASDF is not building. My question now is, does you iPhone have a C
compiler? If not then you will have to install a fake version of
COMPILE-FILE and COMPILE and try to teach ASDF to use it. It might be
a nice addition to ECL for environments without a C compiler but, as I
said, it is fake: there will be no compilation going on.

