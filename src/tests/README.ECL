This test-suite has been revised to work with ECL. To use it, just unpack
the tar-ball on top of the latest ECL release and perform a

	cd build
	cd tests
	make

You will find two types of output files:

	*.out	The whole output of a test
	*.erg	The errors arising from a test

When there is no *.erg, it means that ECL passed the test.
