CC = gcc

unexe	: unexe.c
	$(CC) -static -o unexe -g unexe.c ../../libcrs.a

ecoffdump:	ecoffdump.c
	$(CC) -o ecoffdump ecoffdump.c

coffdump:	coffdump.c
	$(CC) -o coffdump coffdump.c -lld

test-dld:	test-dld.c
	$(CC) -o test-dld test-dld.c

dld:	dld.c
	$(CC) -o dld dld.c -lld

try:	try.c
	$(CC) -o try try.c -D${MACHINE}
