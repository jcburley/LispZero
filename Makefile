SOURCEFILES=lisp-zero.c zero.lisp zero-test.lisp zero-test.gold
BUILDFILES=Makefile .gdbinit
RESULTFILES=zero-test.out

all: lisp-zero zero-test

lisp-zero: lisp-zero.c map.c
	gcc -g -O0 -Wall -W -I/usr/share/gnulib/lib -o lisp-zero lisp-zero.c map.c

zero-test: zero-test.gold zero-test.out
	diff -u zero-test.gold zero-test.out

zero-test.out: lisp-zero zero-test.lisp
	./lisp-zero < zero-test.lisp > zero-test.out

zero-new-gold: zero-test.out
	rm -f zero-test.gold
	cp zero-test.out zero-test.gold
	chmod a-w zero-test.gold

snap:
	$(MAKE) $(MAKEFILE) all || exit 0
	mkdir -p builds
	$(eval $@_TMP := $(shell mktemp -d --tmpdir=builds))
	@echo hi $($@_TMP)/hi.txt
	cp -a $(SOURCEFILES) $(BUILDFILES) $(RESULTFILES) $($@_TMP)
