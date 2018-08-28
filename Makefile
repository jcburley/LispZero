SOURCEFILES=lisp-zero.c zero.lisp zero-test.lisp zero-test.gold
BUILDFILES=Makefile .gdbinit
RESULTFILES=zero-test.out

all: lisp-zero lisp-zero-single zero-test

lisp-zero: lisp-zero.c map.c
	gcc -g -O0 -Wall -W -I/usr/share/gnulib/lib -o lisp-zero lisp-zero.c map.c

lisp-zero-single: lisp-zero-single.c
	gcc -g -O0 -Wall -W -I/usr/share/gnulib/lib -o lisp-zero-single lisp-zero-single.c

zero-test: zero-test.gold zero-test.out
	diff -u zero-test.gold zero-test.out

zero-test.out: lisp-zero zero-test.lisp
	./lisp-zero < zero-test.lisp > zero-test.out

zero-single-test: zero-test.gold zero-single-test.out
	diff -u zero-test.gold zero-single-test.out

zero-single-test.out: lisp-zero-single zero-test.lisp
	./lisp-zero-single < zero-test.lisp > zero-single-test.out

zero-new-gold: zero-test.out
	rm -f zero-test.gold
	cp zero-test.out zero-test.gold
	chmod a-w zero-test.gold

go: lisp-zero-single.go

lisp-zero-single.go: SINGLE-TEST-PASSED.stamp lisp-zero-single.c
	c2go transpile lisp-zero-single.c

SINGLE-TEST-PASSED.stamp: zero-single-test.out
	touch SINGLE-TEST-PASSED.stamp

snap:
	$(MAKE) $(MAKEFILE) all || exit 0
	mkdir -p builds
	$(eval $@_TMP := $(shell mktemp -d --tmpdir=builds))
	@echo hi $($@_TMP)/hi.txt
	cp -a $(SOURCEFILES) $(BUILDFILES) $(RESULTFILES) $($@_TMP)

clean:
	rm -f lisp-zero lisp-zero-single zero-test.out zero-single-test.out

.PHONY: all clean zero-test zero-single-test zero-new-gold snap go
