all: clear run

compile_to_c: iexpr.scm
	gsc -link iexpr.scm
compile: compile_to_c
	gcc -o igs -O1 iexpr.c iexpr_.c -lgambc -lm -ldl -lutil
cleanc: compile
	rm iexpr.c iexpr_.c
run: cleanc
	./igs load test.ism
clear:
	clear
install: cleanc
	cp ./igs /usr/bin/

ctest:
	gsc -link ctest.scm
	gcc -o ctest -D___SINGLE_HOST -O1 ctest.c ctest_.c -lgambc -lm -ldl -lutil
	rm ctest.c ctest_.c
	./ctest
