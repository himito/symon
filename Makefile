all:
	cd src; make

test: all
	cd tests; make

clean:
	cd src; make clean
	cd tests; make clean

cleanall:
	cd src; make cleanall
	rm -rf *.native
	rm -rf *.pdf
	rm -rf *.dot
	rm -rf *.byte
