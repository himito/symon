all:
						cd src; make

clean:
						cd src; make clean

cleanall:
						cd src; make cleanall
						rm -rf *.native
						rm -rf *.pdf
						rm -rf *.dot
						rm -rf *.byte
