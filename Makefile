default: ppplib ppplib.so

PS2PDF ?= ps2pdf

ppplib.so: ppplib20.f90
	gfortran -std=legacy -shared -fPIC -c ppplib20.f90 -o libppp20.so

ppplib: ppplib20.o
	ar cr ppplib20.a ppplib20.o
	ranlib ppplib20.a

ppplib20.o: ppplib20.f90
	gfortran -std=legacy -c ppplib20.f90 -o ppplib20.o -fdefault-real-8

test: ppplib ppplib.so ppptest20
	./ppptest20
	$(PS2PDF) plotfile.ps plotfile.pdf
	@echo "open the generated pdf file to see the test results"

ppptest20: ppptest20.f90 ppplib20.f90
	gfortran -std=legacy -fdefault-real-8 ppptest20.f90 ppplib20.f90 -o ppptest20

clean:
	rm -f ppplib20.o
	rm -f ppplib20.a
	rm -f *.mod
	rm -f fort.*
	rm -f *.ps
	rm -f *.pdf
	rm -f ppptest20
