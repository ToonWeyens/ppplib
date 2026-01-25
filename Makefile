UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  SHLIB_EXT = dylib
  SHLIB_FLAGS = -dynamiclib
else
  SHLIB_EXT = so
  SHLIB_FLAGS = -shared
endif
SHLIB_NAME = libppp20.$(SHLIB_EXT)

default: ppplib shared

PS2PDF ?= ps2pdf

shared: $(SHLIB_NAME)

$(SHLIB_NAME): ppplib20.f90
	gfortran -std=legacy $(SHLIB_FLAGS) -fPIC -fdefault-real-8 ppplib20.f90 -o $(SHLIB_NAME)

ppplib: ppplib20.o
	ar cr ppplib20.a ppplib20.o
	ranlib ppplib20.a

ppplib20.o: ppplib20.f90
	gfortran -std=legacy -c ppplib20.f90 -o ppplib20.o -fdefault-real-8

test: ppplib $(SHLIB_NAME) ppptest20
	./ppptest20
	$(PS2PDF) plotfile.ps plotfile.pdf
	@echo "open the generated pdf file to see the test results"

ppptest20: ppptest20.f90 ppplib20.f90
	gfortran -std=legacy -fdefault-real-8 ppptest20.f90 ppplib20.f90 -o ppptest20

clean:
	rm -f ppplib20.o
	rm -f ppplib20.a
	rm -f libppp20.so
	rm -f libppp20.dylib
	rm -f *.mod
	rm -f fort.*
	rm -f *.ps
	rm -f *.pdf
	rm -f ppptest20
