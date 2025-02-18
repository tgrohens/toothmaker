SOURCES=coreop2d.f90 esclec.f90 toothmaker.f90


runt.e: $(SOURCES)
	gfortran -O3 $(SOURCES) -o runt.e

runt.e.debug: $(SOURCES)
	gfortran -O3 -g -pg $(SOURCES) -o runt.e.debug

debug: runt.e.debug
