runt.e:
	gfortran -O3 humppa_translate2.f90 -o runt.e

runt.e.debug:
	gfortran -O3 humppa_translate2.f90 -o runt.e.debug

debug: runt.e.debug
