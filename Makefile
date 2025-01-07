runt.e: humppa_translate2.f90
	gfortran -O3 humppa_translate2.f90 -o runt.e

runt.e.debug: humppa_translate2.f90
	gfortran -O3 -g -pg humppa_translate2.f90 -o runt.e.debug

debug: runt.e.debug
