gfortran -c -g -Wall -Wextra -Warray-temporaries -Wconversion -fimplicit-none -fbacktrace -ffree-line-length-0 -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow -finit-real=nan lr.f95 2>err
gfortran -w loadspectrum.f lr.o
a.exe <i.dat >out.dat
