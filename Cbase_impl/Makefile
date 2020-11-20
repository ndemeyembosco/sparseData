

gemv : 
	gcc -o gemv gemv.c -O2 -I/usr/local/opt/openblas/include -L/usr/local/opt/openblas/lib -lopenblas 

axpy : 
	gcc -o gemv gemv.c -O2 -I/usr/local/opt/openblas/include -L/usr/local/opt/openblas/lib -lopenblas 

all : 
	axpy 
	gemv 

clean: 
	rm axpy gemv 