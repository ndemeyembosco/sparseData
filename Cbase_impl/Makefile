CC=icc 
LDFLAGS= -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_intel_ilp64.a ${MKLROOT}/lib/intel64/libmkl_intel_thread.a ${MKLROOT}/lib/intel64/libmkl_core.a -Wl,--end-group -liomp5 -lpthread -lm -ldl
CFLAGS=-Ofast -DMKL_ILP64  -I"${MKLROOT}/include"

gemv : 
	$(CC) -o gemv gemv.c $(CFLAGS) $(LDFLAGS)   

axpy : 
	$(CC) -o axpy axpy.c $(CFLAGS) $(LDFLAGS) 

all : axpy gemv 

clean: 
	rm axpy gemv 