all:
	cabal new-build axpy axpydot bicgk gemv gemvt gesummv trilazy 

clean:
	rm pbenchmark.* execs/*