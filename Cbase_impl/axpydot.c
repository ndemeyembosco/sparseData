#include <time.h> 
#include <stdio.h> 
#include <stdlib.h> 
#include <mkl.h> 



double sum (double* arr, int len)
{
    double to_return = 0;
    for(size_t i = 0; i < len; i++) 
    {
        to_return += arr[i];
    }
    return to_return;
}

int main(int argc, const char* argv[])
{
    clock_t start, end;
    double elapsed; 

    int dimension = atoi(argv[1]);
    double* Y     = (double*) malloc(sizeof(double) * dimension);
    double* X     = (double*) malloc(sizeof(double) * dimension);
    double* U     = (double*) malloc(sizeof(double) * dimension); 

    for (size_t i = 0; i < dimension; i++)
    {
        double x = 1.235;
        double y = 0.274; 
        double u = 3.423;
        X[i] = x;
        Y[i] = y;  
        U[i] = u; 
    }

    start = clock(); 
    cblas_daxpy(dimension, 1.0, X, 1, Y, 1); 
    double ans = cblas_ddot(dimension, Y, 1, U, 1);
    end = clock();
    elapsed = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("sum after daxpy: %f \n", sum(Y, dimension));
    printf("residual after axpydot: %f \n", ans);
    printf("elapsed time: %f \n", elapsed);


    free(X);
    free(Y);
    free(U);



}