#include "mkl.h"
#include <time.h>
#include <stdio.h> 
#include <stdlib.h> 


double sum (double* arr, int len)
{
    double to_return = 0;
    for(size_t i = 0; i < len; i++) 
    {
        to_return += arr[i];
    }
    return to_return;
}


int main(int argc, char* argv[])
{
    clock_t start, end; 
    double elapsed;
    int dimension;

    dimension = atoi(argv[1]); 

    double* X = (double*) malloc(sizeof(double) * dimension);
    double* X_COPY = (double*) malloc(sizeof(double) * dimension);
    double* Y = (double*) malloc(sizeof(double) * dimension);
    double* Z = (double*) malloc(sizeof(double) * dimension);
    double* A = (double*) malloc(sizeof(double) * (dimension * dimension)); 
    double* B = (double*) malloc(sizeof(double) * (dimension * dimension));

    for (size_t i = 0; i < dimension; i++)
    {
        double x = 3.4568; 
        X[i]     = x; 
        X_COPY[i] = x; 
    }

    for (size_t r = 0; r < dimension; r++)
    {
        for (size_t c = 0; c < dimension; c++)
        {
            A[r*dimension+c] = 12.3245; 
            B[r*dimension+c] = 5.43287;
        }
        
    }

    start = clock();
    cblas_dgemv(CblasRowMajor, CblasNoTrans, dimension, dimension, 1.0, A, dimension, X, 1, 0.0, Y, 1);
    cblas_dgemv(CblasRowMajor, CblasNoTrans, dimension, dimension, 1.0, B, dimension, X_COPY, 1, 0.0, Z, 1); 
    cblas_daxpy(dimension, 1.0, Y, 1, Z, 1);
    end = clock();
    elapsed = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("elapsed time: %f \n", elapsed);
    printf("sum after gesummv: %.10e \n", sum(Z, dimension));

    free(X);
    free(Y);
    free(Z);
    free(X_COPY);
    free(A);
    free(B);

}