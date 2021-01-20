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

    double* U = (double*) malloc(sizeof(double) * dimension);
    double* R = (double*) malloc(sizeof(double) * dimension);
    double* R1 = (double*) malloc(sizeof(double) * dimension);
    double* R2 = (double*) malloc(sizeof(double) * dimension);
    double* R3 = (double*) malloc(sizeof(double) * dimension);
    double* W = (double*) malloc(sizeof(double) * dimension * dimension);
    double* Y = (double*) malloc(sizeof(double) * dimension * dimension);

    for (size_t i = 0; i < dimension; i++)
    {
        double x = 3.4568; 
        U[i]     = x; 
    }

    for (size_t r = 0; r < dimension; r++)
    {
        for (size_t c = 0; c < dimension; c++)
        {
            W[r*dimension+c] = 12.3245; 
            Y[r*dimension+c] = 5.43287;
        }
        
    }

    start = clock();
    cblas_dgemv(CblasRowMajor, CblasTrans, dimension, dimension, 1.0, Y, dimension, U, 1, 0.0, R, 1); 
    cblas_dgemv(CblasRowMajor, CblasNoTrans, dimension, dimension, 1.0, W, dimension, R, 1, 0.0, R1, 1); 
    cblas_dgemv(CblasRowMajor, CblasTrans, dimension, dimension, 1.0, W, dimension, U, 1, 0.0, R2, 1);
    cblas_dgemv(CblasRowMajor, CblasNoTrans, dimension, dimension, 1.0, Y, dimension, R2, 1, 0.0, R3, 1);
    cblas_daxpy(dimension, -1.0, R1, 1, R3, 1);
    end = clock();

    elapsed = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("elapsed time: %f \n", elapsed);
    printf("sum after trilazy: %.10e \n", sum(R3, dimension));

    free(U);
    free(R1);
    free(R2);
    free(R3);
    free(W); 
    free(Y);





}