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
    clock_t start1, end1, start, end; 
    double elapsed; 
    int dimension; 
    
    start1 = clock();
    dimension = atoi(argv[1]);

    double* X = (double*) malloc(sizeof(double) * dimension); 
    double* Y = (double*) malloc(sizeof(double) * dimension); 
    double* A = (double*) malloc(sizeof(double) * (dimension * dimension));

    for (size_t i = 0; i < dimension; i++)
    {
        double x = 3.4568; 
        double y = 1.2345; 
        X[i] = x;
        Y[i] = y;  
    }

    for (size_t r = 0; r < dimension; r++)
    {
        for (size_t c = 0; c < dimension; c++)
        {
            A[r*1000+c] = 12.3245; 
        }
        
    }
    printf("sum X before dgemv: %f \n", sum(X, dimension));
    printf("sum Y before dgemv: %f \n", sum(Y, dimension));
    printf("sum A before dgemv: %f \n", sum(A, dimension));
    
    

    start = clock();  
    cblas_dgemv(CblasRowMajor, CblasNoTrans, dimension, dimension, 1.0, A, dimension, X, 1, 1.0, Y, 1); 
    end = clock(); 
    elapsed = ((double) (end - start)) / CLOCKS_PER_SEC; 
    printf("sum after dgemv: %.10e \n", sum(Y, dimension));
    printf("elapsed time: %f \n", elapsed); 

    free(X); 
    free(Y);
    free(A); 
    end1 = clock();
    
    double elapsed1 = ((double) (end1 - start1) / CLOCKS_PER_SEC);
    printf("time total: %f \n", elapsed1);
}