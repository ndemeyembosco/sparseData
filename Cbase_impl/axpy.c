#include <time.h>
#include <stdio.h> 
#include <stdlib.h> 
#include <cblas.h> 



double drand (double low, double high)
{
    return ((double) rand() * (high - low)) / ((double) RAND_MAX + 1); 
}

int main(int argc, char* argv[])
{
    clock_t start, end; 
    double elapsed; 

    int dimension = atoi(argv[1]);
    int max_rand  = atoi(argv[2]);

    double* X = (double*) malloc(sizeof(double) * dimension); 
    double* Y = (double*) malloc(sizeof(double) * dimension); 

    for (size_t i = 0; i < dimension; i++)
    {
        double x = drand(1, max_rand); 
        double y = drand(1, max_rand); 
        X[i] = x;
        Y[i] = y;  
    }
    

    start = clock();  
    cblas_daxpy(dimension, 1.0, X, 1, Y, 1); 
    end = clock(); 
    elapsed = ((double) (end - start)) / CLOCKS_PER_SEC; 
    printf("elapsed time: %f \n", elapsed); 
    // printf("first element: %f \n", ans[0]); 

    free(X); 
    free(Y);
}