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
    clock_t start1, end1, start, end; 
    double elapsed; 
    int dimension; 
    int max_rand; 
    
    start1 = clock();
    dimension = atoi(argv[1]);
    max_rand  = atoi(argv[2]);

    double* X = (double*) malloc(sizeof(double) * dimension); 
    double* Y = (double*) malloc(sizeof(double) * dimension); 
    double* A = (double*) malloc(sizeof(double) * (dimension * dimension));

    for (size_t i = 0; i < dimension; i++)
    {
        double x = drand(1, max_rand); 
        double y = drand(1, max_rand); 
        X[i] = x;
        Y[i] = y;  
    }

    printf("done generating vector \n");

    for (size_t r = 0; r < dimension; r++)
    {
        for (size_t c = 0; c < dimension; c++)
        {
            A[r*1000+c] = drand(1, max_rand); 
        }
        
    }
    
    

    start = clock();  
    cblas_dgemv(CblasRowMajor, CblasNoTrans, dimension, dimension, 1.0, A, dimension, X, 1, 1.0, Y, 1); 
    end = clock(); 
    elapsed = ((double) (end - start)) / CLOCKS_PER_SEC; 
    printf("elapsed time: %f \n", elapsed); 

    free(X); 
    free(Y);
    free(A); 
    end1 = clock();
    
    double elapsed1 = ((double) (end1 - start1) / CLOCKS_PER_SEC);
    printf("time total: %f \n", elapsed1);
}