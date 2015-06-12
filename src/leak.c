/* -*- compile-command: "R CMD INSTALL .." -*- */

#include <stdlib.h>
#include <stdio.h>

void leak_matrix(int *n){
  int bytes = (*n) * (*n) * sizeof(double);
  double *ptr = (double*) malloc(bytes);
  for(int i=0; i < *n; i++){
    for(int j=0; j < *n; j++){
      ptr[i + (*n) * j] = i * j;
    }
  }
  printf("leaked %d bytes at %p\n", bytes, ptr);
}

void free_matrix(int *n){
  int bytes = (*n) * (*n) * sizeof(double);
  double *ptr = (double*) malloc(bytes);
  for(int i=0; i < *n; i++){
    for(int j=0; j < *n; j++){
      ptr[i + (*n) * j] = i * j;
    }
  }
  free(ptr);
  printf("freed %d bytes at %p\n", bytes, ptr);
}
