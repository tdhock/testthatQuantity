library(testthatQuantity)

e <- new.env()
L <- list()
m.size <- 10240L

fun.with.possible.memory.side.effect <- function(e.or.L){
  e.or.L$m <- matrix(5, m.size, m.size)
}

doTest <- function(e.or.L){
  e.or.L$m <- NULL
  gc()
  before <- memory.usage()
  fun.with.possible.memory.side.effect(e.or.L)
  during <- memory.usage()
  gc()
  after <- memory.usage()
  print(rbind(before=before, during=during, after=after))
  after$megabytes - before$megabytes
}

## The first time you execute the following line of code, the memory
## usage of your R process should go up by 800MB.
doTest(e) #800

## Executing the following line of code should cause a brief 800MB
## spike in the memory usage, after which memory usage returns to what
## it was before calling it.
doTest(L) #0

## Careful: the following function leaks 800MB of RAM (which are not
## recoverable until you quit R)!
.C("leak_matrix",
   m.size,
   PACKAGE="testthatQuantity")

## The following function does not leak memory, but it still would be
## nice to be able to quantify the fact that it uses a maximum of
## 800MB of RAM during the course of its execution.
.C("free_matrix",
   m.size,
   PACKAGE="testthatQuantity")

## example from Akash 16 June 2015.
print(gc(reset = T))
before <- memory.usage()

##source(temp_file, local = TRUE)
a <- matrix(5, nrow = m.size, ncol = m.size)

print(gc(reset = T))
during <- memory.usage()

remove(a)

print(gc(reset = T))
after <- memory.usage()

test_results <- rbind(before = before, during = during, after = after)
test_results
