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

doTest(e) #800
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

