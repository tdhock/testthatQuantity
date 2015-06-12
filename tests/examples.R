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
  print(memory.usage())
  fun.with.possible.memory.side.effect(e.or.L)
  print(memory.usage())
  gc()
  print(memory.usage())
}

doTest(e)
doTest(L)

.C("leak_matrix",
   m.size,
   PACKAGE="testthatQuantity")

.C("free_matrix",
   m.size,
   PACKAGE="testthatQuantity")

