library(testthatQuantity)

memory.usage()
m.size <- 10240L
m <- matrix(5, m.size, m.size)
print(object.size(m))
memory.usage()
rm(m)
gc()
memory.usage()

.C("leak_matrix",
   m.size,
   PACKAGE="testthatQuantity")

.C("free_matrix",
   m.size,
   PACKAGE="testthatQuantity")

