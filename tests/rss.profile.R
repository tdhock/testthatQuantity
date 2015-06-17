library(testthatQuantity)

m.size <- 10240L

e <- new.env()
L <- list()
m.size <- 10240L

fun.with.possible.memory.side.effect <- function(e.or.L){
  e.or.L$m <- matrix(5, m.size, m.size)
}

doTest <- function(e.or.L){
  e.or.L$m <- NULL
  rss.profile.start("e.or.L.RSS")
  fun.with.possible.memory.side.effect(e.or.L)
  rss.profile.stop("e.or.L.RSS")
}

rss.lists <- list()

rss.lists$e <- doTest(e)

rss.lists$L <- doTest(L)

rss.profile.start("leak_matrix.RSS")
.C("leak_matrix",
   m.size,
   PACKAGE="testthatQuantity")
rss.lists$leak <- rss.profile.stop("leak_matrix.RSS")

rss.profile.start("free_matrix.RSS")
.C("free_matrix",
   m.size,
   PACKAGE="testthatQuantity")
rss.lists$free <- rss.profile.stop("free_matrix.RSS")

rss.row.list <- list()
for(test.name in names(rss.lists)){
  info <- rss.lists[[test.name]]
  rss.row.list[[test.name]] <- with(info, data.frame(leak, swap))
}

(rss.df <- do.call(rbind, rss.row.list))
