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
Rprof("e.Rprof", memory.profiling=TRUE)
doTest(e) #800
Rprof(NULL)
getProf <- function(f){
  df <- summaryRprof(f, memory="tseries", diff=FALSE)
  df$bytes <- with(df, vsize.small + vsize.large)
  df$seconds <- as.numeric(rownames(df))
  df
}
e.Rprof <- getProf("e.Rprof")
ggplot()+
  geom_line(aes(seconds, bytes/1024^2), data=e.Rprof)

## Executing the following line of code should cause a brief 800MB
## spike in the memory usage, after which memory usage returns to what
## it was before calling it.
Rprof("L.Rprof", memory.profiling=TRUE)
doTest(L) #0
Rprof(NULL)
L.Rprof <- getProf("L.Rprof")
ggplot()+
  geom_line(aes(seconds, bytes/1024^2), data=L.Rprof)

## Careful: the following function leaks 800MB of RAM (which are not
## recoverable until you quit R)!
cmd <-
  paste("bash",
        "~/R/testthatQuantity/exec/rss.sh",
        "leak_matrix.RSS",
        Sys.getpid())
gc()
system(cmd, wait=FALSE)
Sys.sleep(1)
Rprof("leak_matrix.Rprof", memory.profiling=TRUE)
.C("leak_matrix",
   m.size,
   PACKAGE="testthatQuantity")
Rprof(NULL)
system("touch leak_matrix.RSS.DONE")

## The following function does not leak memory, but it still would be
## nice to be able to quantify the fact that it uses a maximum of
## 800MB of RAM during the course of its execution.
cmd <-
  paste("bash",
        "~/R/testthatQuantity/exec/rss.sh",
        "free_matrix.RSS",
        Sys.getpid())
gc()
system(cmd, wait=FALSE)
Sys.sleep(1)
Rprof("free_matrix.Rprof", memory.profiling=TRUE)
.C("free_matrix",
   m.size,
   PACKAGE="testthatQuantity")
Rprof(NULL)
system("touch free_matrix.RSS.DONE")

## Read profiling files.
kilobytes <- scan("leak_matrix.RSS", what=integer())
plot(kilobytes)
(max(kilobytes) - kilobytes[1])
(kilobytes[length(kilobytes)]-kilobytes[1])
kilobytes <- scan("free_matrix.RSS", what=integer())
plot(kilobytes)
(max(kilobytes) - kilobytes[1])
(kilobytes[length(kilobytes)]-kilobytes[1])

## These plots clearly show that we can NOT use Rprof to track memory
## usage outside of R allocations.
leak.Rprof <- getProf("leak_matrix.Rprof")
ggplot()+
  ylab("kilobytes")+
  geom_line(aes(seconds, bytes/1024), data=leak.Rprof)
free.Rprof <- getProf("free_matrix.Rprof")
ggplot()+
  ylab("kilobytes")+
  geom_line(aes(seconds, bytes/1024), data=free.Rprof)

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
