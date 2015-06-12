## Use the *nix ps program to get the memory usage of this R process.
memory.usage <- function(ps.parameter=paste("-p", Sys.getpid())){
  cmd <- sprintf("ps %s -o pid,cmd,rss", ps.parameter)
  ps.lines <- system(cmd, intern=TRUE)
  stopifnot(length(ps.lines) > 1)
  ps.table <- read.table(text=ps.lines, header=TRUE)
  ps.table$megabytes <- ps.table$RSS/1024
  ps.table
}
