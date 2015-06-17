rss.profile.start <- function(rss.file){
  stopifnot(is.character(rss.file))
  stopifnot(length(rss.file) == 1)
  sh.file <- system.file("exec", "rss.sh", package="testthatQuantity")
  cmd <- paste("bash", sh.file, rss.file, Sys.getpid())
  gc()
  system(cmd, wait=FALSE)
  ## while({
  ##   rss.size <- file.info(rss.file)$size
  ##   is.na(rss.size) || rss.size == 0
  ## }){
  ##   ## wait for the system(cmd) to start writing to rss.file.
  ##   cat("Waiting for rss.sh to start writing to ", rss.file, "\n")
  ## }
  Sys.sleep(1)
}

rss.profile.stop <- function(rss.file){
  stopifnot(is.character(rss.file))
  stopifnot(length(rss.file) == 1)
  DONE.file <- paste0(rss.file, ".DONE")
  gc()
  Sys.sleep(1)
  cat("", file=DONE.file)
  kilobytes <- scan(rss.file, what=integer(), quiet=TRUE)
  list(kilobytes.over.time=kilobytes,
       swap=max(kilobytes) - kilobytes[1],
       leak=kilobytes[length(kilobytes)]-kilobytes[1])
}


