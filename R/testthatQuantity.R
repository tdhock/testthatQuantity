## Parse the first occurance of pattern from each of several strings
## using (named) capturing regular expressions, returning a matrix
## (with column names).
str_match_perl <- function(string,pattern){
  stopifnot(is.character(string))
  stopifnot(is.character(pattern))
  stopifnot(length(pattern)==1)
  parsed <- regexpr(pattern,string,perl=TRUE)
  captured.text <- substr(string,parsed,parsed+attr(parsed,"match.length")-1)
  captured.text[captured.text==""] <- NA
  captured.groups <- if(is.null(attr(parsed, "capture.start"))){
    NULL
  }else{
    do.call(rbind,lapply(seq_along(string),function(i){
      st <- attr(parsed,"capture.start")[i,]
      if(is.na(parsed[i]) || parsed[i]==-1)return(rep(NA,length(st)))
      substring(string[i],st,st+attr(parsed,"capture.length")[i,]-1)
    }))
  }
  result <- cbind(captured.text,captured.groups)
  colnames(result) <- c("",attr(parsed,"capture.names"))
  result
}

### Named capture regular expression for parsing git log
### --pretty=format:'%H %ci %s' vis str_match_perl.
commit.line.pattern <-
  paste0("(?<SHA1>[0-9a-f]{40})",
         " ",
         "(?<datetime>",
         "(?<year>[0-9]{4})",
         "-",
         "(?<month>[0-9]{2})",
         "-",
         "(?<day>[0-9]{2})",
         " ",
         "(?<hour>[0-9]{2})",
         ":",
         "(?<minute>[0-9]{2})",
         ":",
         "(?<second>[0-9]{2})",
         ")",
         " ",
         "(?<gmt_offset_sign>[-+])",
         "(?<gmt_offset_hours>[0-9]{2})",
         "(?<gmt_offset_minutes>[0-9]{2})",
         " ",
         "(?<subject>.*)")

### Parse a git log character vector, returning data.frame.
commits <- function(git.lines){
  stopifnot(is.character(git.lines))
  m <- str_match_perl(git.lines, commit.line.pattern)
  stopifnot(!is.na(m[,1]))
  commit.time <- strptime(m[,"datetime"], "%Y-%m-%d %H:%M:%S", "GMT") + 0
  gmt.offset.sign <- ifelse(m[,"gmt_offset_sign"]=="+", 1, -1)
  gmt.offset.hours <- as.numeric(m[,"gmt_offset_hours"])
  minutes.only <- as.numeric(m[,"gmt_offset_minutes"])
  gmt.offset.minutes <- minutes.only + gmt.offset.hours * 60
  gmt.offset.seconds <- gmt.offset.minutes * 60
  gmt.offset <- gmt.offset.sign * gmt.offset.seconds
  gmt.time <- commit.time - gmt.offset
  subject <- m[, "subject"]
  SHA1 <- m[, "SHA1"]
  abbrev <- substr(subject, 1, 18)
  commits <- data.frame(SHA1, subject, abbrev, gmt.time,
                        stringsAsFactors = FALSE)
  rownames(commits) <- commits$SHA1
  class(commits) <- c("commits", "data.frame")
  stopifnot(nrow(commits) == length(git.lines))
  commits
}

### Print commits 80 characters per line.
print.commits <- function(commits){
  print(as.data.frame(commits)[, c("gmt.time", "abbrev")])
}

### Get data.frame of all commits involving tfile.
commits.for.file <- structure(function(tfile){
  stopifnot(is.character(tfile))
  stopifnot(length(tfile) == 1)
  testthat.dir <- dirname(tfile)
  old.wd <- setwd(testthat.dir)
  on.exit(setwd(old.wd))
  ## first get data for all commits in this repos.
  all.cmd <- "git log --pretty=format:'%H %ci %s' --all"
  all.txt <- system(all.cmd, intern=TRUE)
  all.commits <- commits(all.txt)
  ## Then get commits just involving this file.
  file.cmd <- paste(all.cmd, tfile)
  file.txt <- system(file.cmd, intern=TRUE)
  last.line <- file.txt[length(file.txt)]
  first.commit <- commits(last.line)
  first.i <- which(first.commit$SHA1 == all.commits$SHA1)
  commits.to.test <- all.commits[1:first.i, ]
  commits.to.test
}, ex=function(){
  tfile <- "~/R/animint/tests/testthat/test-animation.R"
  history <- commits.for.file(tfile)
  time.list <- list()
  n.commits <- 20
  commit.rows <- round(seq(1, nrow(history), l=n.commits))
  commit.rows <- 1:10
  for(commit.i in commit.rows){
    commit <- history[commit.i, ]
    if(! commit$SHA1 %in% names(time.list)){
      time.list[[commit$SHA1]] <- test.commit(tfile, commit$SHA1)
    }
  }
  save(history, time.list, file="../data/animint.timings.RData")
  time.df <- do.call(rbind, time.list)
  time.df$gmt.time <- history[paste(time.df$SHA1), "gmt.time"]
  rownames(time.df) <- NULL
  time.df[order(time.df$gmt.time), c("SHA1", "gmt.time")]
  last <- subset(time.df, gmt.time > ISOdatetime(2015, 2, 15, 23, 30, 0, "GMT"))
  some <- subset(time.df, gmt.time < ISOdatetime(2015, 2, 15, 0, 0, 0, "GMT"))
  to.show <- rbind(last, some)
  library(ggplot2)
  with.legend <- ggplot(to.show, aes(gmt.time, seconds, color=test.name))+
    geom_point(pch=1)+
    ggtitle("Times for two Animint tests, selected commits in 2014-2015")+
    xlab("Time of commit (GMT)")
  library(directlabels)
  direct.label(with.legend)
  ggsave("../Animint-two-tests.png")
})

### Starting from any directory, go to tfile directory and then
### checkout a commit and test it.
test.commit <- function(tfile, SHA1){
  stopifnot(is.character(tfile))
  stopifnot(length(tfile) == 1)
  stopifnot(is.character(SHA1))
  stopifnot(length(SHA1) == 1)
  testthat.dir <- dirname(tfile)
  tfile.base <- basename(tfile)
  old.wd <- setwd(testthat.dir)
  on.exit(setwd(old.wd))
  old.SHA1 <- system("git rev-parse HEAD", intern=TRUE)
  on.exit({
    system(paste("git checkout", old.SHA1))
    setwd(old.wd)
  })
  cmd <- paste("git checkout", SHA1)
  system(cmd)
  pkg <- file.path("..", "..")
  test.df <- tryCatch({
    devtools::load_all(pkg)
    test.file(tfile.base)
  }, error=function(e){
    data.frame(test.name=paste(e), seconds=NA)
  })

  data.frame(SHA1, test.df, row.names=NULL)
}

### Starting from the test file directory, run tests in file.
test.file <- function(tfile, test.repetitions=3){
  stopifnot(is.character(tfile))
  stopifnot(length(tfile) == 1)
  stopifnot(is.numeric(test.repetitions))
  stopifnot(length(test.repetitions) == 1)
  test.repetitions <- floor(test.repetitions)
  stopifnot(is.finite(test.repetitions))
  stopifnot(test.repetitions > 0)
  require(testthat)
  tlines <- readLines(tfile)
  qlines <- sub("test_that(", "testthatQuantity(", tlines, fixed=TRUE)
  qfile <- tempfile()
  writeLines(qlines, qfile)
  test.results <- list()
  testthatQuantity <- function(test.name, code){
    e <- parent.frame()
    code.subs <- substitute(code)
    run <- function(){
      testthat:::test_code(test.name, code.subs, env=e)
    }
    seconds <- if(require(microbenchmark)){
      times <- microbenchmark(test={
        run()
      }, times=test.repetitions)
      times$time/1e9
    }else{
      replicate(test.repetitions, {
        time.vec <- system.time({
          run()
        })
        time.vec[["elapsed"]]
      })
    }
    time.df <- data.frame(test.name, seconds)
    test.results[[test.name]] <<- time.df
  }
  source(qfile, local=TRUE)
  do.call(rbind, test.results)
}
