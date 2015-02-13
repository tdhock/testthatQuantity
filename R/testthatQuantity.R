test.results <- list()
test.repetitions <- 3

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

print.commits <- function(commits){
  print(as.data.frame(commits)[, c("gmt.time", "abbrev")])
}

test.file <- function(tfile){
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
}
