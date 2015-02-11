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
