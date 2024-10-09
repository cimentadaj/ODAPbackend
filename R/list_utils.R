

#' @title capture_args
#' @description returns list of all arguments passed to a function, such that you could re-call the function using `do.call()`
#' @details If you pass in a `data.frame`, such as `data_in`, the whole thing is copied in the respective argument, which is returned. If you have an object in memory called `data_i`, defined as `data.frame(a=1:3,b=letters[1:3])` and you do `f(data_in = data_i)`, then the output is not `list()`
capture_args <- function(){
  args   <- mget(names(formals()),envir=parent.frame())
  args   <- args[names(args) != "..."]
  extras <- list(...)
  args   <- c(args, extras)
  args
}

f <- function(x=1,y=2,...){
  args<- capture_args()
  output <- x * y
  fun <- match.call() |> as.character()
  out <- list(function_name = fun,
              args = args,
              output = output)
  return(out)
}
f(x=3)
