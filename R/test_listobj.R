

capture_args <- function(){
  # parents <- check_parents()
  call_up <-new.env(hash = TRUE, parent = parent.frame(n=1), size = 29L)
  # call_up <-  parent.env(sys.nframe()-1)
  args <- mget(names(formals(envir = call_up)), envir = call_up, inherits=TRUE)
  args <- args[names(args) != "..."]
  extras <- list(...)
  args <- c(args, extras)
  args

}
f <- function(x=1,y=2,...){
  out <- x+y
  args <- capture_args()
  return(list(out, args))
}
f()
given_args <- mget(names(formals()),sys.frame(sys.nframe()))

get_args <- function()
{
  cl <- sys.call(-1)
  f <- get(as.character(cl[[1]]), mode="function", sys.frame(-2))
  cl <- match.call(definition=f, call=cl)
  as.list(cl)[-1]
}
get_args <- function () {
  args <- match.call(
    def = sys.function( 1 ),
    call = sys.call(1))
  args
}

f <- function(x=1,y=2,...){
  out <- x+y
  args <- capture_args()
  return(list(out, args))
}
f()
check_parents <- function(){
  sys.parents()
}
g <- function(x=1,y=2,...){
  check_parents()
}
g()

