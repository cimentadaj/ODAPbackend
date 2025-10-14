# .onLoad = function (libname, pkgname) {
#   assign('single_hmd_spain', value, envir = topenv())
# }

# which functions should be updated (apparently one)
library(codetools)

find_callers <- function(pkg = "DemoTools", fun_name = "downloadnLx") {
  ns <- asNamespace(pkg)
  funs <- ls(ns)
  
  users <- funs[sapply(funs, function(f) {
    obj <- get(f, envir = ns)
    if (is.function(obj)) {
      globals <- codetools::findGlobals(obj, merge = FALSE)$functions
      fun_name %in% globals
    } else
      FALSE
  })]
  
  users
  
}

# Example:
find_callers("DemoTools", "downloadnLx")
find_callers("DemoTools", "downloadAsfr")
find_callers("DemoTools", "downloadSRB")