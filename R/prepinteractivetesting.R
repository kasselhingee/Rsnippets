#' Short code to prepare environment for running content of testthat tests interactively
#' @description I keep getting bugs using C++ and RStudio/devtools built in methods for testing. Something how things compile and not finding C++ objects. This little function seems to get around the problem largely by installing the package differently, if at all.
#' @details Uses `attach()`. Perhaps I could use something like `devtools::load_all(compile = TRUE)`
#' @param path Directory of package. If this fails to get a package name, the it will try using path as the package name.
#' @export
prepinteractivetesting <- function(path, ..., install = TRUE, build = TRUE){
  if (install){devtools::install(path, build = build)
  name <- tryCatch(pkgload::pkg_name(path = path),
                   error = function(e){path})
  attach(loadNamespace(name), name = name)
  library(testthat)
}
