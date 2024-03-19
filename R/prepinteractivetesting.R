#' Prepare for running content of testthat tests interactively
#' @description 
#' This installs a local package using `devtools::install()` then attaches the namespace of the package and `testthat`.
#' I'm not sure why this led to different behaviour than `devtools::test()` and `devtools::load_all()`.
#' I think `devtools::test()` may compile C++ without the `NDEBUG` flag so that any assertions in the C++ code get thrown and cause R to abort.
#' @details Uses `attach()`. Perhaps I could use something like `devtools::load_all(compile = TRUE)`
#' @param path Directory of package. If this fails to get a package name, the it will try using path as the package name.
#' @export
prepinteractivetesting <- function(path = ".", ..., install = TRUE, build = TRUE){
  if (install){devtools::install(path, build = build)}
  name <- tryCatch(pkgload::pkg_name(path = path),
                   error = function(e){path})
  attach(loadNamespace(name), name = name)
  attach(loadNamespace("testthat"), name = "testthat")
}
