#' Prepare for running content of testthat tests interactively
#' @description 
#' This installs a local package using `devtools::install()` then attaches the namespace of the package and `testthat`.
#' I'm not sure why this led to different behaviour than `devtools::test()`, but I think `devtools::test()` may compile C++ without the `NDEBUG` flag so that any assertions in the C++ code get thrown and cause R to abort.
#' @details Uses `attach()`. Perhaps I could use something like `devtools::load_all(compile = TRUE)`.
#' I suspect a difficulty with repeatability is that the C++ shared library for the package (a `.so` file) is automatically only regenerated if the source files change. So the issue is that behaviour of the C++ asserts depends on whether the most recent compiling was by `devtools::test()` or something else.
#' @param path Directory of package. If this fails to get a package name, the it will try using path as the package name.
#' @export
prepinteractivetesting <- function(path = ".", ..., install = TRUE, build = TRUE){
  if (install){devtools::install(path, build = build)}
  name <- tryCatch(pkgload::pkg_name(path = path),
                   error = function(e){path})
  attach(loadNamespace(name), name = name)
  attach(loadNamespace("testthat"), name = "testthat")
}

#' @export
pt <- function(){prepinteractivetesting(install = FALSE)}
#' @export
pti <- function(){prepinteractivetesting(install = TRUE)}
