.stp25Env <- new.env()
assign("stp25.options", list(), envir = .stp25Env)
.onLoad <- function(libname, pkgname) {
  stp25.options(.defaultstp25Options())
}
