.stp25Env <- new.env()

assign("stp25.options", list(), envir = .stp25Env)

assign("stp25.language", list(), envir = .stp25Env)

## assign("last.object",     NULL,   envir = .stp25Env)

.onLoad <- function(libname, pkgname){
  ## library.dynam("stp25", pkgname, libname )
  stp25.options(.defaultstp25Options())
  stp25.language(.defaultstp25Language(language="de"))

 # lattice::trellis.par.set(bw_theme())

  # if (!"package:lattice" %in% search()){
  #   lattice::lattice.options(default.theme = effectsTheme)
  #   packageStartupMessage("lattice theme set by effectsTheme()",
  #                         "\nSee ?effectsTheme for details.")
  # }
  # else packageStartupMessage("Use the command",
  #                            "\n    lattice::trellis.par.set(effectsTheme())",
  #                            "\n  to customize lattice options for effects plots.",
  #                            "\nSee ?effectsTheme for details.")

 # lattice::trellis.par.set(effects::effectsTheme())
}


# .onUnload <- function(libpath){
#   library.dynam.unload("stp25", libpath)
#   }




