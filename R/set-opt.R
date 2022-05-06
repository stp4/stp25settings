#' Settings
#'
#' @name set_opt
#' @param ... parameter
#'
#' @return string
#' @export
#'
set_opt <- function(...) {
  new <- list(...)
  names(new) <-
    dplyr::recode(names(new), mean = "mittelwert", percent = "prozent")
  if (any(names(new) == "fig_folder"))
    new$fig_folder <-
    paste0(clnsng(new$fig_folder), .Platform$file.sep)

  if (any(names(new) == "html_folder"))
    new$html_folder <-
    paste0(clnsng(new$html_folder), .Platform$file.sep)

  stp25.options(new)
}


stp25.options <- function(...){
  ## this would have been really simple if only form allowed were
  ## stp25.options("foo", "bar") and
  ## stp25.options(foo=1, bar=2). But it could also be
  ## stp25.options(foo=1, "bar"), which makes some juggling necessary

  new <- list(...)
  if (is.null(names(new)) && length(new) == 1 && is.list(new[[1]]))
    new <- new[[1]]

  old <-   get("stp25.options", envir = .stp25Env)  # get_opt() #  .stp25Env$stp25.options
  ## any reason to prefer get("stp25.options", envir = .stp25Env)?

  ## if no args supplied, returns full options list
  if (length(new) == 0)
    return(old)

  nm <- names(new)
  if (is.null(nm))
    return(old[unlist(new)]) ## typically getting options, not setting

  isNamed <- nm != "" ## typically all named when setting, but could have mix
  if (any(!isNamed)) nm[!isNamed] <- unlist(new[!isNamed])

  ## so now everything has non-"" names, but only the isNamed ones should be set
  ## everything should be returned, however

  retVal <- old[nm]
  names(retVal) <- nm
  nm <- nm[isNamed]

  ## this used to be

  ## modified <- updateList(retVal[nm], new[nm])
  ## .stp25Env$stp25.options[names(modified)] <- modified

  ## but then calling stp25.options(foo = NULL) had no effect
  ## because foo would be missing from modified.  So, we now do:


  ## but then calling stp25.options(foo = NULL) had no effect
  ## because foo would be missing from modified.  So, we now do:

  .stp25Env$stp25.options <- updateList(old, new[nm])

  ## return changed entries invisibly
  invisible(retVal)
}








#' @rdname set_opt
#' @param name,type spezifikation was gesucht ist 'prozent', und 'digits'
#' @export
get_opt <- function(name = NULL, type = NULL) {
   opt <-
     if(!is.null(name)) get("stp25.options", envir = .stp25Env)[[name]]
     else get("stp25.options", envir = .stp25Env)

  if (is.null(type)) opt
  else opt[[type]]
}


#' @rdname set_opt
#' @export
default_stp25 <- function(type, name) {
  ## altlast weil bei der alten Version das so war.
  if (is.null(type))  return(NULL)
  get_opt(name, type)
}


#' Recursively Modify Elements of a List
#'
#' @noRd
updateList <- function (x, val) {
  if (is.null(x))
    x <- list()
  utils::modifyList(x, val)
}








clnsng <-
  function(x) {
    x <- gsub("\u00e4","ae", x)
    x <- gsub("\u00fc","ue", x)
    x <- gsub("\u00f6","oe", x)
    x <- gsub("\u00dc","Ue", x)
    x <- gsub("\u00c4","Ae", x)
    x <- gsub("\u00d6","Oe", x)
    x <- gsub("\u00df","ss", x)
    x <- gsub(" ", "_", x)

    gsub("[^[:alnum:]_ ]", "", x)
  }













# stp25.options <- function(...){
#   ## this would have been really simple if only form allowed were
#   ## stp25.options("foo", "bar") and
#   ## stp25.options(foo=1, bar=2). But it could also be
#   ## stp25.options(foo=1, "bar"), which makes some juggling necessary
#
#   new <- list(...)
#   if (is.null(names(new)) && length(new) == 1 && is.list(new[[1]]))
#     new <- new[[1]]
#   old <- .stp25Env$stp25.options
#   ## any reason to prefer get("stp25.options", envir = .stp25Env)?
#
#   ## if no args supplied, returns full options list
#   if (length(new) == 0)
#     return(old)
#
#   nm <- names(new)
#   if (is.null(nm))
#     return(old[unlist(new)]) ## typically getting options, not setting
#
#   isNamed <- nm != "" ## typically all named when setting, but could have mix
#   if (any(!isNamed)) nm[!isNamed] <- unlist(new[!isNamed])
#
#   ## so now everything has non-"" names, but only the isNamed ones should be set
#   ## everything should be returned, however
#
#   retVal <- old[nm]
#   names(retVal) <- nm
#   nm <- nm[isNamed]
#
#   ## this used to be
#
#   ## modified <- updateList(retVal[nm], new[nm])
#   ## .stp25Env$stp25.options[names(modified)] <- modified
#
#   ## but then calling stp25.options(foo = NULL) had no effect
#   ## because foo would be missing from modified.  So, we now do:
#
#   .stp25Env$stp25.options <- updateList(old, new[nm])
#
#   ## return changed entries invisibly
#   invisible(retVal)
# }








