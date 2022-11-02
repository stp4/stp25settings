#' Settings
#'
#' @name set_opt
#' @param output,fig_folder,html_folder,center  wird über Projekt() eingestellt "html"
#' @param caption logical TRUE => include N in caption
#' @param sep_element,brackets globale symbole
#' @param table list(
#' stubhead  = "Item",
#' measure.name.m = "m",
#' measure.name.sd = "sd",
#' measure.name.total ="Total",
#' measure.name.statistics ="Statistics")
#' @param prozent list(
#' digits = 0,
#' lead.zero = TRUE,
#' style = 1,
#' percentage_str = "%",
#' null_percent_sign = ".",
#' include_name= "",
#' include_level_multi=TRUE,
#' exclude)
#' @param mean   list(
#' digits = 2,
#' lead.zero = TRUE,
#' plusmin_str = intToUtf8(177),
#' style = 1,
#' seperator = ", ",
#' include_name= "(mean)" )
#' @param median   list(
#'   digits = 2,
#'   lead.zero = TRUE,
#'   seperator = ", ",
#'   style = "IQR",
#'   include_name="(median)" )
#' @param Fstat  list(
#' digits = 2,
#' lead.zero = TRUE,
#' include.statistic=TRUE )
#' @param r list(
#' digits = 2,
#' lead.zero = FALSE)
#' @param r2  list(
#' digits = 2,
#' lead.zero = FALSE)
#' @param p   list(
#'   digits = 3,
#'   lead.zero = FALSE,
#'   stars.value = c(0.001, 0.01, 0.05),
#'   stars.symbols = c("***", "**", "*"),
#'   with.stars = FALSE,
#'   mark.sig = NULL )
#'
#' @return string
#' @export
#'
#' @examples
#'
#' set_opt(
#' percent = list(
#'   style = 2,
#'   null_percent_sign =  ' . ',
#'   digits = 0,
#'   percentage_str = " %",
#'   include_name = ", n (%)"
#' ),
#'
#' mean = list(style = 1,
#'             include_name = ", mean (sd)"),
#'
#' median = list(style = 3,
#'               include_name = ", median (range)"),
#'
#' Fstat =list(include.statistic=FALSE),
#'
#' table = list(
#'   stubhead = "Items",
#'   measure.name.m = "Mittelwert/Prozent",
#'   measure.name.total = "Summe",
#'   measure.name.statistics = "Statistik"
#' )
#' )
#'  get_opt()
set_opt <- function(...) {
  new <- list(...)

  if("mittelwert" %in%  names(new))
    stop("mittelwert gibs nicht mehr! Verwende median oder mean!")
  names(new) <-
    dplyr::recode(names(new),
                #  mean = "mittelwert",
                  percent = "prozent")
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







