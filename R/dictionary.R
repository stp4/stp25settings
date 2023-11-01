## # -  Loeschkandidat ------------------------------------------------------------
##
##
## ##  Automatisches Uebersetzen
## ##
## ## @param x name nin get_lang
## get_lang <- function(x = NULL) {
##   lng <- stp25.getLanguage()
##   if (is.null(x))
##     lng
##   else
##     Names2Language(x, lng)
## }
##
##
## ## @rdname get_lang
## ## @param ... alles an set_lang
## set_lang <- function(...) {
##   new <- list(...)
##   if(length(new)== 0) stp25.language("de")
##   else if( is.null(new[[1]]))  .stp25Env$stp25.language <- NULL
##   else stp25.language(...)
##
## }
##
##
## ## @rdname get_lang
## delet_lang <- function(...) {
##   .stp25Env$stp25.language <- NULL
## }
##
## stp25.getLanguage <- function(){
##   get("stp25.language", envir = .stp25Env)
## }
##
##
## stp25.language <- function(...){
##   new <- list(...)
##
##   if (new[1] == "language" || new[1] %in% c("de", "en")) {
##     lng <-  (.defaultstp25Language(new[[1]]))
##     new <- new[-1]
##   } else
##     lng <- .stp25Env$stp25.language
##
##   if (length(new) > 0) {
##     for (i in names(new))
##       lng[i] <- new[i]
##     lng <- unlist(lng)
##   }
##
##   .stp25Env$stp25.language <- lng
##   invisible(lng)
##
## }
##
##
## ## @rdname get_lang
## ## @param replace in Names2Language
## ## @description Names2Language() wurde von Output benutzt um die De/En Uebersaetzung durchzufuehren.
## Names2Language <- function(x, replace) {
##   if (is.null(replace))  return(x)
##   plyr::mapvalues(x,
##                   from = names(replace),
##                   to = replace,
##                   warn_missing = FALSE)
## }
##
##
## ## Liste mit Uebersaetzung
## ##
## ## @noRd
## ## @param language  Sprache default="de"
## ## @param words Wort-liste Deutsch Englisch
## .defaultstp25Language <-
##   function(language = "de",
##
##            words = list(
##              characteristics =  c("Characteristics", "Bezeichnung"),
##
##              statistics = c("Test Statistic", "Test Statistik"),
##              statistic = c("Test Statistic", "Test Statistik"),
##              sig.test = c("Test Statistic", "Test Statistik"),
##
##              mean.Rank = c("mean Rank", "mittlerer Rang"),
##              skalenwert = c("Characteristic Value", "Merkmalsauspraegung"),
##              items  = c("Items", "Items"),
##              item  = c("Item", "Item"),
##
##              value = c("Value", "Wert"),
##              variable = c ("Variable", "Merkmal"),
##              mass = c ("Value", "Anpassungsmass"),
##              annforderung = c ("Requirement", "Anforderung"),
##
##              source  = c ("Source", "Quelle"),
##
##
##              N = c("N", "N"),
##              n = c("n", "n"),
##              mean = c("Mean", "M"),
##              sd = c("SD", "SD"),
##              ci95.low = c("low 95% CI", "low 95% CI"),
##              ci95.hig = c("up 95% CI", "up 95% CI"),
##
##              se = c("SE", "SE"),
##              #Std. Error
##              estimate = c("B", "B"),
##              #Estimate
##              est.std = c("Beta", "Beta"),
##              #std. Estimate
##              beta = c("Beta", "Beta"),
##              #std. Estimate
##
##              df = c("Df", "Df"),
##              z.value = c("z", "z"),
##              f.value = c("F", "F"),
##              t.value  = c ("T", "T"),
##              p.value = c("p", "p"),
##              lr.chisq = c ("LR Chisq", "LR Chisq"),
##              eta = c("eta Sq", "eta Sq"),
##              adj.r2 = c("adj. R2", "adj. R2"),
##              sum.sq = c("Sum Sq", "Sum Sq"),
##              r2 = c("R2", "R2")
##
##            )) {
##
##     lng <- if (language == "de") 2  else  1
##
##     sapply(words, "[[", lng)
##   }
##
##
##
##
##
##
##
##
##
##
