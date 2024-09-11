#' Graphical Parameters for Lattice
#'
#' @param theme default= bw_theme()
#'
#' @return  invisible liste der lattice-settings
#' @export
#'
#' @examples
#'
#' library(lattice)
#' axis.padding   <- lattice.getOption("axis.padding")
#' layout.heights <- lattice.getOption("layout.heights")
#' layout.widths  <- lattice.getOption("layout.widths")
#'
#' #' Abstand der inneren Labels in effect::plot() (ist ja ein lattice-plot)
#' lattice.options(axis.padding =list(numeric=0.3))
#'
#' #' Abstand oben, unten, links, rechts
#' lattice.options(
#'   layout.widths =
#'     list(
#'       left.padding=list(x=.4),
#'       right.padding=list(x=1)
#'     )
#'   )
#'
#' #' reset
#' lattice.options(
#'   layout.widths = layout.widths,
#'   layout.heights =layout.heights,
#'   axis.padding =axis.padding
#'   )
#'
#'
#' # require(stp25settings)
#' require(lattice)
#' #lattice::trellis.par.set(standard_theme())
#' lattice::trellis.par.set(bw_theme(farbe()))
#' barchart(yield ~ variety | site, data = barley,
#'          groups = year, layout = c(1,6), stack = TRUE,
#'          auto.key = list(space = "right"),
#'          ylab = "Barley Yield (bushels/acre)",
#'          scales = list(x = list(rot = 45)))
#' #'
#' his <- data.frame(variable = LETTERS[1:5],  value = c(2, 3, 5, 6, 4))
#'
#' barchart(
#'   variable ~ value ,
#'   his,
#'   xlab = "Prozent nicht zutreffend",
#'   origin = 0,
#'   par.settings =
#'     bw_theme(col.bar  =  "#708C98FF")
#' )
#'
#'
#'  #lattice::trellis.par.set(
#'  #  effects::effectsTheme(
#'  #
#'  #    strip.background = list(col = "transparent"),
#'  #    strip.shingle = list(col = "black"),
#'  #    clip = list(strip = "off"),
#'  #    superpose.line = list(lwd = c(2, rep(1, 6))),
#'  #    col = grey.colors(7, start = 0.3, end = 0.9)
#'  #    ))
#'
set_lattice <- function(theme = bw_theme()) {
  lattice::trellis.par.set(theme)
  invisible(theme)
}

my_theme <- function(
                     col = c("#0C5BB0FF","#15983DFF","#EC579AFF","#FA6B09FF","#667840FF"),
                     col.bar  = "#708C98FF",
                     ...) {
  #' @rdname set_lattice
  #' @export
  bw_theme(col = col, col.bar = col.bar,...)
}

#' @rdname set_lattice
#' @export
reset_lattice <- function() {
  # weil beim standart theme die transpazenz fehlt.
  lattice::trellis.par.set(standard_theme())
  invisible(standard_theme())
}


#' @rdname set_lattice
#' @export
standard_theme <- function() {
  theme <- lattice::standard.theme()
  theme$background$col <- "transparent"
  theme
}

#' @rdname set_lattice
#' @description  ggplot lattice-Theme
#'
#' @export
ggplot_theme <-
  function(col = c("#00BA38",
                   "#00BFC4",
                   "#619CFF",
                   "#F564E3",
                   "#F8766D",
                   "#B79F00"),
           col.bar = NULL ,
           pch = 15:18,
           lty = 1:3,
           cex = 1,
           cex.main = 1.2,
           cex.axis = 0.8,
           cex.xlab = 1,
           cex.ylab = 1,
           cex.add = 0.8,
           strip.background.col = c("grey80", "grey70", "grey60"),
           ...) {
    theme <- latticeExtra::ggplot2like(n = 6, h.start = 120)

    if (is.null(col.bar))
      col.bar <- theme$plot.polygon$col

    theme$strip.background$col = strip.background.col

    theme$axis.text$cex = 0.8
    theme$axis.text$lineheight = 0.9
    theme$axis.text$col = "#000000"

    theme$superpose.symbol$col = col
    theme$superpose.symbol$pch = pch


    theme$superpose.polygon$col = col
    theme$superpose.polygon$border = "transparent"

    theme$plot.polygon$col = col.bar

    theme$superpose.line$col = col
    theme$superpose.line$lty = lty

    theme$box.dot$pch = 19
    theme$box.dot$cex = cex
    theme$box.rectangle$col = col.bar
    theme$superpose.symbol$fill = col

    theme$plot.symbol$pch = 1


    theme$par.xlab.text$cex <- cex.xlab
    theme$par.ylab.text$cex <- cex.ylab
    theme$par.main.text$cex <- cex.main
    theme$axis.text$cex <- cex.axis

    # text in auto.key
    theme$add.text$cex <- cex.add


    val <- list(...)
    if (length(val == 0))
      theme
    else
      modifyList(theme, val)
  }

#' @param col,col.bar,col.n  Farben
#' @param pch,lty,lwd Symbole
#' @param cex,cex.main,cex.axis,cex.xlab,cex.ylab,cex.symbol Schrift und Symbol-Groese
#' @param cex.strip,cex.add,strip.lineheight,strip.background.col,strip.border.col,strip.text.col  Strip
#' @param box Box
#' @param ... alles was direkt an trellis.par.set uebergeben wird
#' @rdname set_lattice
#' @description  Black and White lattice-Theme
#' @export
#' @importFrom utils modifyList
bw_theme <- function(col = grDevices::grey.colors(7, start = 0.3, end = 0.9),
                     col.n=5,
                     col.bar = "grey50",
                     pch = 15:18,
                     lty = 1:3,
                     lwd =c(2, rep(1, 6)),

                     cex = 1,
                     cex.main = 1.2,
                     cex.axis = 0.8,
                     cex.xlab = 1,
                     cex.ylab = 1,
                     cex.strip = 0.8,

                     cex.add = cex.strip,
                     cex.symbol =  0.8,

                     strip.lineheight = 1.2,
                     strip.background.col = "transparent",
                     strip.border.col = "#000000",
                     strip.text.col = "#000000",

                     bottom = 1,
                     top = 1,
                     left = 1,
                     right = 1,

                     box = NULL,
                     ...) {

  if (length(col) == 1) {
    col <-  if (grepl("[A-Z]", substr(col, 1, 1)))
      farbe(tolower(col), col.n)
  }

  theme <- lattice::standard.theme(color = FALSE)
  # background
  theme$strip.background$col <- strip.background.col
  # symbol
  theme$superpose.symbol$pch <- pch
  theme$superpose.symbol$fill <- col
  theme$superpose.symbol$col <- col
  theme$superpose.symbol$cex <- cex.symbol

  # balken
  theme$superpose.polygon$col <- col
  theme$superpose.polygon$border <- "transparent"
  theme$plot.polygon$col <- col.bar
  theme$plot.polygon$border <- "transparent"

  # lienien
  theme$superpose.line$col <- col
  theme$superpose.line$lty <- lty
  theme$superpose.line$lwd <- lwd

  # box-plot
  theme$box.dot$pch <- 19
  theme$box.dot$cex <- cex.symbol
  theme$plot.symbol$pch <- 1
  # text in auto.key und strip
  theme$add.text$cex <- cex.add

  # strip.default
  theme$strip.shingle$col <- col
  theme$strip.background$col <- strip.background.col
  theme$strip.border$col <- strip.border.col
  theme$add.text$col <-  strip.text.col

  theme$par.xlab.text$cex <- cex.xlab
  theme$par.ylab.text$cex <- cex.ylab
  theme$par.main.text$cex <- cex.main
  theme$axis.text$cex <- cex.axis


  # effects::effectsTheme
  # strip.background = list(col = gray(seq(0.95, 0.5, length = 3)))
  # strip.shingle = list(col = "black")
  theme$clip$strip <-"off"
  #  superpose.line = list(lwd = c(2, rep(1, 6)))


  # box um die Grafik
  if (!is.null(box)) {
    theme$box.3d$col <- "transparent"
    theme$strip.border$col <- "transparent"
    theme$axis.line$col <- "transparent"
  }

  # layout
  theme$layout.heights$top.padding <- top
  theme$layout.heights$bottom.padding <- bottom
  theme$layout.widths$left.padding <- left
  theme$layout.widths$right.padding <- right

  theme$layout.heights$strip <- strip.lineheight

  val <- list(...)
  if (length(val == 0))
    theme
  else
    utils::modifyList(theme, val)
}




