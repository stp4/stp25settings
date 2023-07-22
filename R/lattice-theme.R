#' Graphical Parameters for Lattice
#'
#' @param theme default= bw_theme()
#'
#' @return  invisible liste der lattice-settings
#' @export
#'
#' @examples
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
#' @param cex,cex.main,cex.axis,cex.xlab,cex.ylab,cex.add,cex.symbol Schrift und Symbol-Groese
#' @param strip.background.col,strip.border.col,strip.text.col,strip.text.cex,strip.text.lineheight Strip
#' @param box Box
#' @param ... alles was direkt an trellis.par.set uebergeben wird
#' @rdname set_lattice
#' @description  Black and White lattice-Theme
#' @export
#' @importFrom utils modifyList
bw_theme <- function(col = grDevices::grey.colors(7, start = 0.3, end = 0.9),
                     col.n=5,
                     col.bar =  "grey50",
                     pch = 15:18,
                     lty = 1:3,
                     lwd =c(2, rep(1, 6)),

                     cex = 1,
                     cex.main = 1.2,
                     cex.axis = 0.8,
                     cex.xlab = 1,
                     cex.ylab = 1,
                     cex.add = 0.8,
                     cex.symbol =  0.8,

                     strip.background.col = "transparent",
                     strip.border.col = "#000000",
                     strip.text.col = "#000000",
                     strip.text.cex = NULL,
                     strip.text.lineheight = NULL,
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
  if (!is.null(strip.text.cex)) {
    theme$add.text$cex <- strip.text.cex
    warning("strip.text.cex: das geht nur mit\n par.strip.text = list(cex = 2)")
  }
  if (!is.null(strip.text.lineheight))
    warning(
      "strip.text.lineheight: das geht nicht - aber das funktioniert\n par.strip.text = list(lineheight= 2)"
    )
  # main, labs
  theme$par.xlab.text$cex <- cex.xlab
  theme$par.ylab.text$cex <- cex.ylab
  theme$par.main.text$cex <- cex.main
  theme$axis.text$cex <- cex.axis


  # effects::effectsTheme
  #strip.background = list(col = gray(seq(0.95, 0.5, length = 3)))
 # strip.shingle = list(col = "black")
  theme$clip$strip <-"off"
#  superpose.line = list(lwd = c(2, rep(1, 6)))



  #box um die Grafik
  if (!is.null(box)) {
    theme$box.3d$col <- "transparent"
    theme$strip.border$col <- "transparent"
    theme$axis.line$col <- "transparent"
  }

  val <- list(...)
  if (length(val == 0))
    theme
  else
    utils::modifyList(theme, val)
}




#' Color palettes
#'
#' Hex-6-Code
#'
#' #FF0000   Maximum an rot und kein greun oder blau
#'
#' #00FF00  nur gruen und kein rot oder blau
#'
#' #0000FF  nur blau und kein rot oder gruen
#'
#'
#' Hex-8-Code
#'
#'  Leuchtkraft und Zahlenwert
#'
#' Hex-Wert	 	Leuchtkraft (Prozent)
#' 00	 	0 Prozent
#' 40	 	25 Prozent
#' 80	 	50 Prozent
#' C0	 	75 Prozent
#' FF	 	100 Prozent
#'
#'
#'
#'
#' @param type   "pirat", "ggplot",
#' "dark", "pastel","cb", "sex","sex.mf","brewer","bw","grays"
#' @param n Number of different colors in the palette, minimum 3, maximum depending on palette
#' @param ... an Likert_col name, middle, middle.color
#'
#' @return character-string
#' @export
#'
#' @examples
#'
#' farbe()
#'
#'  par(mfrow=c(2,3))
#' barplot(cbind(1:5, rep(3,5)), horiz = TRUE, col=farbe("likert", 5, name="RdBl"))
#' barplot(cbind(1:3, rep(3,3)), horiz = TRUE, col=farbe("likert", 3, name="RdBl"))
#' barplot(cbind(1:8, rep(3,8)), horiz = TRUE, col=farbe("likert", 8, name="RdBl"))
#'
#' barplot(cbind(1:5, rep(3,5)), horiz = TRUE, col=farbe("pirat", 5))
#' barplot(cbind(1:3, rep(3,3)), horiz = TRUE, col=farbe("Greens", 3))
#' barplot(cbind(1:8, rep(3,8)), horiz = TRUE, col=farbe("color.blinde", 8))
#'
farbe <- function(
  type = c(
  "pirat", "ggplot", "dark", "dunkel",
  "pastel", "hell", "cb", "color.blinde",
  "sex", "sex.mf", "bw", "grays", "likert",
  "Reds", "Blues", "Greens","Blues", "Greys", "Oranges", "Purples"),
  n = 5,
  ...) {

  cbPalette <- c(
    orange     = "#E69F00",
    skyblue    = "#56B4E9",
    green      = "#009E73",
    yellow     = "#F0E442",
    blue       = "#0072B2",
    vermillion = "#D55E00",
    purple     = "#CC79A7",
                "#66C2A5",
                "#FC8D62",
                "#8DA0CB",
                "#E78AC3",
                "#A6D854",
                "#FFD92F",
                "#E5C494",
                "#B3B3B3",
                "#1B9E77",
                "#D95F02",
                "#7570B3",
                "#E7298A",
                "#66A61E",
                "#E6AB02",
                "#A6761D",
                "#666666"
  )

  type <-  match.arg(type, several.ok = FALSE)
  switch(
    type,
    pirat = c(
      blue1     = "#0C5BB0FF",
      green    =  "#15983DFF" ,
      pink   =    "#EC579AFF",
      orange    = "#FA6B09FF",
      green1   =  "#667840FF" ,
      red1      = "#B91226FF" ,
      blue2     = "#149BEDFF" ,
      green2    = "#A1C720FF",
      yellow    = "#FEC10BFF",
      turquoise = "#16A08CFF",
      poop    =   "#9A703EFF"  ,
      purple2   = "#972C8DFF",
      orange2  =  "#FF6435FF"  ,
      brown     = "#6A1D1AFF",
      purple =    "#5A5895FF" ,
      salmon    = "#D86C4FFF" ,
      darkgreen = "#006A40FF" ,
      brown1    = "#88775FFF" ,
      red     =   "#EE0011FF"  ,
      green     = "#5FB233FF",
      gray1    =  "#323337FF" ,
      gray2    =  "#534C53FF",
      blue    =   "#3F516AFF"  ,
      grayblue  = "#708C98FF"
    )[1:n],
    ggplot =  c("#00BA38",
                "#00BFC4",
                "#619CFF",
                "#F564E3",
                "#F8766D",
                "#B79F00"),

    dark = RColorBrewer::brewer.pal(n, "Dark2"),
 #   "#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E" "#E6AB02" "#A6761D" "#666666"
    dunkel = RColorBrewer::brewer.pal(n, "Dark2"),
    pastel = RColorBrewer::brewer.pal(n, "Pastel1"),
    hell = RColorBrewer::brewer.pal(n, "Pastel1"),
    cb = cbPalette[1:n],
    color.blinde = cbPalette[1:n],

    bw =   grDevices::grey.colors(n, start = 0.3, end = 0.9),
    sex =  c(pink  = "#EC579AFF",
             blue2 = "#149BEDFF"),

    sex.mf = c(blue2 = "#149BEDFF",
               pink  = "#EC579AFF"),

    grays   =  grDevices::grey.colors(n, start = 0.3, end = 0.9),
    likert = likert_col(n=n, ...),

    brewer_pal2(n, type )
  )

}

#' @rdname farbe
#' @param name A palette name from the lists below "RdBl"  ist   RdBl = c("Reds", "Blues")
#' @param middle,middle.color reference   "gray65"
#' @importFrom grDevices rgb
#'
#' @examples
#'
#'
#'
#' farbe("likert")
#' stp25settings:::likert_col(5 )
#' #"#FC9272"    "#FEE0D2"     "gray90"    "#DEEBF7"    "#9ECAE1"
#'
#'
#' farbe("likert", n=4,  name="GrRd", middle=2)
#' stp25settings:::likert_col(4 , "GrRd", middle=2)
#' #"#A1D99B"     "gray90"    "#FEE0D2"    "#FC9272"
#'
likert_col <- function(n = 5,
                       name =  "RdBl" ,
                       # c("RdBl", "BlRd",
                       #          "RdGr", "GrRd",
                       #          "GrBl", "BlGr",
                       #          "Bw"),
                       middle = mean(1:n),
                       middle.color =  "gray90") {
  # cat("\n n = ", n, " middle = " ,middle, "\n")
  if (length(name) == 1) {
    name <-
      switch(
        name,
        RdBl = c("Reds", "Blues"),
        BlRd = c("Blues", "Reds"),
        RdGr = c("Reds", "Greens"),
        BlGr = c("Blues", "Greens"),
        GrBl = c("Greens", "Blues"),
        GrRd = c("Greens", "Reds"),
        c("Greys", "Greys")
      )
  }
  if (is.odd(middle)) {
    c(rev(brewer_pal2(n = middle - 1, name = name[1])),
      middle.color = middle.color,
      brewer_pal2(n = n - middle, name = name[2]))
  }
  else{
    c(rev(brewer_pal2(n = floor(middle), name = name[1])),
      brewer_pal2(n = n - floor(middle), name = name[2]))
  }

}

brewer_pal2<- function(n,
                        name="Blues"){
  my_color <-
    switch(
      name,
      Reds = switch(
        n,
        rgb(
          c( 252),
          c( 146),
          c( 114),
          maxColorValue = 255
        ),
        rgb(
          c(254, 252),
          c(224, 146),
          c(210, 114),
          maxColorValue = 255
        ),
        rgb(
          c(254, 252, 222),
          c(224, 146, 45),
          c(210, 114, 38),
          maxColorValue = 255
        ),
        rgb(
          c(254,  252, 251, 203),
          c(229, 174, 106, 24),
          c(217, 145, 74, 29),
          maxColorValue = 255
        ),
        rgb(
          c(254, 252, 251,222, 165),
          c(229, 174, 106, 45, 15),
          c(217, 145, 74, 38, 21),
          maxColorValue = 255
        ),
        rgb(
          c(254, 252,  252, 251, 222, 165),
          c(229, 187, 146, 106, 45, 15),
          c(217, 161, 114, 74, 38, 21),
          maxColorValue = 255
        ),
        rgb(
          c(254, 252, 252, 251, 239, 203, 153),
          c(229, 187, 146, 106, 59, 24, 0),
          c(217, 161, 114, 74, 44, 29, 13),
          maxColorValue = 255
        ),
        rgb(
          c(255, 254, 252, 252, 251, 239, 203, 153),
          c(245, 224, 187, 146, 106, 59, 24, 0),
          c(240, 210, 161, 114, 74, 44, 29, 13),
          maxColorValue = 255
        ),
        rgb(
          c(255, 254, 252, 252, 251, 239, 203, 165, 103),
          c(245, 224, 187, 146, 106,  59, 24, 15, 0),
          c(240, 210, 161, 114,  74,  44, 29, 21, 13),
          maxColorValue = 255
        )
      )


      ,



      Blues = switch(
        n ,
        rgb(
          c(158),
          c(202),
          c(225),
          maxColorValue = 255
        ),
        rgb(
          c(222, 158),
          c(235, 202),
          c(247, 225),
          maxColorValue = 255
        ),
        rgb(
          c(222, 158, 49),
          c(235, 202, 130),
          c(247, 225, 189),
          maxColorValue = 255
        ),
        rgb(
          c(239, 189, 107, 33),
          c(243, 215, 174, 113),
          c(255, 231, 214, 181),
          maxColorValue = 255
        ),
        rgb(
          c(239, 189, 107, 49, 8),
          c(243, 215, 174, 130, 81),
          c(255, 231, 214, 189, 156),
          maxColorValue = 255
        ),
        rgb(
          c(239, 198, 158, 107, 49, 8),
          c(243, 219, 202, 174,
            130, 81),
          c(255, 239, 225, 214, 189, 156),
          maxColorValue = 255
        ),
        rgb(
          c(239, 198, 158, 107, 66, 33, 8),
          c(243, 219, 202,
            174, 146, 113, 69),
          c(255, 239, 225, 214, 198, 181,
            148),
          maxColorValue = 255
        ),
        rgb(
          c(247, 222, 198,
            158, 107, 66, 33, 8),
          c(251, 235, 219, 202, 174,
            146, 113, 69),
          c(255, 247, 239, 225, 214, 198, 181,
            148),
          maxColorValue = 255
        ),
        rgb(
          c(247, 222, 198,
            158, 107, 66, 33, 8, 8),
          c(251, 235, 219, 202, 174,
            146, 113, 81, 48),
          c(255, 247, 239, 225, 214, 198,
            181, 156, 107),
          maxColorValue = 255
        )
      ),





      Greens = switch(
        n,
        rgb(
          c(161),
          c(217),
          c(155),
          maxColorValue = 255
        ),
        rgb(
          c(229, 161),
          c(245, 217),
          c(224, 155),
          maxColorValue = 255
        ),
        rgb(
          c(229, 161, 49),
          c(245, 217, 163),
          c(224, 155, 84),
          maxColorValue = 255
        ),
        rgb(
          c(237,186, 116, 35),
          c(248, 228, 196, 139),
          c(233, 179,118, 69),
          maxColorValue = 255
        ),
        rgb(
          c(237, 186, 116,49, 0),
          c(248, 228, 196, 163, 109),
          c(233, 179, 118,84, 44),
          maxColorValue = 255
        ),
        rgb(
          c(237, 199, 161,116, 49, 0),
          c(248, 233, 217, 196, 163, 109),
          c(233,192, 155, 118, 84, 44),
          maxColorValue = 255
        ),
        rgb(
          c(237,199, 161, 116, 65, 35, 0),
          c(248, 233, 217, 196,171, 139, 90),
          c(233, 192, 155, 118, 93, 69, 50),
          maxColorValue = 255
        ),
        rgb(
          c(247, 229, 199, 161, 116,65, 35, 0),
          c(252, 245, 233, 217, 196, 171, 139,90),
          c(245, 224, 192, 155, 118, 93, 69, 50),
          maxColorValue = 255
        ),
        rgb(
          c(247, 229, 199, 161, 116, 65, 35, 0, 0),
          c(252,245, 233, 217, 196, 171, 139, 109, 68),
          c(245,224, 192, 155, 118, 93, 69, 44, 27),
          maxColorValue = 255
        )
      ),


      Greys = switch(
        n,
        rgb(c(189),
            c(189),
            c(189),
            maxColorValue = 255),
        rgb(c(240, 189),
            c(240, 189),
            c(240, 189),
            maxColorValue = 255),

        rgb(c(240, 189, 99),
            c(240, 189, 99),
            c(240, 189, 99), maxColorValue = 255),
        rgb(
          c(247, 204, 150, 82),
          c(247, 204, 150, 82),
          c(247, 204, 150, 82),
          maxColorValue = 255
        ),
        rgb(
          c(247, 204, 150, 99, 37),
          c(247, 204, 150, 99, 37),
          c(247, 204, 150, 99, 37),
          maxColorValue = 255
        ),
        rgb(
          c(247, 217, 189, 150, 99, 37),
          c(247, 217, 189, 150, 99, 37),
          c(247, 217, 189, 150, 99, 37),
          maxColorValue = 255
        ),
        rgb(
          c(247, 217, 189, 150, 115, 82, 37),
          c(247, 217, 189, 150, 115, 82, 37),
          c(247, 217, 189, 150, 115, 82, 37),
          maxColorValue = 255
        ),
        rgb(
          c(255, 240, 217, 189, 150, 115, 82, 37),
          c(255, 240, 217, 189, 150, 115, 82, 37),
          c(255, 240, 217, 189, 150, 115, 82, 37),
          maxColorValue = 255
        ),
        rgb(
          c(255, 240, 217, 189, 150, 115, 82, 37, 0),
          c(255, 240, 217, 189, 150, 115, 82, 37, 0),
          c(255, 240, 217, 189, 150, 115, 82, 37, 0),
          maxColorValue = 255
        )
      ),



      Oranges = switch(
        n ,
        rgb(
          c(253),
          c(174),
          c(107),
          maxColorValue = 255
        ),
        rgb(
          c(254, 253),
          c(230, 174),
          c(206, 107),
          maxColorValue = 255
        ),
        rgb(
          c(254, 253, 230),
          c(230, 174,   85),
          c(206, 107, 13),
          maxColorValue = 255
        ),
        rgb(
          c(254, 253, 253, 217),
          c(237, 190, 141,  71),
          c(222, 133,  60,   1),
          maxColorValue = 255
        ),
        rgb(
          c(254, 253, 253, 230, 166),
          c(237, 190, 141,  85,  54),
          c(222, 133,  60,  13,   3),
          maxColorValue = 255
        ),
        rgb(
          c(254, 253, 253, 253, 230, 166),
          c(237, 208, 174, 141,  85,  54),
          c(222, 162, 107,  60,  13,   3),
          maxColorValue = 255
        ),
        rgb(
          c(254, 253, 253, 253, 241, 217, 140),
          c(237, 208, 174, 141, 105,  72,  45),
          c(222, 162, 107,  60,  19,   1,   4),
          maxColorValue = 255
        ),
        rgb(
          c(255, 254, 253, 253, 253, 241, 217, 140),
          c(245, 230, 208, 174, 141, 105,  72,  45),
          c(235, 206, 162, 107, 60,   19,   1,   4),
          maxColorValue = 255
        ),
        rgb(
          c(255, 254, 253, 253, 253, 241, 217, 166, 127),
          c(245, 230, 208, 174, 141, 105,  72,  54,  39),
          c(235, 206, 162, 107,  60,  19,   1,   3,   4),
          maxColorValue = 255
        )
      ),


      Purples = switch(
        n,
        rgb(
          c( 188 ),
          c( 189 ),
          c( 220 ),
          maxColorValue = 255
        ),
        rgb(
          c(239, 188),
          c(237, 189),
          c(245, 220),
          maxColorValue = 255
        ),

        rgb(
          c(239, 188, 117),
          c(237, 189, 107),
          c(245, 220, 177),
          maxColorValue = 255
        ),
        rgb(
          c(242, 203, 158, 106),
          c(240, 201, 154,  81),
          c(247, 226, 200, 163),
          maxColorValue = 255
        ),
        rgb(
          c(242, 203, 158, 117,  84),
          c(240, 201, 154, 107,  39),
          c(247, 226, 200, 177, 143),
          maxColorValue = 255
        ),
        rgb(
          c(242, 218, 188, 158, 117,  84),
          c(240, 218, 189, 154, 107,  39),
          c(247, 235, 220, 200, 177, 143),
          maxColorValue = 255
        ),
        rgb(
          c(242, 218, 188, 158, 128, 106,  74),
          c(240, 218, 189, 154, 125,  81,  20),
          c(247, 235, 220, 200, 186, 163, 134),
          maxColorValue = 255
        ),
        rgb(
          c(252,  239, 218, 188, 158, 128, 106,  74),
          c(251,  237, 218, 189, 154, 125,  81,  20),
          c(253,  245, 235, 220, 200, 186, 163, 134),
          maxColorValue = 255
        ),
        rgb(
          c(252, 239, 218, 188, 158, 128, 106,  84,  63),
          c(251, 237, 218, 189, 154, 125,  81,  39,   0),
          c(253, 245, 235, 220, 200, 186, 163, 143, 125),
          maxColorValue = 255
        )
      )
    )
  names(my_color ) <- paste0(name, 1:n)
  my_color
}


is.odd <- function(x)
  trunc(x) - x == 0
