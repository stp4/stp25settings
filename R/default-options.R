.defaultstp25Options  <-  function () {
  list(
  #  language = "de",
    output =  "",
   # default_browser = "meleon",
    fig_folder = "Fig/",
    # data_folder = "Raw data/",
    html_folder = "Results/",
    # css=TRUE,
    caption = NULL, # TRUE/FALSE
    center =  TRUE,
    sep_element = ", ",
    brackets = c("[", "]"),

    table = list(
      stubhead  = "Item",
      measure.name.m = "m",
      measure.name.sd = "sd",
      measure.name.total ="Total",
      measure.name.statistics ="Statistics",
      caption.nr.prefix =NULL,
      include.n=FALSE,
      include.tabel.number=TRUE
     # fmt = number = TRUE
     # caption.nr =TRUE
    ),

    prozent = list(
      digits = 0,
      lead.zero = TRUE,
      style = 1,
     # percentage_sign = TRUE, # nicht Aktiv
      percentage_str = "%",
      null_percent_sign = ".",
      include_name= "",
      include_level_multi=TRUE,
      exclude = NA
    ),

    mean = list(
      digits = 2,
      lead.zero = TRUE,

      plusmin_str = paste(" ", intToUtf8(177), " "),
      style = 1,   # style=3 plusmin_sign
      seperator = ", ",
      include_name= "(mean)"
    ),

    median = list(
      digits = 2,
      lead.zero = TRUE,
      seperator = ", ",
      style = "IQR",
      include_name="(median)"
    ),

    Fstat = list(digits = 2,
                 lead.zero = TRUE,
                 include.statistic=TRUE

                 ),

    r = list(digits = 2,
             lead.zero = FALSE),

    r2 = list(digits = 2,
              lead.zero = FALSE),

    p = list(
      digits = 3,
      lead.zero = FALSE,
      stars.value = c(0.001, 0.01, 0.05),
      stars.symbols = c("***", "**", "*"),
      with.stars = FALSE,
      mark.sig = NULL
    )
  )

}
