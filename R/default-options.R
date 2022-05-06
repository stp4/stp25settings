.defaultstp25Options <-  function () {
  list(
    language = "de",
    output =  "",
    fig_folder = "Fig/",
    # data_folder = "Raw data/",
    html_folder = "Results/",
    # css=TRUE,
    caption = NULL,
    center =  TRUE,
    sep_element = ", ",
    brackets = c("[", "]"),

    prozent = list(
      digits = 0,
      lead.zero = TRUE,
      style = 1,
      percentage_sign = TRUE,
      percentage_str = "%",
      null_percent_sign = "."
    ),

    mittelwert = list(
      digits = 2,
      lead.zero = TRUE,
      plusmin_sign = FALSE,
      plusmin_str = intToUtf8(177),
      mean.style = 1,
      seperator = ", ",
      median.style = "IQR"
    ),

    Fstat = list(digits = 2,
                 lead.zero = TRUE),
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
