## ---- echo = FALSE------------------------------------------------------------
library <- function(...) {
  suppressWarnings(
    suppressPackageStartupMessages(base::library(..., quietly = TRUE))
  )
}

## -----------------------------------------------------------------------------
library(magrittr)
library(dplyr)
library(ggplot2)
library(rdbnomics)

## ---- echo = FALSE------------------------------------------------------------
reorder_cols <- function(x) {
  cols <- c(
    "provider_code", "dataset_code", "dataset_name", "series_code",
    "series_name", "original_period", "period", "original_value", "value",
    "@frequency"
  )

  if ("unit" %in% colnames(x)) {
    cols <- c(cols, "unit", "Unit")
  }

  if ("geo" %in% colnames(x)) {
    cols <- c(cols, "geo", "Country")
  }

  if ("freq" %in% colnames(x)) {
    cols <- c(cols, "freq", "Frequency")
  }

  cols_add <- setdiff(colnames(x), cols)
  cols <- c(cols, cols_add)

  cols <- cols[cols %in% colnames(x)]
  
  cols <- match(cols, colnames(x))

  dplyr::select(x, cols)
}

knitr::opts_chunk$set(dev.args = list(bg = "transparent"))

dbnomics <- function(color_palette = "Set1", ...) {
  # Check if ggplot2 is installed.
  ggplot2_ok <- try(utils::packageVersion("ggplot2"), silent = TRUE)
  if (inherits(ggplot2_ok, "try-error")) {
    stop(
      "Please run install.packages('ggplot2') to use dbnomics().",
      call. = FALSE
    )
  }

  # DBnomics vignette theme
  result <- list(
    ggplot2::scale_x_date(expand = c(0, 0)),
    ggplot2::scale_y_continuous(
      labels = function(x) { format(x, big.mark = " ") }
    ),
    ggplot2::xlab(""),
    ggplot2::ylab(""),
    ggplot2::theme_bw(),
    ggplot2::theme(
      legend.position = "bottom", legend.direction = "vertical",
      legend.background = ggplot2::element_rect(
        fill = "transparent", colour = NA
      ),
      legend.key = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(
        fill = "transparent", colour = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = "transparent", colour = NA
      ),
      legend.title = ggplot2::element_blank()
    ),
    ggplot2::theme(...),
    ggplot2::annotate(
      geom = "text", label = "DBnomics <https://db.nomics.world>", 
      x = structure(Inf, class = "Date"), y = -Inf,
      hjust = 1.1, vjust = -0.4, col = "grey", 
      fontface = "italic"
    )
  )

  if (!is.null(color_palette)) {
    result <- c(
      result,
      list(ggplot2::scale_color_brewer(palette = color_palette))
    )
  }

  result
}

display_table <- function(DT) {
  DT_ok <- FALSE
  if (
    "rmarkdown" %in% installed.packages()[, "Package"] &
    "DT" %in% installed.packages()[, "Package"]
  ) {
    if (rmarkdown::pandoc_available()) {
      if (rmarkdown::pandoc_version() >= numeric_version("1.12.3")) {
        DT_ok <- TRUE
      }
    }
  }

  if (DT_ok) {
    DT::datatable(
      DT,
      rownames = FALSE, options = list(pageLength = 5, scrollX = TRUE)
    )
  } else {
    dplyr::as.tbl(DT)
  }
}

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb(ids = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN") %>%
#    filter(!is.na(value))

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df001

## ---- echo = FALSE------------------------------------------------------------
df %>%
  reorder_cols() %>%
  display_table()

## ---- fig.align = 'center'----------------------------------------------------
ggplot(df, aes(x = period, y = value, color = series_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  dbnomics()

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("AMECO/ZUTN/EA19.1.0.0.0.ZUTN")

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb(ids = c("AMECO/ZUTN/EA19.1.0.0.0.ZUTN", "AMECO/ZUTN/DNK.1.0.0.0.ZUTN")) %>%
#    filter(!is.na(value))

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df002

## ---- echo = FALSE------------------------------------------------------------
df %>%
  arrange(series_code, period) %>%
  reorder_cols() %>%
  display_table()

## ---- fig.align = 'center'----------------------------------------------------
ggplot(df, aes(x = period, y = value, color = series_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  dbnomics()

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb(ids = c("AMECO/ZUTN/EA19.1.0.0.0.ZUTN", "Eurostat/une_rt_q/Q.SA.TOTAL.PC_ACT.T.EA19")) %>%
#    filter(!is.na(value))

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df003

## ---- echo = FALSE------------------------------------------------------------
df %>%
  arrange(series_code, period) %>%
  reorder_cols() %>%
  display_table()

## ---- fig.align = 'center'----------------------------------------------------
ggplot(df, aes(x = period, y = value, color = series_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  dbnomics(legend.text = element_text(size = 7))

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("IMF", "BOP", mask = "A.FR.BCA_BP6_EUR") %>%
#    filter(!is.na(value))

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df004

## ---- echo = FALSE------------------------------------------------------------
df %>%
  reorder_cols() %>%
  display_table()

## ---- fig.align = 'center'----------------------------------------------------
ggplot(df, aes(x = period, y = value, color = series_name)) +
  geom_step(size = 1.2) +
  geom_point(size = 2) +
  dbnomics()

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("IMF", "BOP", "A.FR.BCA_BP6_EUR")

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("IMF", "BOP", mask = "A.FR+ES.BCA_BP6_EUR") %>%
#    filter(!is.na(value))

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df005

## ---- echo = FALSE------------------------------------------------------------
df %>%
  arrange(series_code, period) %>%
  reorder_cols() %>%
  display_table()

## ---- fig.align = 'center'----------------------------------------------------
ggplot(df, aes(x = period, y = value, color = series_name)) +
  geom_step(size = 1.2) +
  geom_point(size = 2) +
  dbnomics()

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("IMF", "BOP", mask = "A..BCA_BP6_EUR") %>%
#    filter(!is.na(value)) %>%
#    arrange(desc(period), REF_AREA) %>%
#    head(100)

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df006

## ---- echo = FALSE------------------------------------------------------------
df %>%
  reorder_cols() %>%
  display_table()

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("IMF", "BOP", mask = "A.FR.BCA_BP6_EUR+IA_BP6_EUR") %>%
#    filter(!is.na(value)) %>%
#    group_by(INDICATOR) %>%
#    top_n(n = 50, wt = period)

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- ungroup(rdbnomics:::rdbnomics_df007)

## ---- echo = FALSE------------------------------------------------------------
df %>%
  reorder_cols() %>%
  display_table()

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("AMECO", "ZUTN", dimensions = list(geo = "ea19")) %>%
#    filter(!is.na(value))
#  # or
#  # df <- rdb("AMECO", "ZUTN", dimensions = '{"geo": ["ea19"]}') %>%
#  #   filter(!is.na(value))

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df008

## ---- echo = FALSE------------------------------------------------------------
df %>%
  reorder_cols() %>%
  display_table()

## ---- fig.align = 'center'----------------------------------------------------
ggplot(df, aes(x = period, y = value, color = series_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  dbnomics()

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("AMECO", "ZUTN", dimensions = list(geo = c("ea19", "dnk"))) %>%
#    filter(!is.na(value))
#  # or
#  # df <- rdb("AMECO", "ZUTN", dimensions = '{"geo": ["ea19", "dnk"]}') %>%
#  #   filter(!is.na(value))

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df009

## ---- echo = FALSE------------------------------------------------------------
df %>%
  arrange(series_code, period) %>%
  reorder_cols() %>%
  display_table()

## ---- fig.align = 'center'----------------------------------------------------
ggplot(df, aes(x = period, y = value, color = series_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  dbnomics()

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("WB", "DB", dimensions = list(country = c("DZ", "PE"), indicator = c("ENF.CONT.COEN.COST.ZS", "IC.REG.COST.PC.FE.ZS"))) %>%
#    filter(!is.na(value))
#  # or
#  # df <- rdb("WB", "DB", dimensions = '{"country": ["DZ", "PE"], "indicator": ["ENF.CONT.COEN.COST.ZS", "IC.REG.COST.PC.FE.ZS"]}') %>%
#  #   filter(!is.na(value))

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df010

## ---- echo = FALSE------------------------------------------------------------
df %>%
  arrange(series_name, period) %>%
  reorder_cols() %>%
  display_table()

## ---- fig.align = 'center'----------------------------------------------------
ggplot(df, aes(x = period, y = value, color = series_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  dbnomics()

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("IMF", "WEO", query = "France current account balance percent") %>%
#    filter(!is.na(value))

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df014

## ---- echo = FALSE------------------------------------------------------------
df %>%
  reorder_cols() %>%
  display_table()

## ---- fig.align = 'center'----------------------------------------------------
ggplot(df, aes(x = period, y = value, color = series_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  dbnomics()

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("IMF", "WEO", query = "current account balance percent") %>%
#    filter(!is.na(value))

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df015

## ---- echo = FALSE------------------------------------------------------------
df %>%
  reorder_cols() %>%
  display_table()

## ---- fig.align = 'center'----------------------------------------------------
ggplot(df, aes(x = period, y = value, color = `WEO Country`)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  ggtitle("Current account balance (% GDP)") +
  dbnomics(legend.direction = "horizontal")

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb(api_link = "https://api.db.nomics.world/v22/series/WB/DB?dimensions=%7B%22country%22%3A%5B%22FR%22%2C%22IT%22%2C%22ES%22%5D%7D&q=IC.REG.PROC.FE.NO&observations=1&format=json&align_periods=1&offset=0&facets=0") %>%
#    filter(!is.na(value))

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df011

## ---- echo = FALSE------------------------------------------------------------
df %>%
  arrange(period, series_name) %>%
  reorder_cols() %>%
  display_table()

## ---- fig.align = 'center'----------------------------------------------------
ggplot(df, aes(x = period, y = value, color = series_name)) +
  geom_step(size = 1.2) +
  geom_point(size = 2) +
  dbnomics()

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("https://api.db.nomics.world/v22/series/WB/DB?dimensions=%7B%22country%22%3A%5B%22FR%22%2C%22IT%22%2C%22ES%22%5D%7D&q=IC.REG.PROC.FE.NO&observations=1&format=json&align_periods=1&offset=0&facets=0")

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb(api_link = "https://api.db.nomics.world/v22/series?observations=1&series_ids=BOE/6008/RPMTDDC,BOE/6231/RPMTBVE") %>%
#    filter(!is.na(value))

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df012

## ---- echo = FALSE------------------------------------------------------------
df %<>%
  mutate(
    series_name = sapply(
      series_name,
      function(y) {
        paste0(
          paste0(
            strsplit(y, "institutions' ")[[1]], collapse = "institutions'\n"
          ),
          "\n"
        )
      }
    )
  )

## ---- echo = FALSE------------------------------------------------------------
df %>%
  arrange(period, series_name) %>%
  reorder_cols() %>%
  display_table()

## ---- fig.align = 'center'----------------------------------------------------
ggplot(df, aes(x = period, y = value, color = series_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  dbnomics()

## ---- eval = FALSE------------------------------------------------------------
#  Error in open.connection(con, "rb") :
#    Could not resolve host: api.db.nomics.world

## ---- eval = FALSE------------------------------------------------------------
#  h <- list(
#    proxy = "<proxy>",
#    proxyport = <port>,
#    proxyusername = "<username>",
#    proxypassword = "<password>"
#  )

## ---- eval = FALSE------------------------------------------------------------
#  options(rdbnomics.curl_config = h)

## ---- eval = FALSE------------------------------------------------------------
#  hndl <- curl::new_handle()
#  curl::handle_setopt(hndl, .list = getOption("rdbnomics.curl_config"))
#  curl::curl_fetch_memory(url = <...>, handle = hndl)

## ---- eval = FALSE------------------------------------------------------------
#  df1 <- rdb(ids = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN")

## ---- eval = FALSE------------------------------------------------------------
#  options(rdbnomics.curl = NULL)

## ---- eval = FALSE------------------------------------------------------------
#  df1 <- rdb(ids = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN", curl_config = h)

## ---- eval = FALSE------------------------------------------------------------
#  options(rdbnomics.use_readLines = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  df1 <- rdb(ids = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN")

## ---- eval = FALSE------------------------------------------------------------
#  options(rdbnomics.use_readLines = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  df1 <- rdb(ids = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN", use_readLines = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  filters <- list(
#    code = "interpolate",
#    parameters = list(frequency = "monthly", method = "spline")
#  )

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb(
#    ids = c("AMECO/ZUTN/EA19.1.0.0.0.ZUTN", "AMECO/ZUTN/DNK.1.0.0.0.ZUTN"),
#    filters = filters
#  )

## ---- eval = FALSE------------------------------------------------------------
#  filters <- list(
#    list(
#      code = "interpolate",
#      parameters = list(frequency = "monthly", method = "spline")
#    ),
#    list(
#      code = "aggregate",
#      parameters = list(frequency = "bi-annual", method = "end_of_period")
#    )
#  )
#  
#  df <- rdb(
#    ids = c("AMECO/ZUTN/EA19.1.0.0.0.ZUTN", "AMECO/ZUTN/DNK.1.0.0.0.ZUTN"),
#    filters = filters
#  )

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df013

## ---- echo = FALSE------------------------------------------------------------
df %>%
  arrange(filtered, series_name, period) %>%
  reorder_cols() %>%
  display_table()

## ---- fig.align = 'center'----------------------------------------------------
ggplot(filter(df, !is.na(value)), aes(x = period, y = value, color = series_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  dbnomics()

## ---- eval = FALSE------------------------------------------------------------
#  dbnomics <- function(color_palette = "Set1", ...) {
#    # Check if ggplot2 is installed.
#    ggplot2_ok <- try(utils::packageVersion("ggplot2"), silent = TRUE)
#    if (inherits(ggplot2_ok, "try-error")) {
#      stop(
#        "Please run install.packages('ggplot2') to use dbnomics().",
#        call. = FALSE
#      )
#    }
#  
#    # DBnomics vignette theme
#    result <- list(
#      ggplot2::scale_x_date(expand = c(0, 0)),
#      ggplot2::scale_y_continuous(
#        labels = function(x) { format(x, big.mark = " ") }
#      ),
#      ggplot2::xlab(""),
#      ggplot2::ylab(""),
#      ggplot2::theme_bw(),
#      ggplot2::theme(
#        legend.position = "bottom", legend.direction = "vertical",
#        legend.background = ggplot2::element_rect(
#          fill = "transparent", colour = NA
#        ),
#        legend.key = ggplot2::element_blank(),
#        panel.background = ggplot2::element_rect(
#          fill = "transparent", colour = NA
#        ),
#        plot.background = ggplot2::element_rect(
#          fill = "transparent", colour = NA
#        ),
#        legend.title = ggplot2::element_blank()
#      ),
#      ggplot2::theme(...),
#      ggplot2::annotate(
#        geom = "text", label = "DBnomics <https://db.nomics.world>",
#        x = structure(Inf, class = "Date"), y = -Inf,
#        hjust = 1.1, vjust = -0.4, col = "grey",
#        fontface = "italic"
#      )
#    )
#  
#    if (!is.null(color_palette)) {
#      result <- c(
#        result,
#        list(ggplot2::scale_color_brewer(palette = color_palette))
#      )
#    }
#  
#    result
#  }

