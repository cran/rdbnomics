## ---- echo = FALSE------------------------------------------------------------
library <- function(...) {
  suppressWarnings(
    suppressPackageStartupMessages(base::library(..., quietly = TRUE))
  )
}

## -----------------------------------------------------------------------------
library(data.table)
library(rdbnomics)

## ---- echo = FALSE------------------------------------------------------------
reorder_cols <- function(x) {
  data.table::setDT(x)

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

  x[, .SD, .SDcols = cols]
}

knitr::opts_chunk$set(dev.args = list(bg = "transparent"))

display_table <- function(DT) {
  DT <- head(DT)
  DT <- as.data.table(
    lapply(DT, function(x) {
      if (is.character(x)) {
        ifelse(
          nchar(x) > 16,
          paste0(substr(x, 1, 16), "..."),
          x
        )
      } else {
        x
      }
    })
  )
  DT[]
}

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb(ids = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN")
#  df <- df[!is.na(value)]

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df001
data.table::setDT(df)

## ---- echo = FALSE------------------------------------------------------------
df <- reorder_cols(df)
display_table(df)

## ---- echo = FALSE, fig.align = 'center'--------------------------------------
# ggplot(df, aes(x = period, y = value, color = series_name)) +
#   geom_line(size = 1.2) +
#   geom_point(size = 2) +
#   dbnomics()
i <- 1
x1 <- df[series_name == sort(unique(series_name))[i]]$period
y1 <- df[series_name == sort(unique(series_name))[i]]$value
cols <- c("red", "blue", "darkgreen")
PCH <- 18

plot(
  x1, y1, col = cols[1], type = "l", xlab = "", ylab = "",
  xlim = c(min(df$period), max(df$period)),
  ylim = c(min(df$value) - 0.5, max(df$value)),
  panel.first = grid(lty = 1)
)
points(x1, y1, col = cols[1], pch = PCH)
legend(
  "bottomleft", inset = 0.005,
  legend = sort(unique(df$series_name)),
  col = cols,
  lty = 1, pch = PCH,  box.lty = 0, cex = 0.7
)
mtext(
  text = "DBnomics <https://db.nomics.world>",
  side = 3, col = "grey", font = 3, adj = 1
)

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("AMECO/ZUTN/EA19.1.0.0.0.ZUTN")

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb(ids = c("AMECO/ZUTN/EA19.1.0.0.0.ZUTN", "AMECO/ZUTN/DNK.1.0.0.0.ZUTN"))
#  df <- df[!is.na(value)]

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df002
data.table::setDT(df)

## ---- echo = FALSE------------------------------------------------------------
df <- df[order(series_code, period)]
df <- reorder_cols(df)
display_table(df)

## ---- echo = FALSE, fig.align = 'center'--------------------------------------
# ggplot(df, aes(x = period, y = value, color = series_name)) +
#   geom_line(size = 1.2) +
#   geom_point(size = 2) +
#   dbnomics()
i <- 1
x1 <- df[series_name == sort(unique(series_name))[i]]$period
y1 <- df[series_name == sort(unique(series_name))[i]]$value
i <- 2
x2 <- df[series_name == sort(unique(series_name))[i]]$period
y2 <- df[series_name == sort(unique(series_name))[i]]$value
cols <- c("red", "blue", "darkgreen")
PCH <- 18

plot(
  x1, y1, col = cols[1], type = "l", xlab = "", ylab = "",
  xlim = c(min(df$period), max(df$period)),
  ylim = c(min(df$value) - 1.7, max(df$value)),
  panel.first = grid(lty = 1)
)
points(x1, y1, col = cols[1], pch = PCH)
lines(x2, y2, col = cols[2], type = "l")
points(x2, y2, col = cols[2], pch = PCH)
legend(
  "bottomleft", inset = 0.005,
  legend = sort(unique(df$series_name)),
  col = cols,
  lty = 1, pch = PCH,  box.lty = 0, cex = 0.7
)
mtext(
  text = "DBnomics <https://db.nomics.world>",
  side = 3, col = "grey", font = 3, adj = 1
)

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb(ids = c("AMECO/ZUTN/EA19.1.0.0.0.ZUTN", "Eurostat/une_rt_q/Q.SA.Y15-24.PC_ACT.T.EA19"))
#  df <- df[!is.na(value)]

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df003
data.table::setDT(df)

## ---- echo = FALSE------------------------------------------------------------
df <- df[order(series_code, period)]
df <- reorder_cols(df)
display_table(df)

## ---- echo = FALSE, fig.align = 'center'--------------------------------------
# ggplot(df, aes(x = period, y = value, color = series_name)) +
#   geom_line(size = 1.2) +
#   geom_point(size = 2) +
#   dbnomics(legend.text = element_text(size = 7))
i <- 1
x1 <- df[series_name == sort(unique(series_name))[i]]$period
y1 <- df[series_name == sort(unique(series_name))[i]]$value
i <- 2
x2 <- df[series_name == sort(unique(series_name))[i]]$period
y2 <- df[series_name == sort(unique(series_name))[i]]$value
cols <- c("red", "blue", "darkgreen")
PCH <- 18
legend_text <- sort(unique(df$series_name))
legend_text[2] <- sapply(
  legend_text[2],
  function(y) {
    paste0(
      paste0(
        strsplit(y, "active ")[[1]], collapse = "active\n"
      ),
      "\n"
    )
  }
)

plot(
  x1, y1, col = cols[1], type = "l", xlab = "", ylab = "",
  xlim = c(min(df$period), max(df$period)),
  ylim = c(min(df$value) - 1.5, max(df$value)),
  panel.first = grid(lty = 1)
)
points(x1, y1, col = cols[1], pch = PCH)
lines(x2, y2, col = cols[2], type = "l")
points(x2, y2, col = cols[2], pch = PCH)
legend(
  "bottomleft", inset = 0.005,
  legend = legend_text,
  col = cols,
  lty = 1, pch = PCH,  box.lty = 0, cex = 0.7
)
mtext(
  text = "DBnomics <https://db.nomics.world>",
  side = 3, col = "grey", font = 3, adj = 1
)

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("IMF", "BOP", mask = "A.FR.BCA_BP6_EUR")
#  df <- df[!is.na(value)]

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df004
data.table::setDT(df)

## ---- echo = FALSE------------------------------------------------------------
df <- reorder_cols(df)
display_table(df)

## ---- echo = FALSE, fig.align = 'center'--------------------------------------
# ggplot(df, aes(x = period, y = value, color = series_name)) +
#   geom_step(size = 1.2) +
#   geom_point(size = 2) +
#   dbnomics()
i <- 1
x1 <- df[series_name == sort(unique(series_name))[i]]$period
y1 <- df[series_name == sort(unique(series_name))[i]]$value
cols <- c("red", "blue", "darkgreen")
PCH <- 18
legend_text <- sort(unique(df$series_name))

plot(
  x1, y1, col = cols[1], type = "s", xlab = "", ylab = "",
  xlim = c(min(df$period), max(df$period)),
  ylim = c(min(df$value), max(df$value)),
  panel.first = grid(lty = 1)
)
points(x1, y1, col = cols[1], pch = PCH)
legend(
  "bottomleft", inset = 0.005,
  legend = legend_text,
  col = cols,
  lty = 1, pch = PCH,  box.lty = 0, cex = 0.7
)
mtext(
  text = "DBnomics <https://db.nomics.world>",
  side = 3, col = "grey", font = 3, adj = 1
)

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("IMF", "BOP", "A.FR.BCA_BP6_EUR")

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("IMF", "BOP", mask = "A.FR+ES.BCA_BP6_EUR")
#  df <- df[!is.na(value)]

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df005
data.table::setDT(df)

## ---- echo = FALSE------------------------------------------------------------
df <- df[order(series_code, period)]
df <- reorder_cols(df)
display_table(df)

## ---- echo = FALSE, fig.align = 'center'--------------------------------------
# ggplot(df, aes(x = period, y = value, color = series_name)) +
#   geom_step(size = 1.2) +
#   geom_point(size = 2) +
#   dbnomics()
i <- 1
x1 <- df[series_name == sort(unique(series_name))[i]]$period
y1 <- df[series_name == sort(unique(series_name))[i]]$value
i <- 2
x2 <- df[series_name == sort(unique(series_name))[i]]$period
y2 <- df[series_name == sort(unique(series_name))[i]]$value
cols <- c("red", "blue", "darkgreen")
PCH <- 18
legend_text <- sort(unique(df$series_name))

plot(
  x1, y1, col = cols[1], type = "s", xlab = "", ylab = "",
  xlim = c(min(df$period), max(df$period)),
  ylim = c(min(df$value) - 2*10^4, max(df$value)),
  panel.first = grid(lty = 1)
)
points(x1, y1, col = cols[1], pch = PCH)
lines(x2, y2, col = cols[2], type = "s")
points(x2, y2, col = cols[2], pch = PCH)
legend(
  "bottomleft", inset = 0.005,
  legend = legend_text,
  col = cols,
  lty = 1, pch = PCH,  box.lty = 0, cex = 0.7
)
mtext(
  text = "DBnomics <https://db.nomics.world>",
  side = 3, col = "grey", font = 3, adj = 1
)

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("IMF", "BOP", mask = "A..BCA_BP6_EUR")
#  df <- df[!is.na(value)]
#  df <- df[order(-period, REF_AREA)]
#  df <- head(df, 100)

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df006
data.table::setDT(df)

## ---- echo = FALSE------------------------------------------------------------
df <- reorder_cols(df)
display_table(df)

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("IMF", "BOP", mask = "A.FR.BCA_BP6_EUR+IA_BP6_EUR")
#  df <- df[!is.na(value)]
#  df <- df[order(period), head(.SD, 50), by = INDICATOR]

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df007
data.table::setDT(df)

## ---- echo = FALSE------------------------------------------------------------
df <- reorder_cols(df)
display_table(df)

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("AMECO", "ZUTN", dimensions = list(geo = "ea19"))
#  df <- df[!is.na(value))]
#  # or
#  # df <- rdb("AMECO", "ZUTN", dimensions = '{"geo": ["ea19"]}')
#  # df <- df[!is.na(value))]

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df008
data.table::setDT(df)

## ---- echo = FALSE------------------------------------------------------------
df <- reorder_cols(df)
display_table(df)

## ---- echo = FALSE, fig.align = 'center'--------------------------------------
# ggplot(df, aes(x = period, y = value, color = series_name)) +
#   geom_line(size = 1.2) +
#   geom_point(size = 2) +
#   dbnomics()
i <- 1
x1 <- df[series_name == sort(unique(series_name))[i]]$period
y1 <- df[series_name == sort(unique(series_name))[i]]$value
cols <- c("red", "blue", "darkgreen")
PCH <- 18
legend_text <- sort(unique(df$series_name))

plot(
  x1, y1, col = cols[1], type = "l", xlab = "", ylab = "",
  xlim = c(min(df$period), max(df$period)),
  ylim = c(min(df$value) - 0.2, max(df$value)),
  panel.first = grid(lty = 1)
)
points(x1, y1, col = cols[1], pch = PCH)
legend(
  "bottomleft", inset = 0.005,
  legend = legend_text,
  col = cols,
  lty = 1, pch = PCH,  box.lty = 0, cex = 0.7
)
mtext(
  text = "DBnomics <https://db.nomics.world>",
  side = 3, col = "grey", font = 3, adj = 1
)

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("AMECO", "ZUTN", dimensions = list(geo = c("ea19", "dnk")))
#  df <- df[!is.na(value))]
#  # or
#  # df <- rdb("AMECO", "ZUTN", dimensions = '{"geo": ["ea19", "dnk"]}')
#  # df <- df[!is.na(value))]

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df009
data.table::setDT(df)

## ---- echo = FALSE------------------------------------------------------------
df <- df[order(series_code, period)]
df <- reorder_cols(df)
display_table(df)

## ---- echo = FALSE, fig.align = 'center'--------------------------------------
# ggplot(df, aes(x = period, y = value, color = series_name)) +
#   geom_line(size = 1.2) +
#   geom_point(size = 2) +
#   dbnomics()
i <- 1
x1 <- df[series_name == sort(unique(series_name))[i]]$period
y1 <- df[series_name == sort(unique(series_name))[i]]$value
i <- 2
x2 <- df[series_name == sort(unique(series_name))[i]]$period
y2 <- df[series_name == sort(unique(series_name))[i]]$value
cols <- c("red", "blue", "darkgreen")
PCH <- 18
legend_text <- sort(unique(df$series_name))

plot(
  x1, y1, col = cols[1], type = "l", xlab = "", ylab = "",
  xlim = c(min(df$period), max(df$period)),
  ylim = c(min(df$value) - 1.2, max(df$value)),
  panel.first = grid(lty = 1)
)
points(x1, y1, col = cols[1], pch = PCH)
lines(x2, y2, col = cols[2], type = "l")
points(x2, y2, col = cols[2], pch = PCH)
legend(
  "bottomleft", inset = 0.005,
  legend = legend_text,
  col = cols,
  lty = 1, pch = PCH,  box.lty = 0, cex = 0.7
)
mtext(
  text = "DBnomics <https://db.nomics.world>",
  side = 3, col = "grey", font = 3, adj = 1
)

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("WB", "DB", dimensions = list(country = c("DZ", "PE"), indicator = c("ENF.CONT.COEN.COST.ZS", "IC.REG.COST.PC.FE.ZS")))
#  df <- df[!is.na(value))]
#  # or
#  # df <- rdb("WB", "DB", dimensions = '{"country": ["DZ", "PE"], "indicator": ["ENF.CONT.COEN.COST.ZS", "IC.REG.COST.PC.FE.ZS"]}')
#  # df <- df[!is.na(value))]

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df010
data.table::setDT(df)

## ---- echo = FALSE------------------------------------------------------------
df <- df[order(series_name, period)]
df <- reorder_cols(df)
display_table(df)

## ---- echo = FALSE, fig.align = 'center'--------------------------------------
# ggplot(df, aes(x = period, y = value, color = series_name)) +
#   geom_line(size = 1.2) +
#   geom_point(size = 2) +
#   dbnomics()
i <- 1
x1 <- df[series_name == sort(unique(series_name))[i]]$period
y1 <- df[series_name == sort(unique(series_name))[i]]$value
i <- 2
x2 <- df[series_name == sort(unique(series_name))[i]]$period
y2 <- df[series_name == sort(unique(series_name))[i]]$value
i <- 3
x3 <- df[series_name == sort(unique(series_name))[i]]$period
y3 <- df[series_name == sort(unique(series_name))[i]]$value
i <- 4
x4 <- df[series_name == sort(unique(series_name))[i]]$period
y4 <- df[series_name == sort(unique(series_name))[i]]$value
cols <- c("red", "blue", "darkgreen", "purple")
PCH <- 18
legend_text <- sort(unique(df$series_name))

plot(
  x1, y1, col = cols[1], type = "l", xlab = "", ylab = "",
  xlim = c(min(df$period), max(df$period)),
  ylim = c(min(df$value) - 7, max(df$value)),
  panel.first = grid(lty = 1)
)
points(x1, y1, col = cols[1], pch = PCH)
lines(x2, y2, col = cols[2], type = "l")
points(x2, y2, col = cols[2], pch = PCH)
lines(x3, y3, col = cols[3], type = "l")
points(x3, y3, col = cols[3], pch = PCH)
lines(x4, y4, col = cols[4], type = "l")
points(x4, y4, col = cols[4], pch = PCH)
legend(
  "bottomleft", inset = 0.005,
  legend = legend_text,
  col = cols,
  lty = 1, pch = PCH,  box.lty = 0, cex = 0.7
)
mtext(
  text = "DBnomics <https://db.nomics.world>",
  side = 3, col = "grey", font = 3, adj = 1
)

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("IMF", "WEO:2019-10", query = "France current account balance percent")
#  df <- df[!is.na(value))]

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df014
data.table::setDT(df)

## ---- echo = FALSE------------------------------------------------------------
df <- reorder_cols(df)
display_table(df)

## ---- echo = FALSE, fig.align = 'center'--------------------------------------
# ggplot(df, aes(x = period, y = value, color = series_name)) +
#   geom_line(size = 1.2) +
#   geom_point(size = 2) +
#   dbnomics()
i <- 1
x1 <- df[series_name == sort(unique(series_name))[i]]$period
y1 <- df[series_name == sort(unique(series_name))[i]]$value
cols <- c("red", "blue", "darkgreen", "purple")
PCH <- 18
legend_text <- sort(unique(df$series_name))

plot(
  x1, y1, col = cols[1], type = "l", xlab = "", ylab = "",
  xlim = c(min(df$period), max(df$period)),
  ylim = c(min(df$value) - 0.5, max(df$value)),
  panel.first = grid(lty = 1)
)
points(x1, y1, col = cols[1], pch = PCH)
legend(
  "bottomleft", inset = 0.005,
  legend = legend_text,
  col = cols,
  lty = 1, pch = PCH,  box.lty = 0, cex = 0.7
)
mtext(
  text = "DBnomics <https://db.nomics.world>",
  side = 3, col = "grey", font = 3, adj = 1
)

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("IMF", "WEO:2019-10", query = "current account balance percent")
#  df <- df[!is.na(value))]

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df015
data.table::setDT(df)

## ---- echo = FALSE------------------------------------------------------------
df <- reorder_cols(df)
display_table(df)

## ---- echo = FALSE, fig.align = 'center'--------------------------------------
# ggplot(df, aes(x = period, y = value, color = `WEO Country`)) +
#   geom_line(size = 1.2) +
#   geom_point(size = 2) +
#   ggtitle("Current account balance (% GDP)") +
#   dbnomics(legend.direction = "horizontal")
i <- 1
x1 <- df[series_name == sort(unique(series_name))[i]]$period
y1 <- df[series_name == sort(unique(series_name))[i]]$value
i <- 2
x2 <- df[series_name == sort(unique(series_name))[i]]$period
y2 <- df[series_name == sort(unique(series_name))[i]]$value
i <- 3
x3 <- df[series_name == sort(unique(series_name))[i]]$period
y3 <- df[series_name == sort(unique(series_name))[i]]$value
i <- 4
x4 <- df[series_name == sort(unique(series_name))[i]]$period
y4 <- df[series_name == sort(unique(series_name))[i]]$value
cols <- c("red", "blue", "darkgreen", "purple")
PCH <- 18
legend_text <- sort(unique(df$series_name))

plot(
  x1, y1, col = cols[1], type = "l", xlab = "", ylab = "",
  xlim = c(min(df$period), max(df$period)),
  ylim = c(min(df$value), max(df$value)),
  panel.first = grid(lty = 1)
)
points(x1, y1, col = cols[1], pch = PCH)
lines(x2, y2, col = cols[2], type = "l")
points(x2, y2, col = cols[2], pch = PCH)
lines(x3, y3, col = cols[3], type = "l")
points(x3, y3, col = cols[3], pch = PCH)
lines(x4, y4, col = cols[4], type = "l")
points(x4, y4, col = cols[4], pch = PCH)
legend(
  "bottomleft", inset = 0.005,
  legend = legend_text,
  col = cols,
  lty = 1, pch = PCH,  box.lty = 0, cex = 0.7
)
mtext(
  text = "DBnomics <https://db.nomics.world>",
  side = 3, col = "grey", font = 3, adj = 1
)

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb(api_link = "https://api.db.nomics.world/v22/series/WB/DB?dimensions=%7B%22country%22%3A%5B%22FR%22%2C%22IT%22%2C%22ES%22%5D%7D&q=IC.REG.PROC.FE.NO&observations=1&format=json&align_periods=1&offset=0&facets=0")
#  df <- df[!is.na(value))]

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df011
data.table::setDT(df)

## ---- echo = FALSE------------------------------------------------------------
df <- df[order(period, series_name)]
df <- reorder_cols(df)
display_table(df)

## ---- echo = FALSE, fig.align = 'center'--------------------------------------
# ggplot(df, aes(x = period, y = value, color = series_name)) +
#   geom_step(size = 1.2) +
#   geom_point(size = 2) +
#   dbnomics()
i <- 1
x1 <- df[series_name == sort(unique(series_name))[i]]$period
y1 <- df[series_name == sort(unique(series_name))[i]]$value
i <- 2
x2 <- df[series_name == sort(unique(series_name))[i]]$period
y2 <- df[series_name == sort(unique(series_name))[i]]$value
i <- 3
x3 <- df[series_name == sort(unique(series_name))[i]]$period
y3 <- df[series_name == sort(unique(series_name))[i]]$value
cols <- c("red", "blue", "darkgreen")
PCH <- 18

plot(
  x1, y1, col = cols[1], type = "s", xlab = "", ylab = "",
  xlim = c(min(df$period), max(df$period)),
  ylim = c(min(df$value) - 1.2, max(df$value)),
  panel.first = grid(lty = 1)
)
points(x1, y1, col = cols[1], pch = PCH)
lines(x2, y2, col = cols[2], type = "s")
points(x2, y2, col = cols[2], pch = PCH)
lines(x3, y3, col = cols[3], type = "s")
points(x3, y3, col = cols[3], pch = PCH)
legend(
  "bottomleft", inset = 0.005,
  legend = sort(unique(df$series_name)),
  col = cols,
  lty = 1, pch = PCH,  box.lty = 0, cex = 0.7
)
mtext(
  text = "DBnomics <https://db.nomics.world>",
  side = 3, col = "grey", font = 3, adj = 1
)

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb("https://api.db.nomics.world/v22/series/WB/DB?dimensions=%7B%22country%22%3A%5B%22FR%22%2C%22IT%22%2C%22ES%22%5D%7D&q=IC.REG.PROC.FE.NO&observations=1&format=json&align_periods=1&offset=0&facets=0")

## ---- eval = FALSE------------------------------------------------------------
#  df <- rdb(api_link = "https://api.db.nomics.world/v22/series?observations=1&series_ids=BOE/6008/RPMTDDC,BOE/6231/RPMTBVE")
#  df <- df[!is.na(value))]

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
df <- rdbnomics:::rdbnomics_df012
data.table::setDT(df)

## ---- echo = FALSE------------------------------------------------------------
df[
    ,
    series_name := sapply(
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
  ]

## ---- echo = FALSE------------------------------------------------------------
df <- df[order(period, series_name)]
df <- reorder_cols(df)
display_table(df)

## ---- echo = FALSE, fig.align = 'center'--------------------------------------
# ggplot(df, aes(x = period, y = value, color = series_name)) +
#   geom_line(size = 1.2) +
#   geom_point(size = 2) +
#   dbnomics()
i <- 1
x1 <- df[series_name == sort(unique(series_name))[i]]$period
y1 <- df[series_name == sort(unique(series_name))[i]]$value
i <- 2
x2 <- df[series_name == sort(unique(series_name))[i]]$period
y2 <- df[series_name == sort(unique(series_name))[i]]$value
cols <- c("red", "blue", "darkgreen")
PCH <- 18

plot(
  x1, y1, col = cols[1], type = "l", xlab = "", ylab = "",
  xlim = c(min(df$period), max(df$period)),
  ylim = c(min(df$value) - 4*10^3, max(df$value)),
  panel.first = grid(lty = 1)
)
points(x1, y1, col = cols[1], pch = PCH)
lines(x2, y2, col = cols[2], type = "l")
points(x2, y2, col = cols[2], pch = PCH)
legend(
  "bottomleft", inset = 0.005,
  legend = sort(unique(df$series_name)),
  col = cols,
  lty = 1, pch = PCH,  box.lty = 0, cex = 0.7
)
mtext(
  text = "DBnomics <https://db.nomics.world>",
  side = 3, col = "grey", font = 3, adj = 1
)

## ---- eval = FALSE------------------------------------------------------------
#  rdb_datasets(provider_code = "IMF")

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
str(rdbnomics:::rdbnomics_df016)

## ---- eval = FALSE------------------------------------------------------------
#  rdb_datasets(provider_code = c("IMF", "BDF"))

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
str(rdbnomics:::rdbnomics_df018)

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
DT <- rdbnomics:::rdbnomics_df018
DT <- sapply(DT, function(y) { paste0(": ", nrow(y)) })
DT <- paste0("Number of datasets for ", names(DT), " ", unname(DT))
cat(DT, sep = "\n")

## ---- eval = FALSE------------------------------------------------------------
#  rdb_datasets(provider_code = "IMF", simplify = TRUE)

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
DT <- rdbnomics:::rdbnomics_df017
data.table::setDT(DT)
display_table(DT)

## ---- eval = FALSE------------------------------------------------------------
#  options(rdbnomics.progress_bar_datasets = TRUE)
#  rdb_datasets()
#  options(rdbnomics.progress_bar_datasets = FALSE)

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
DT <- rdbnomics:::rdbnomics_df019
DT <- data.table(Provider = names(DT), `Number of datasets` = sapply(DT, nrow))
DT <- DT[order(Provider)]
display_table(DT)

## ---- eval = FALSE------------------------------------------------------------
#  rdb_dimensions(provider_code = "IMF", dataset_code = "WEO:2019-10")

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
DT <- rdbnomics:::rdbnomics_df020
DT <- DT$IMF$`WEO:2019-10`
DT <- paste0("Number of dimensions for IMF/WEO:2019-10 : ", length(DT))
cat(DT, sep = "\n")

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
DT <- rdbnomics:::rdbnomics_df020
DT <- DT$IMF$`WEO:2019-10`[[1]]
display_table(DT)

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
DT <- rdbnomics:::rdbnomics_df020
DT <- DT$IMF$`WEO:2019-10`[[2]]
display_table(DT)

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
DT <- rdbnomics:::rdbnomics_df020
DT <- DT$IMF$`WEO:2019-10`[[3]]
display_table(DT)

## ---- eval = FALSE------------------------------------------------------------
#  rdb_dimensions(provider_code = "IMF", dataset_code = "WEO:2019-10", simplify = TRUE)

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
str(rdbnomics:::rdbnomics_df021)

## ---- eval = FALSE------------------------------------------------------------
#  options(rdbnomics.progress_bar_datasets = TRUE)
#  rdb_dimensions()
#  options(rdbnomics.progress_bar_datasets = FALSE)

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
DT <- rdbnomics:::rdbnomics_df022
DT <- DT[order(Provider, Dataset)]
DT <- head(DT, 100)
display_table(DT)

# rdbnomics_df022 %>%
#   sapply(function(u) {
#     sapply(
#       u,
#       function(x) {
#         sapply(
#           x,
#           function(y) {
#             nrow(y)
#           },
#           simplify = FALSE
#         ) %>%
#           {
#             data.table(Dimension = names(.), `Number of codes` = unname(.))
#           }
#       },
#       simplify = FALSE
#     ) %>%
#     rbindlist(idcol = "Dataset")
#   },
#   simplify = FALSE
# ) %>%
#   rbindlist(idcol = "Provider")

## ---- eval = FALSE------------------------------------------------------------
#  rdb_series(provider_code = "IMF", dataset_code = "WEO:2019-10", simplify = TRUE)

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
DT <- rdbnomics:::rdbnomics_df023
DT <- head(DT, 100)
display_table(DT)

## ---- eval = FALSE------------------------------------------------------------
#  rdb_series(provider_code = "IMF", dataset_code = "WEO:2019-10", dimensions = list(`weo-subject` = "NGDP_RPCH"), simplify = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  rdb_series(provider_code = "IMF", dataset_code = c("WEO:2019-10", "WEOAGG:2019-10"), query = "NGDP_RPCH")

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
DT <- rdbnomics:::rdbnomics_df024
DT <- DT[order(-`Number of series`)]
DT <- head(DT, 5)
display_table(DT)

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
data.table::setDT(df)

## ---- echo = FALSE------------------------------------------------------------
df <- df[order(filtered, series_name, period)]
df <- reorder_cols(df)
display_table(df)

## ---- echo = FALSE, fig.align = 'center'--------------------------------------
# ggplot(df[!is.na(value)], aes(x = period, y = value, color = series_name)) +
#   geom_line(size = 1.2) +
#   geom_point(size = 2) +
#   dbnomics()
df <- df[!is.na(value)]
i <- 1
x1 <- df[series_name == sort(unique(series_name))[i]]$period
y1 <- df[series_name == sort(unique(series_name))[i]]$value
i <- 2
x2 <- df[series_name == sort(unique(series_name))[i]]$period
y2 <- df[series_name == sort(unique(series_name))[i]]$value
i <- 3
x3 <- df[series_name == sort(unique(series_name))[i]]$period
y3 <- df[series_name == sort(unique(series_name))[i]]$value
i <- 4
x4 <- df[series_name == sort(unique(series_name))[i]]$period
y4 <- df[series_name == sort(unique(series_name))[i]]$value
cols <- c("red", "blue", "darkgreen", "purple")
PCH <- 18
legend_text <- sort(unique(df$series_name))

plot(
  x1, y1, col = cols[1], type = "l", xlab = "", ylab = "",
  xlim = c(min(df$period), max(df$period)),
  ylim = c(min(df$value) - 4, max(df$value)),
  panel.first = grid(lty = 1)
)
points(x1, y1, col = cols[1], pch = PCH)
lines(x2, y2, col = cols[2], type = "l")
points(x2, y2, col = cols[2], pch = PCH)
lines(x3, y3, col = cols[3], type = "l")
points(x3, y3, col = cols[3], pch = PCH)
lines(x4, y4, col = cols[4], type = "l")
points(x4, y4, col = cols[4], pch = PCH)
legend(
  "bottomleft", inset = 0.005,
  legend = legend_text,
  col = cols,
  lty = 1, pch = PCH,  box.lty = 0, cex = 0.7
)
mtext(
  text = "DBnomics <https://db.nomics.world>",
  side = 3, col = "grey", font = 3, adj = 1
)

