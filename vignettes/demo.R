## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(DAISIE)
library(ggdaisie)

## ------------------------------------------------------------------------
df <- get_daisie_data("Galapagos_datatable.RData")
fig_order <- c(2, 7, 5, 3, 6, 8, 1, 4)
testit::assert(fig_order == unique(fig_order))
df <- df[ order(fig_order), ]
rownames(df) <- NULL
knitr::kable(df)

## ----fig.width=7, fig.height=7-------------------------------------------
#ggdaisie(df, island_age = 4)

