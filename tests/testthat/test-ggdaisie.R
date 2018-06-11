context("ggdaisie")

test_that("use", {

  df <- get_daisie_data("Galapagos_datatable.RData")
  ggdaisie(df)

})
