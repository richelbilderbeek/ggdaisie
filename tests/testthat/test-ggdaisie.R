context("ggdaisie")

test_that("use", {


  df <- get_daisie_data("Galapagos_datatable.RData")
  expect_silent(
    ggdaisie(df, island_age = 4)
  )

})

test_that("use", {


  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Status == "Non_endemic", ]
  df <- cbind(df)
  expect_silent(
    ggdaisie(df, island_age = 4)
  )
})
