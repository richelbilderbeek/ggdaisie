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
  df <- rbind(df, df)
  df$Branching_times[2] <- 3 # Fake
  expect_silent(
    ggdaisie(df, island_age = 4)
  )
})
