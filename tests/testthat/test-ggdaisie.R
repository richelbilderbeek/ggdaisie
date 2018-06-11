context("ggdaisie")

test_that("use", {

  skip("WIP")
  df <- get_daisie_data("Galapagos_datatable.RData")
  expect_silent(
    ggdaisie(df, island_age = 4)
  )

})

test_that("use, Non_endemic_MaxAge", {


  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Status == "Non_endemic_MaxAge", ]
  expect_silent(
    ggdaisie(df, island_age = 4)
  )
})

test_that("use, Non_endemic", {


  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Status == "Non_endemic", ]
  # Duplicate
  df <- rbind(df, df)
  df$Branching_times[2] <- 3 # Fake
  expect_silent(
    ggdaisie(df, island_age = 4)
  )
})

test_that("use, Non_endemic_MaxAge and Non_endemic", {


  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Status == "Non_endemic_MaxAge" | df$Status == "Non_endemic", ]
  expect_silent(
    ggdaisie(df, island_age = 4)
  )
})

test_that("use, Endemic", {

  skip("WIP")
  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Status == "Endemic", ]
  expect_silent(
    ggdaisie(df, island_age = 4)
  )
})

test_that("use, Endemic with many branches", {

  skip("WIP")
  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Clade_name == "Finches", ]
  expect_silent(
    ggdaisie(df, island_age = 4)
  )
})
