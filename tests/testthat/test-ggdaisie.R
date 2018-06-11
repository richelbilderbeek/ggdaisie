context("ggdaisie")

test_that("use", {
  df <- get_daisie_data("Galapagos_datatable.RData")
  ggdaisie(df, island_age = 4)
})

test_that("use, Non_endemic_MaxAge", {
  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Status == "Non_endemic_MaxAge", ]
  ggdaisie(df, island_age = 4)
})

test_that("use, Non_endemic", {
  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Status == "Non_endemic", ]
  # Duplicate
  df <- rbind(df, df)
  df$Branching_times[2] <- 3 # Fake
  ggdaisie(df, island_age = 4)
})

test_that("use, Non_endemic_MaxAge and Non_endemic", {
  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Status == "Non_endemic_MaxAge" | df$Status == "Non_endemic", ]
  ggdaisie(df, island_age = 4)
})

test_that("use, Endemic", {
  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Status == "Endemic", ]
  ggdaisie(df, island_age = 4)
})

test_that("use, Endemic with many branches", {
  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Clade_name == "Finches", ]
  ggdaisie(df, island_age = 4)
})
