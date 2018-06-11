context("ggd_create_phylos")

test_that("use", {

  df <- get_daisie_data("Galapagos_datatable.RData")
  phylos <- ggd_create_phylos(df, island_age = 4)
  expect_true(class(phylos) == "multiPhylo")
  expect_true(length(phylos) >= 3)
})

test_that("use Non_endemic", {

  island_age <- 4
  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Status == "Non_endemic", ]
  df <- rbind(df, df)
  df$Branching_times[2] <- 3
  phylos <- ggd_create_phylos(df, island_age = island_age)
  expect_true(class(phylos) == "multiPhylo")
  expect_true(length(phylos) == 2)
  expect_equal(island_age, get_crown_age(phylos[[1]]))
  expect_equal(island_age, get_crown_age(phylos[[2]]))
})

test_that("use Endemic", {
  island_age <- 4
  df <- get_daisie_data("Galapagos_datatable.RData")
  df <- df[ df$Status == "Endemic", ]
  phylos <- ggd_create_phylos(df, island_age = island_age)
  expect_true(class(phylos) == "multiPhylo")
  expect_true(length(phylos) == 2)
  expect_equal(island_age, ggdaisie:::get_crown_age(phylos[[1]]))
  expect_equal(island_age, ggdaisie:::get_crown_age(phylos[[2]]))
})
