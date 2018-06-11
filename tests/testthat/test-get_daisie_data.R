context("get_daisie_data")

test_that("use", {

  load(system.file("data", "Galapagos_datatable.RData", package = "DAISIE"))
  expected <- Galapagos_datatable
  expected$Branching_times <- as.character(expected$Branching_times)
  rm(Galapagos_datatable)

  created <- get_daisie_data(filename = "Galapagos_datatable.RData")

  expect_equal(created, expected)
})

test_that("abuse", {

  expect_error(
    get_daisie_data("abs.ent"),
    "'filename' absent."
  )
})
