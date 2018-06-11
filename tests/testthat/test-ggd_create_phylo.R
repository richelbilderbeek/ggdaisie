context("ggd_create_phylo")

test_that("use, Non_endemic_MaxAge", {
  phylo <- ggdaisie:::ggd_create_phylo(
    clade_name = "Pyrocephalus rubinus",
    status = "Non_endemic_MaxAge",
    branching_times = 10.28,
    island_age = 4
  )
  expect_true(class(phylo) == "phylo")
  expect_true("status" %in% names(attributes(phylo)))
  expect_true(all(attributes(phylo)$status %in% get_ggdaisy_states()))
  expect_true(all(levels(attributes(phylo)$status) %in% get_ggdaisy_states()))
})

test_that("use, Non_endemic", {
  phylo <- ggdaisie:::ggd_create_phylo(
    clade_name = "Dendroica",
    status = "Non_endemic",
    branching_times = 1.0,
    island_age = 4
  )
  expect_true(class(phylo) == "phylo")
  expect_true("status" %in% names(attributes(phylo)))
  expect_true(all(attributes(phylo)$status %in% get_ggdaisy_states()))
  expect_true(all(levels(attributes(phylo)$status) %in% get_ggdaisy_states()))
})

test_that("use, Non_endemic", {
  phylo <- ggdaisie:::ggd_create_phylo_non_endemic(
    taxon_label = "Dendroica",
    immigration_time = 1.0,
    island_age = 4
  )
  #ape::plot.phylo(phylo); ape::add.scale.bar()
  expect_true(class(phylo) == "phylo")
  expect_true("status" %in% names(attributes(phylo)))
  expect_true(all(attributes(phylo)$status %in% get_ggdaisy_states()))
  expect_true(all(levels(attributes(phylo)$status) %in% get_ggdaisy_states()))
})
