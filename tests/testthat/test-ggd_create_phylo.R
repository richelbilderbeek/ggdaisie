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

test_that("use, Non_endemic, specialized function", {
  island_age <- 4
  phylo <- ggdaisie:::ggd_create_phylo_non_endemic(
    taxon_label = "Dendroica",
    immigration_time = 1.0,
    island_age = island_age
  )
  expect_true(class(phylo) == "phylo")
  expect_true("status" %in% names(attributes(phylo)))
  expect_true(all(attributes(phylo)$status %in% get_ggdaisy_states()))
  expect_true(all(levels(attributes(phylo)$status) %in% get_ggdaisy_states()))
  expect_equal(ggdaisie:::get_crown_age(phylo), island_age)
})

test_that("use, Endemic, 3 branching times", {
  phylo <- ggdaisie:::ggd_create_phylo(
    clade_name = "Mimus",
    status = "Endemic",
    branching_times = c(3.958, 3.422,2.884,0.459),
    island_age = 4
  )
  expect_true(class(phylo) == "phylo")
  expect_true("status" %in% names(attributes(phylo)))
  expect_true(all(attributes(phylo)$status %in% get_ggdaisy_states()))
  expect_true(all(levels(attributes(phylo)$status) %in% get_ggdaisy_states()))
})

test_that("use, Endemic, 3 branching times, specialized function", {
  island_age <- 4
  phylo <- ggdaisie:::ggd_create_phylo_endemic(
    clade_label = "Mimus",
    immigration_time = 3.958,
    branching_times = c(3.422,2.884,0.459),
    island_age = island_age
  )
  expect_true(class(phylo) == "phylo")
  expect_true("status" %in% names(attributes(phylo)))
  expect_true(all(attributes(phylo)$status %in% get_ggdaisy_states()))
  expect_true(all(levels(attributes(phylo)$status) %in% get_ggdaisy_states()))
  expect_equal(ggdaisie:::get_crown_age(phylo), island_age)
  expect_silent(ape::plot.phylo(phylo))
})

test_that("use, Endemic, 1 branching times", {
  phylo <- ggdaisie:::ggd_create_phylo(
    clade_name = "Mimus_subset",
    status = "Endemic",
    branching_times = c(3.958, 3.422),
    island_age = 4
  )
  expect_true(class(phylo) == "phylo")
  expect_true("status" %in% names(attributes(phylo)))
  expect_true(all(attributes(phylo)$status %in% get_ggdaisy_states()))
  expect_true(all(levels(attributes(phylo)$status) %in% get_ggdaisy_states()))
})

test_that("use, Endemic, 1 branching time, specialized function", {
  island_age <- 4
  phylo <- ggdaisie:::ggd_create_phylo_endemic(
    clade_label = "Mimus_subset",
    immigration_time = 3.958,
    branching_times = c(3.422),
    island_age = island_age
  )
  expect_true(class(phylo) == "phylo")
  expect_true("status" %in% names(attributes(phylo)))
  expect_true(all(attributes(phylo)$status %in% get_ggdaisy_states()))
  expect_true(all(levels(attributes(phylo)$status) %in% get_ggdaisy_states()))
  expect_equal(ggdaisie:::get_crown_age(phylo), island_age)
  expect_silent(ape::plot.phylo(phylo))
})

test_that("use, Endemic, 0 branching times", {
  phylo <- ggdaisie:::ggd_create_phylo(
    clade_name = "Myiarchus",
    status = "Endemic",
    branching_times = c(0.855),
    island_age = 4
  )
  expect_true(class(phylo) == "phylo")
  expect_true("status" %in% names(attributes(phylo)))
  expect_true(all(attributes(phylo)$status %in% get_ggdaisy_states()))
  expect_true(all(levels(attributes(phylo)$status) %in% get_ggdaisy_states()))
})

test_that("use, Endemic, 0 branching times, specialized function", {
  island_age <- 4
  phylo <- ggdaisie:::ggd_create_phylo_endemic(
    clade_label = "Myiarchus",
    immigration_time = 0.855,
    branching_times = c(),
    island_age = island_age
  )
  expect_true(class(phylo) == "phylo")
  expect_true("status" %in% names(attributes(phylo)))
  expect_true(all(attributes(phylo)$status %in% get_ggdaisy_states()))
  expect_true(all(levels(attributes(phylo)$status) %in% get_ggdaisy_states()))
  expect_equal(ggdaisie:::get_crown_age(phylo), island_age)
  expect_silent(ape::plot.phylo(phylo))
})
