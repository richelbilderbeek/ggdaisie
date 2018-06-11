context("ggd_brts_to_rnd_phylo")

test_that("use, 1 branching time, thus 2 taxa", {
  phylo <- ggdaisie:::ggd_brts_to_rnd_phylo(branching_times = c(42))
  expect_true(class(phylo) == "phylo")
  expect_equal(ggdaisie:::get_crown_age(phylo), 42)
  expect_equal(ape::Ntip(phylo), 2)
  expect_silent(ape::plot.phylo(phylo))
})

test_that("use, 2 branching times, thus 3 taxa", {
  phylo <- ggdaisie:::ggd_brts_to_rnd_phylo(branching_times = c(1, 2))
  expect_true(class(phylo) == "phylo")
  expect_equal(ggdaisie:::get_crown_age(phylo), 2)
  expect_equal(ape::Ntip(phylo), 3)
  expect_silent(ape::plot.phylo(phylo))
})

test_that("use", {
  phylo <- ggdaisie:::ggd_brts_to_rnd_phylo(branching_times = c(1, 3, 2))
  expect_true(class(phylo) == "phylo")
  expect_equal(ggdaisie:::get_crown_age(phylo), 3)
  expect_equal(ape::Ntip(phylo), 4)
  expect_silent(ape::plot.phylo(phylo))
})
