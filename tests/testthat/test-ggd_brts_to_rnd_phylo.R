context("ggd_brts_to_rnd_phylo")

test_that("use, 2 taxa", {
  phylo <- ggdaisie:::ggd_brts_to_rnd_phylo(branching_times = c(1, 2))
  expect_true(class(phylo) == "phylo")
  expect_equal(ggdaisie:::get_crown_age(phylo), 2)
})

test_that("use", {
  phylo <- ggdaisie:::ggd_brts_to_rnd_phylo(branching_times = c(1, 3, 2))
  expect_true(class(phylo) == "phylo")
  expect_equal(ggdaisie:::get_crown_age(phylo), 3)
})
