context("plot_phylo_with_stem")

test_that("use", {
  phylo <- ape::read.tree(text = "(A:1,B:1):20;")
  expect_silent(
    ggdaisie:::plot_phylo_with_stem(phylo) +
    ggtree::geom_treescale()
  )
})

test_that("ape", {

  phylo <- ape::read.tree(text = "(A:1,B:1):20;")
  expect_silent({
    ggdaisie:::plot_phylo_with_stem_ape(phylo)
    ape::add.scale.bar()
    }
  )
})

test_that("ggtree", {

  phylo <- ape::read.tree(text = "(A:1,B:1):20;")
  expect_silent(
    ggdaisie:::plot_phylo_with_stem_ggtree(phylo)
  )
})

