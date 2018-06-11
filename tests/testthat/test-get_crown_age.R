context("get_crown_age")

test_that("use", {
  phylogeny <- ape::read.tree(text = "(a:15,b:15):1;")
  created <- get_crown_age(phylogeny)
  expect_equal(15, created)
})
