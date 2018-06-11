context("get_crown_age")

test_that("use", {
  phylogeny <- ape::read.tree(text = "(a:15,b:15):1;")
  created <- get_crown_age(phylogeny)
  testit::assert(created == 15)
})
