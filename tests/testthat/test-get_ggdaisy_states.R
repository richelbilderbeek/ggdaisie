context("get_ggdaisy_states")

test_that("use", {

  expect_true(all(get_daisy_input_statuses() %in% get_ggdaisy_states()))
  expect_true("invisible" %in% get_ggdaisy_states())
})
