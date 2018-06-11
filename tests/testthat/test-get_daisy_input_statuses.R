context("get_daisy_input_statuses")

test_that("use", {
  expect_true("Endemic" %in% get_daisy_input_statuses())
  expect_true("Non_endemic" %in% get_daisy_input_statuses())
  expect_true("Non_endemic_MaxAge" %in% get_daisy_input_statuses())
  expect_true("Endemic&Non_Endemic" %in% get_daisy_input_statuses())
})
