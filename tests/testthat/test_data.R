library(FedIRT)
library(testthat)
test_that('test data', {

  data(example_data_2PL)
  expect_equal(ncol(example_data_2PL),10)
  expect_equal(nrow(example_data_2PL),160)

  data(example_data_2PL_1)
  expect_equal(ncol(example_data_2PL_1),10)
  expect_equal(nrow(example_data_2PL_1),81)

  data(example_data_2PL_2)
  expect_equal(ncol(example_data_2PL_2),10)
  expect_equal(nrow(example_data_2PL_2),79)

  combined_data = rbind(example_data_2PL_1,example_data_2PL_2)
  expect_equal(combined_data,example_data_2PL)

  data(example_data_graded)
  expect_equal(ncol(example_data_graded),10)
  expect_equal(nrow(example_data_graded),100)

  data(example_data_graded_and_binary)
  expect_equal(ncol(example_data_graded_and_binary),8)
  expect_equal(nrow(example_data_graded_and_binary),81)


})
