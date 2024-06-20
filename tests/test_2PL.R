library(FedIRT)
library(testthat)
test_that('test 2PL', {

  inputdata1 = list(as.matrix(example_data_2PL))
  fedresult1 = fedirt(inputdata1)

  inputdata2 = list(as.matrix(example_data_2PL_1), as.matrix(example_data_2PL_2))
  fedresult2 = fedirt(inputdata2)

  expect_equal(fedresult1[['a']],fedresult2[['a']])
  expect_equal(fedresult1[['b']],fedresult2[['b']])
  expect_equal(fedresult1[['loglik']],fedresult2[['loglik']])

  expect_equal(fedresult1[['a']], c(0.6630576,0.2394203,2.1177645,0.8156689,0.4185483,0.4195789,0.5181838,0.6657132,0.8049487,0.9141963),tolerance = 1e-2)
  expect_equal(fedresult1[['b']],c(-1.34716966, -4.51079294, -0.40097569, -0.52843935, -4.05022564, -2.71823636, -2.04151169, -1.06254190, -0.13873640, -0.09320751),tolerance = 1e-2)
  expect_equal(fedresult1[['loglik']],-957, tolerance = 1e-2)

})
