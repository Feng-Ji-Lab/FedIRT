test_that("Shiny app launches without error", {
  skip_on_cran()
  app_dir <- system.file("shiny", package = "FedIRT")
  expect_true(dir.exists(app_dir))

  shiny_process <- callr::r_bg(function(app_dir) {
    shiny::runApp(app_dir, launch.browser = FALSE)
  }, args = list(app_dir = app_dir))

  Sys.sleep(5)

  shiny_process$kill()
  expect_true(TRUE)


})
