test_that("constructs valid objects", {
  default_model <- animotum_ssm()
  expect_s3_class(default_model, "sporeg_animotum_ssm")
  expect_equal(default_model$model, "crw")

  custom_model <- animotum_ssm(model = "rw", time.step = 12, vmax = 3)
  expect_s3_class(custom_model, "sporeg_animotum_ssm")
  expect_equal(custom_model$model, "rw")
  expect_equal(custom_model$time.step, 12)
  expect_equal(custom_model$vmax, 3)
})
