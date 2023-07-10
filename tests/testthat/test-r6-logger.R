test_that("Testing r6-logger functions", {
  expect_message(msg_function(NA), "skipped")
  expect_message(msg_function(TRUE), "finished")
  expect_message(msg_function(FALSE), "finished")
})
