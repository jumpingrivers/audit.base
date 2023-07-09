test_that("Testing suppression functions", {
  for (i in 0:2) {
    expect_equal(get_debug_level(i), i)
  }
  expect_error(get_debug_level(3))

  suppress = get_suppress(0)
  expect_silent(suppress(message("test")))

  suppress = get_suppress(1)
  expect_message(suppress(message("test")), "test")

  suppress = get_suppress(2)
  expect_message(suppress(message("test")), "test")
})
