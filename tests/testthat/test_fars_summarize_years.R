context("Test the fars summarize years outputs")

test_that("summarize years works",{
  expect_that(fars_summarize_years(2013),is_a("data.frame"))
}
          )