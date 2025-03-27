context("Model Creation")

test_that("Create Model", {
  model <- createModel()
  expect_is(model, "list")
  # ...
})

# test_that("Alter Model", {
#    ...
# })
