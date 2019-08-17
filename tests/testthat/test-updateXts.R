context("test-updateXts")

test_that("basic", {
  library(xts)
  date_1 = seq(as.Date("2000-01-01"), as.Date("2000-01-10"), "days")

  x = as.xts(1:10, order.by = date_1)

  date_2 = seq(as.Date("2000-01-09"), as.Date("2000-01-12"), "days")

  new_data = as.xts(101:104, order.by = date_2)

  expect_equal(as.numeric(updateXts(x, new_data)["2000-01-09"]), 101)


  df_1 = data.frame(a = 1:10, b = 2:11)

  x = as.xts(df_1, order.by = date_1)

  df_2 =  data.frame(a = 101:104, b = 102:105)
  new_data = as.xts(df_2, order.by = date_2)

  expect_equal(as.numeric(updateXts(x, new_data)["2000-01-09"]), c(101, 102))

  expect_equal(as.numeric(update(x, new_data)["2000-01-09"]), c(101, 102))


})


test_that("cross", {
  library(xts)
  date_1 = seq(as.Date("2000-01-01"), as.Date("2000-01-10"), "days")

  x = as.xts(1:10, order.by = date_1)

  date_2 = seq(as.Date("2000-01-09"), as.Date("2000-01-12"), "days")

  new_data = as.xts(101:104, order.by = date_2)

  suppressWarnings({
    new_data = rbind(as.xts(100, order.by = as.Date("2000-01-05")), new_data)
  })

  expect_equal(as.numeric(updateXts(x, new_data)["2000-01-09"]), 101)

  expect_equal(as.numeric(updateXts(x, new_data)["2000-01-05"]), 100)


  expect_equal(updateXts(x, x["1990-01-01"]), x)

})

test_that("errors", {
  library(xts)

  date_1 = seq(as.Date("2000-01-01"), as.Date("2000-01-10"), "days")

  x = as.xts(1:10, order.by = date_1)

  expect_error(updateXts(x, NULL))

  expect_error(updateXts(NULL, x))

  expect_error(updateXts(x["1990-01-01"], x["1991-01-01"]))

  expect_error(as.numeric(update(x)))

})

test_that("different names", {
  date_1 = seq(as.Date("2000-01-01"), as.Date("2000-01-10"), "days")

  x = as.xts(1:10, order.by = date_1)

  date_2 = seq(as.Date("2000-01-09"), as.Date("2000-01-12"), "days")

  new_data = as.xts(101:104, order.by = date_2)

  df_1 = data.frame(a = 1:10, b = 2:11)

  x = as.xts(df_1, order.by = date_1)

  df_2 =  data.frame(x = 101:104, y = 102:105)
  new_data = as.xts(df_2, order.by = date_2)

  expect_error(updateXts(x, new_data))


})
