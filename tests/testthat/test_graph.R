test_that("graph_data handles minimal input with no optional metadata", {
  temp_dir <- tempdir()
  old_wd <- setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  df <- data.frame(A = rnorm(100))

  expect_message(
    graph_data(df, ci = 2),
    "Individual plots saved to working directory"
  )

  expect_true(file.exists("frame_001.png"))
  unlink("frame_*.png")
})

test_that("graph_data handles non-standard axis limits", {
  temp_dir <- tempdir()
  old_wd <- setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  df <- data.frame(A = rnorm(100))

  expect_message(
    graph_data(df, ci = 1.64, xlim = c(-3, 3)),
    "Individual plots saved to working directory"
  )

  expect_true(file.exists("frame_001.png"))
  unlink("frame_*.png")
})

test_that("graph_data warns or fills NA when optional `n` or `a` is incomplete", {
  temp_dir <- tempdir()
  old_wd <- setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  df <- data.frame(A = rnorm(100), B = rnorm(100))

  expect_message(
    graph_data(df, ci = 2, samples = 2, n = c(100, NA), a = c(0.05, NA)),
    "Individual plots saved to working directory"
  )

  expect_true(file.exists("frame_001.png"))
  expect_true(file.exists("frame_002.png"))
  unlink("frame_*.png")
})

test_that("graph_data works when ci = 0 and xlim includes 0", {
  temp_dir <- tempdir()
  old_wd <- setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  df <- data.frame(A = rnorm(100))

  expect_message(
    graph_data(df, ci = 0, xlim = c(-1, 1)),
    "Individual plots saved to working directory"
  )

  expect_true(file.exists("frame_001.png"))
  unlink("frame_*.png")
})

test_that("graph_data creates GIF and deletes PNGs when compile_gif = TRUE", {
  temp_dir <- tempdir()
  old_wd <- setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  df <- data.frame(A = rnorm(100), B = rnorm(100))

  expect_message(
    graph_data(df, ci = 1.96, compile_gif = TRUE, filename = "test_output.gif"),
    "Using ImageMagick to create gif..."
  )

  expect_true(file.exists("test_output.gif"))
  expect_false(any(grepl("^frame_\\d+\\.png$", list.files())))
  file.remove("test_output.gif")
})
