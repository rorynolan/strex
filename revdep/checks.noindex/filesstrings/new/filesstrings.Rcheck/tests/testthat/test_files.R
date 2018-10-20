context("Files")

test_that("nice_file_nums works", {
  setwd(tempdir())
  expect_true(dir.create("nice_file_nums_test"))
  setwd("nice_file_nums_test")
  files <- c("1litres_1.txt", "1litres_30.txt", "3litres_5.txt")
  expect_equal(file.create(files), rep(TRUE, 3))
  expect_equal(nice_file_nums(), rep(TRUE, 3))
  expect_equal(nice_file_nums(pattern = "\\.txt$"), rep(TRUE, 3))
  setwd("..")
  expect_true(dir.remove("nice_file_nums_test"))
})

test_that("remove_filename_spaces works", {
  cwd <- setwd(tempdir())
  on.exit(setwd(cwd))
  expect_true(dir.create("remove_filename_spaces_test"))
  setwd("remove_filename_spaces_test")
  files <- c("1litres 1.txt", "1litres 30.txt", "3litres 5.txt")
  expect_equal(file.create(files), rep(TRUE, 3))
  expect_equal(remove_filename_spaces(), rep(TRUE, 3))
  expect_equal(list.files(), c("1litres1.txt", "1litres30.txt", "3litres5.txt"))
  expect_equal(remove_filename_spaces(), logical())
  setwd("..")
  expect_true(dir.remove("remove_filename_spaces_test"))
  file.create(c("1litres 1.txt", "1litres1.txt"))
  expect_error(remove_filename_spaces(),
               str_c("Not renaming because to do so would also overwrite.*",
                     "1litres 1.txt.*and.*1litres1.txt.*already exist.*",
                     "so to rename.*1litres 1.txt.*to.*1litres1.txt.*",
                     "would be to overwrite.*1litres1.txt"))
  setwd(cwd)
})

test_that("rename_with_nums works", {
  cwd <- setwd(tempdir())
  on.exit(setwd(cwd))
  expect_true(dir.create("rename_with_nums_test"))
  setwd("rename_with_nums_test")
  files <- c("1litres 1.txt", "1litres 30.txt", "3litres 5.txt")
  expect_equal(file.create(files), rep(TRUE, 3))
  expect_equal(rename_with_nums(pattern = ".txt$"), rep(TRUE, 3))
  expect_equal(list.files(pattern = ".txt$"), paste0(1:3, ".txt"))
  expect_error(rename_with_nums(),
               str_c("Some of the names are already in the desired format.*",
                     "[Uu]nable to proceed as renaming may result in deletion"))
  file.create("xyz.csv")
  expect_error(rename_with_nums(),
               "Files matching pattern have different extensions.")
  file.remove(dir(pattern = ".txt$"))
  file.remove(dir(pattern = ".csv$"))
  expect_error(rename_with_nums(pattern = ".txt$"), "No files found to rename.")
  setwd("..")
  expect_true(dir.remove("rename_with_nums_test"))
  setwd(cwd)
})

test_that("create_dir works", {
  setwd(tempdir())
  expect_equal(create_dir(c("mydir", "yourdir")), rep(TRUE, 2),
               check.names = FALSE)
  expect_equal(create_dir(c("mydir", "yourdir")), rep(FALSE, 2),
               check.names = FALSE)
  expect_equal(dir.remove(c("mydir", "yourdir")), rep(TRUE, 2),
               check.names = FALSE)
})

test_that("unitize_dirs works", {
  cwd <- setwd(tempdir())
  on.exit(setwd(cwd))
  expect_equal(dir.create("unitize_dirs_test"), TRUE)
  setwd("unitize_dirs_test")
  files <- c("1litres_1.txt", "1litres_3.txt", "3litres.txt", "5litres_1.txt")
  expect_equal(file.create(files), rep(TRUE, length(files)))
  expect_true(unitize_dirs("litres", "\\.txt"))
  file.create("10ml.txt")
  setwd("..")
  expect_error(unitize_dirs("litres"))
  expect_true(dir.remove("unitize_dirs_test"))
  setwd(cwd)
})

test_that("file.move edge cases work correctly", {
  cwd <- setwd(tempdir())
  on.exit(setwd(cwd))
  dir.create("tmpdir0")
  file.create("tmpfile0.R")
  expect_error(file.move("tmpfile0.R", c("tmpdir0", "tmpdir0")),
               "number of destinations must.*1 or.*number of files to be moved")
  expect_error(file.move(c("tmpfile0.R", "tmpfile0.R"),
                         c("tmpdir0", "tmpdir0")),
               "must not have.*duplicated elements.*Element 2.*duplicate")
  file.move("tmpfile0.R", "tmpdir0/")
  file.create("tmpfile0.R")
  expect_message(file.move("tmpfile0.R", "tmpdir0/"),
                 "To allow overwriting, use `overwrite = TRUE`")
  expect_message(file.move("tmpfile0.R", "tmpdir0/", overwrite = TRUE),
                 "1 file moved. 0 failed.")
  expect_message(file.move(character(0), character(0)),
                 "0 files moved. 0 failed.")
  setwd(cwd)
})
