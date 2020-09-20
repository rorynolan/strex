test_that("str_trim_anything works", {
  expect_equal(str_trim_anything("..abcd.", coll("."), "left"), "abcd.")
  expect_equal(
    str_trim_anything("..abcd.", coll("."), "left"),
    str_trim_anything("..abcd.", fixed("."), "left")
  )
  expect_equal(
    str_trim_anything("..abcd.", coll("."), "Ri"),
    str_trim_anything("..abcd.", coll("."), "r")
  )
  expect_equal(str_trim_anything("..abcd.", ".", "left"), "")
  expect_equal(str_trim_anything("-ghi--", "-"), "ghi")
  expect_equal(str_trim_anything("-ghi--", "--"), "-ghi")
  expect_equal(str_trim_anything("-ghi--", "--", "right"), "-ghi")
  expect_equal(str_trim_anything(character(), "a"), character())
  expect_equal(str_trim_anything("-ghi--", "i-+"), "-gh")
  expect_equal(str_trim_anything("-ghi--", "-"), "ghi")
  expect_equal(str_trim_anything(c("-ghi--", "xx"), "-"), c("ghi", "xx"))
  expect_equal(str_trim_anything(c("-ghi--", "xx"), "(-)+"), c("ghi", "xx"))
  expect_equal(
    str_trim_anything(c("tttattt", "ttatt", "tat"), "t"),
    rep("a", 3)
  )
  expect_error(
    str_trim_anything("x", boundary("word")),
    "`str_trim_anything()` cannot handle a `pattern` of type 'boundary'.",
    fixed = TRUE
  )
})
