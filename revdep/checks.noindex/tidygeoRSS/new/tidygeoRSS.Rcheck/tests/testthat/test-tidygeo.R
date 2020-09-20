
# utils.R
# test_that("HTML gets stripped correctly", {
#   ht <- "<h4>GeoRSS ::<br/>Geographically Encoded Objects for RSS feeds</h4>
#   <h2>GeoRSS Simple</h2>
#   <p>The Simple serialization of GeoRSS is designed to be maximally concise.</p>"
#   
#   expect_equal(strip_html(ht), 
#                "GeoRSS ::Geographically Encoded Objects for RSS feeds\n  GeoRSS Simple\n  The Simple serialization of GeoRSS is designed to be maximally concise.")
# })
test_that("check_p works", {
  expect_equal(check_p(c(NA, NA)), FALSE)
  expect_equal(check_p(c(NA, "NA")), FALSE)
  expect_equal(check_p(c("NA", "NA")), TRUE)
})
# # type_check.R
# # use mocking
# test_that("type check returns correctly", {
#   expect_equal()type_check
# })