fake_matches = data.frame(
    player_age = c(20, 25, 30), 
    age_diff = c(-2, 0, 3), 
    outcome = factor(c("Won", "Lost", "Won"), levels = c("Won", "Lost")), 
    player = "Test Player"
)

test_that("plot_age_gap returns a ggplot", {
    expect_s3_class(plot_age_gap(fake_matches), "ggplot")
})

test_that("plot_age_gap rejects data that isn't from get_career_matches", {
    expect_error(plot_age_gap(mtcars), "get_career_matches")
})