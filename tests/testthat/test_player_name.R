test_that("format_player_name handles spaces and capitalization", {
    expect_equal(format_player_name("serena williams"), "SerenaWilliams")
    expect_equal(format_player_name("Serena Williams"), "SerenaWilliams")
    expect_equal(format_player_name("SerenaWilliams"), "SerenaWilliams")
    expect_equal(format_player_name("  rafael  nadal   "), "RafaelNadal")
})

test_that("check_player_name rejects invalid inputs", {
    expect_error(check_player_name(42), "single string")
    expect_error(check_player_name(c("a", "b")), "single string")
    expect_error(check_player_name(""), "single string")
    expect_error(check_player_name(NA_character_), "single string")
})

test_that("URLs are built correctly", {
    expect_equal(build_career_url("RafaelNadal"), "https://www.tennisabstract.com/jsmatches/RafaelNadalCareer.js")
    expect_match(player_page_url("SerenaWilliams", TRUE), "wplayer.cgi\\?p=SerenaWilliams$")
    expect_match(player_page_url("RafaelNadal", FALSE), "/player.cgi\\?p=RafaelNadal$")
})
