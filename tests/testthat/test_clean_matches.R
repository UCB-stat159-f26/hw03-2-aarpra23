read_mock = function(file_name) {
    paste(readLines(test_path(file_name), warn = FALSE), collapse = "\n")
}

female_rows = extract_match_array(read_mock("female_player_career.js"))
male_rows = extract_match_array(read_mock("male_player_career.js"))

test_that("extract_match_array returns one vector per match", {
    expect_length(female_rows, 3)
    expect_length(male_rows, 3)
    expect_equal(female_rows[[1]][1], "19951030")
    expect_equal(male_rows[[1]][12], "Guillermo Platel")
})

test_that("rows_to_df pick the right columns", {
    matches = rows_to_df(female_rows)
    expect_equal(nrow(matches), 3)
    expect_s3_class(matches$date, "Date")
    expect_equal(matches$date[1], as.Date("1995-10-30"))
    expect_equal(matches$opponent[1], "Anne Miller")
    expect_equal(matches$result, c("L", "L", "L"))
})

test_that("pick_field gives NA when a match is too short", {
    expect_equal(pick_field(list(c("a", "b")), 5), NA_character_)
})

test_that("is_female tells the two file formats apart", {
    expect_true(is_female(rows_to_df(female_rows)$opponent_info))
    expect_false(is_female(rows_to_df(male_rows)$opponent_info))
})

test_that("opponent age is calculated from birthdates and read from ages", {
    female = rows_to_df(female_rows)
    male = rows_to_df(male_rows)
    expect_equal(round(compute_opponent_age(female$date, female$opponent_info)[1], 1), 18.8)
    expect_equal(round(compute_opponent_age(male$date, male$opponent_info)[1], 2), 22.26)
})

test_that("add_age_columns gives the player's age and the age gap", {
    female = add_age_columns(rows_to_df(female_rows), as.Date("1981-09-26"))
    male = add_age_columns(rows_to_df(male_rows), as.Date("1986-06-03"))
    expect_equal(round(female$player_age[1], 1), 14.1)
    expect_equal(round(female$age_diff[1], 1), -4.7)
    expect_equal(round(male$age_diff[1], 1), -7.0)
})

test_that("extract_dob reads the birthdate from the page text", {
    expect_equal(extract_dob("var fullname = 'X', var dob = 19810926;"), as.Date("1981-09-26"))
    expect_error(extract_dob("no birth date here"), "birth date")
})  