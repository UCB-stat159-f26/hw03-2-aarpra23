# Get one field from every match, NA if a match is too short

pick_field = function(match_rows, position) {
    vapply(match_rows, function(row) {
        if (length(row) >= position) row[position] 
        else NA_character_
    }, character(1))
}

# Build a data frame with the columns needed for the plot

rows_to_df = function(match_rows) {
    matches = data.frame(
        date = pick_field(match_rows, 1),
        tournament = pick_field(match_rows, 2),
        surface = pick_field(match_rows, 3),
        result = pick_field(match_rows, 5),
        opponent = pick_field(match_rows, 12),
        opponent_info = pick_field(match_rows, 17))
    matches[matches == ""] = NA
    matches$date = as.Date(matches$date, format = "%Y%m%d")
    matches
}

# Opponent's age at each match
# Female files give a birthdate (8 digits) and male files give the age already

compute_opponent_age = function(match_date, opponent_info) {
    is_dob = grepl("^[0-9]{8}$", opponent_info)
    opponent_dob = as.Date(ifelse(is_dob, opponent_info, NA), format = "%Y%m%d")
    age_from_dob = as.numeric(match_date - opponent_dob) / 365.25
    age_given = suppressWarnings(as.numeric(opponent_info))
    ifelse(is_dob, age_from_dob, age_given)
}

# Add player and opponent ages, and the age gap (positive = player is older)

add_age_columns = function(matches, player_dob) {
    matches$opponent_age = compute_opponent_age(matches$date, matches$opponent_info)
    matches$player_age = as.numeric(matches$date - player_dob) / 365.25
    matches$age_diff = matches$player_age - matches$opponent_age
    matches
} 