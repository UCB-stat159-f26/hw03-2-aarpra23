#' Get the career matches of a tennis player
#'
#' Downloads a player's career match data from tennisabstract.com and returns a data frame with the player's and opponent ages, and the age gap at each match, ready to use with [plot_age_gap()].
#'
#' @param player_name Name of the player, e.g. `"serena williams"`, `"Serena Williams"` or `"SerenaWilliams"`.
#' @return A data frame with one row per match. The main columns are `date`, `opponent`, `player_age`, `opponent_age`, `age_diff` (player's age minus opponent's age), `outcome` ("Won" or "Lost") and `player`.
#' @export
#' @examples
#' \dontrun{
#' serena = get_career_matches("serena williams")
#' rafa = get_career_matches("Rafael Nadal")
#' }

get_career_matches = function(player_name) {
    check_player_name(player_name)
    player_id = format_player_name(player_name)
    js_text = read_career_js(build_career_url(player_id), player_name)
    matches = rows_to_df(extract_match_array(js_text))
    player_dob = read_player_dob(player_id, is_female(matches$opponent_info))
    matches = add_age_columns(matches, player_dob)
    matches$outcome = factor(matches$result, levels = c("W", "L"), labels = c("Won", "Lost"))
    matches$player = gsub("([a-z])([A-Z])", "\\1 \\2", player_id)
    matches
}
