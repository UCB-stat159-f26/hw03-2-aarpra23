# Check that a player name is a single non-empty string
# @param player_name Name of the player.

check_player_name = function(player_name) {
    is_valid = is.character(player_name) && length(player_name) == 1 && !is.na(player_name) && nzchar(trimws(player_name))
    if (!is_valid) {
        stop("`player_name` must be a single string, e.g. \"Serena Williams\".", call. = FALSE)
    }
    invisible(TRUE)
}

# Turn a player name into the Tennis Abstract format
# "serena williams", "Serena Williams" and "SerenaWilliams" all become "SerenaWilliams". Hyphens and apostrophes are dropped.
# @param player_name Name of the player.
# @return The name with each word capitalized and no spaces.

format_player_name = function(player_name) {
    words = strsplit(trimws(player_name), "[^A-Za-z]+")[[1]]
    words = words[nzchar(words)]
    words = paste0(toupper(substr(words, 1, 1)), substr(words, 2, nchar(words)))
    paste(words, collapse = "")
}

# Build the URL of a player's career JS file
# @param player_id Player name in Tennis Abstract format, e.g. "SerenaWilliams".
# @return URL of the career JS file.

build_career_url = function(player_id) {
    paste0("https://www.tennisabstract.com/jsmatches/", player_id, "Career.js")
}