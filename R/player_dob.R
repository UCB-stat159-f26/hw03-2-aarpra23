# TRUE if the file is a female player's career (opponent birthdays instead of ages)

is_female = function(opponent_info) {
    any(grepl("^[0-9]{8}$", opponent_info))
}

# URL of the player's Tennis Abstract page (female player's pages use wplayer.cgi)

player_page_url = function(player_id, female) {
    page = if (female) "wplayer.cgi" else "player.cgi"
    paste0("https://www.tennisabstract.com/cgi-bin/", page, "?p=", player_id)
}

# Find "var dob = YYYYMMDD" in the page text and return it as a date
extract_dob = function(page_text) {
    dob_text = regmatches(page_text, regexpr("var dob = [0-9]{8}", page_text))
    if (length(dob_text) == 0) {
        stop("Could not find the player's birth date on tennisabstract.com.", call. = FALSE)
    }
    as.Date(sub("var dob = ", "", dob_text), format = "%Y%m%d")
}
# Download the player page and return the player's birthday

read_player_dob = function(player_id, female) {
    url = player_page_url(player_id, female)
    page_lines = tryCatch(readLines(url, warn = FALSE), error = function(e) NULL)
    if (is.null(page_lines)) {
        stop("Could not download the player page to find the birthdate", call. = FALSE)
    }
    extract_dob(paste(page_lines, collapse = "\n"))
}

