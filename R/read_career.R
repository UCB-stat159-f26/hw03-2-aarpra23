# Download the text of a career JS file
# @param url URL of the career JS file.
# @param player_name Name typed by the user, used in the error message.
# @return The file contents as one string.

read_career_js = function(url, player_name) {
    js_lines = tryCatch(suppressWarnings(readLines(url, warn = FALSE)), error = function(e) NULL)
    if (is.null(js_lines) || !any(grepl("matchmx", js_lines))) {
        stop("No career data found for \"", player_name, "\", check the spelling.", call. = FALSE)
    } 
    paste(js_lines, collapse = "\n")
}

# Pull the match array out of the JS text
# Keeps everything from the first "[" to the last "]" and parses it as JSON.
# @param js_text Contents of a career JS file.
# @return A list with one character vector per match.

extract_match_array = function(js_text) {
    start = regexpr("[", js_text, fixed = TRUE)
    end = max(gregexpr("]", js_text, fixed = TRUE)[[1]])
    match_rows = jsonlite::fromJSON(substr(js_text, start, end), simplifyVector = FALSE)
    lapply(match_rows, unlist)
}