# Stop if the dataframe didn't come from get_career_matches()

check_career_matches = function(career_matches) {
    needed = c("player_age", "age_diff", "outcome", "player")
    if (!is.data.frame(career_matches) || !all(needed %in% names(career_matches))) {
        stop("`career matches` must be a dataframe from get_career_matches().", call. = FALSE)
    }
    invisible(TRUE)
}

#' Plot a player's matches by age and age gap
#'
#' Makes a scatter plot like the NTY graphic of Serena Williams's matches. 
#' Each point is a match, placed by the player's age (x-axis) and the player's age minus the opponent's age (y-axis), colored by win or loss. 
#'
#' @param career_matches A dataframe returned by [get_career_matches()].
#' @return A ggplot object.
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' serena = get_career_matches("serena williams")
#' plot_age_gap(serena)
#' }

plot_age_gap = function(career_matches) {
    check_career_matches(career_matches)
    player = career_matches$player[1]
    keep = !is.na(career_matches$age_diff) & !is.na(career_matches$outcome)
    ggplot(career_matches[keep, ], aes(x = .data$player_age, y = .data$age_diff, color = .data$outcome)) +
    geom_hline(yintercept = 0, color = "grey50") +
    geom_point(alpha = 0.7, size = 1) +
    scale_color_manual(
        values = c(Won = "deepskyblue", Lost = "purple4"), 
        labels = c(Won = "Match won", Lost = "Match lost")) +
    scale_x_continuous(position = "top") +
    labs(
        title = paste0(player, "'s Professional Matches"),
        x = paste0(player, "'s age"),
        y = "Younger than opponent       Older than opponent",
        color = NULL, 
        caption = "Source: Tennis Abstract") +
    theme_minimal() +
    theme(
        legend.position = "top",
        plot.title = element_text(face = "bold"), 
        panel.grid.minor = element_blank())
}