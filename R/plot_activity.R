#' Creates plot showing MAP test scores and khan activity.
#'
#' @param stu_activity_df a dataframe created by generate_activity()
#'
#' @return prints a plot
#' @export

plot_activity <- function(
    stu_activity_df
) {

    stu_map <- stu_activity_df %>%
        dplyr::filter(
            exercise == 'maptest'
        )

    sid <- stu_map$studentid[1]

    p <- ggplot(
        data = stu_activity_df
    ) +
        geom_point(
            aes(
                x = date,
                y = rit,
                color = exercise_status
            )
        ) +
        geom_point(data = stu_map,
                   aes(
                       x = date,
                       y = rit
                   ),
                   color = 'black',
                   shape = 1,
                   size = 7
        ) +
        ggtitle(
            paste(
                'RIT Score / Khan Difficulty vs. Date :',
                sid
            )
        ) +
        xlab('Date') +
        ylab('RIT Score / Difficulty')

    print(p)

}

