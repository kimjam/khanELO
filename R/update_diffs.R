#' Uses student activity data to update item difficulties.
#'
#' @param student_activity List of dataframes returned by generate_activity()
#' @param priors A string (nwea, tree, inhouse), to indicate which estimates to
#' use as priors. Defaults to tree.
#' @param priorpath If inhouse priors are used, provide path to csv containing
#' these estimates.
#' @param filepath a path and filename to write the updated difficulties to.
#' ex: '/mypath/my_estimates.csv'
#' @param numeric_sid Conditional indicating whether studentid is numeric or
#' not. Default is TRUE
#' @param plot TRUE if you want to print a plot showing change in difficulty.
#' Default is FALSE.
#' @param verbose Default is TRUE to print status updates.
#'
#' @return returns dataframe of updated difficulties or writes to a csv if
#' filepath is not NA
#' @export


update_diffs <- function(
    student_activity,
    priors = 'tree',
    priorpath = NA,
    filepath = NA,
    numeric_sid = TRUE,
    plot = FALSE,
    verbose = TRUE
) {

    if (verbose) print('Getting priors...')

    if (priors == 'nwea') {
        item_diffs <- nwea_estimates
    } else if (priors == 'tree') {
        item_diffs <- tree_estimates
    } else if (priors == 'inhouse') {
        if (!is.na(priorpath)) {
            item_diffs <- read.csv(priorpath, stringsAsFactors = FALSE)
        } else {
            stop(
                'Provide file path to in house estimates.
                Ex: /mypath/my_estimates.csv'
            )
        }
    } else {
        stop('priors must be the string nwea, tree, or inhouse')
    }

    if (verbose) print('Done.')
    names(item_diffs) <- c('slug', 'rit_estimate')

    items <- item_diffs$slug
    item_diffs$rit_estimate <- (item_diffs$rit_estimate - 200) / 10
    item_diffs$updated_estimate <- 0

    if (verbose) print(paste('Updating', length(items), 'item difficulties...'))
    for (item in items) {

        item_history <- get_item_history(student_activity, item_title = item)

        item_diff <- item_diffs$rit_estimate[match(item, items)]

        if (verbose) {
            print(paste0('Updating item ',
                        match(item, items),
                        ' of ',
                        length(items),
                        '...')
            )
        }
        if (nrow(item_history) == 0) {

            item_diffs$updated_estimate[match(item, item_diffs$slug)] <- item_diff * 10 + 200

        } else {

            item_history$rit_score <- (item_history$rit_score - 200) / 10
            if (nrow(item_history) > 100) {
                W <- .02
            } else if (nrow(item_history) > 50) {
                W <- .04
            } else {
                W <- .2
            }

            for (i in 1:nrow(item_history)) {

                item_diff <- item_diff +
                    W *
                    (item_history$mastered_dummy[i] -
                         (exp(item_diff - item_history$rit_score[i]) /
                              (1 + exp(item_diff - item_history$rit_score[i]))
                         )
                    )
            }

            item_diff <- item_diff * 10 + 200
            item_diffs$updated_estimate[match(item, item_diffs$slug)] <- item_diff
        }
        if(verbose) print('Done.')
    }

    item_diffs$rit_estimate <- item_diffs$rit_estimate * 10 + 200
    if (plot) {
        p <- ggplot(
            item_diffs,
            aes(
                x = updated_estimate,
                y = rit_estimate
            )
        ) +
            geom_point() +
            geom_abline(
                slope = 1,
                intercept = 0,
                color = 'red'
            ) +
            xlim(
                150,
                250
            ) +
            ylim(
                150,
                250
            ) +
            ggtitle('Prior vs. Updated Item Difficulties') +
            xlab('Updated Estimate') +
            ylab('Prior Estimate')

        print(p)
    }

    updated_diffs <- item_diffs %>%
        dplyr::select(
            slug,
            updated_estimate
        ) %>%
        dplyr::rename(
            rit_estimate = updated_estimate
        )

    if (!is.na(filepath)) {
        write.csv(updated_diffs, file = filepath, row.names = FALSE)
    } else {
        return(updated_diffs)
    }
}
