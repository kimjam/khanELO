#' Clean and merge MAP, Khan, and item difficulty data.
#'
#' @param map Dataframe containing student MAP test data provided by NWEA. Looks
#' like dataframe found in data(sample_map_data)
#' @param khan_exers Dataframe containing student data for Khan Academy excises.
#' Looks like dataframe found in data(simulated_khanmath)
#' @param khan_states Dataframe containing student state change data for Khan
#' Academy exercises. Looks like dataframe found in data(simulated_khanstate)
#' @param estimate_type String to indicate which type (NWEA, tree, inhouse)
#' of Khan item estimate to use. Defaults to tree (more coverage than nwea). If
#' 'inhouse' is chosen, user must provide filepath to csv of inhouse estimates
#' containing exercise title and estimated difficulty. Can be created using
#' update_diff() function.
#' @param numeric_sid Conditional indicating whether studentid is numeric or
#' not. Default is TRUE
#' @param verbose Default is TRUE to print status updates.
#'
#' @return Returns list of dataframes. Each dataframe will contain a student's
#' Khan Activity as well as his / her MAP test history.
#' @export

generate_activity <- function(
    map,
    khan_exers,
    khan_states,
    numeric_sid = TRUE,
    estimate_type = 'tree',
    inhouse_est = NA,
    verbose = TRUE
) {

    if (verbose) print('Cleaning map data...')
    map <- clean_map(mapdata = map, numeric_sid)
    if (verbose) print('Done.')

    if (verbose) print('Cleaning Khan exercise data...')
    khan_exers <- clean_khanmath(khanmath = khan_exers, numeric_sid)
    if (verbose) print('Done.')

    if (verbose) print('Cleaning Khan state change data...')
    khan_states <- clean_states(khanstates = khan_states, numeric_sid)
    if (verbose) print('Done.')

    if (verbose) print('Reshaping Khan state change data...')
    states_wide <- reshape_states(states_long = khan_states)
    if (verbose) print('Done.')

    if (verbose) print(paste('Joining with', estimate_type, 'estimates...'))
    if (estimate_type == 'tree') {

        states_wide <- dplyr::left_join(
            x = states_wide,
            y = tree_estimates,
            by = c('exercise' = 'slug')
        )

    } else if (estimate_type == 'nwea') {

        nwea_estimates$rit_estimate <- 0

        states_wide <- dplyr::left_join(
            x = states_wide,
            y = nwea_estimates,
            by = c('exercise' = 'slug')
        )

    } else if (estimate_type == 'inhouse') {

        if (is.na(inhouse_est)) {
            stop('Must provide csv of in house estimates if
                 estimate_type = inhouse'
            )
        } else {
            inhouse <- read.csv(inhouse_est, stringsAsFactors = FALSE)
            names(inhouse) <- c('slug', 'rit_estimate')
        }

        states_wide <- dplyr::left_join(
            x = states_wide,
            y = inhouse,
            by = c('exercise' = 'slug')
        )
    }

    if (verbose) print('Done.')

    if (length(unique(map$studentid)) < length(unique(states_wide$studentid))) {
        mask <- unique(map$studentid %in% unique(states_wide$studentid))
        students <- unique(map$studentid)[mask]
    } else {
        mask <- unique(states_wide$studentid) %in% unique(map$studentid)
        students <- unique(states_wide$studentid)[mask]
    }

    map <- map %>%
        dplyr::filter(
            studentid %in% students
        )

    states_wide <- states_wide %>%
        dplyr::filter(
            studentid %in% students
        )

    if (verbose) print('Adding columns to map and parsing Khan states...')
    stu_map <- populate_map(map)
    stu_states <- parse_wide_states(states_wide)
    if (verbose) print('Done.')

    if (verbose) print('Creating list of dataframes...')
    activity <- vector('list', length(students))
    for (i in 1:length(students)) {

        stu_states[[i]] <- stu_states[[i]] %>%
            dplyr::filter(
                !is.na(date)
            )
        date_range <- c(
            min(stu_states[[i]]$date) - lubridate::ddays(30),
            max(stu_states[[i]]$date) + lubridate::ddays(30)
        )

        stu_map[[i]] <- stu_map[[i]] %>%
            dplyr::filter(
                date >= date_range[1],
                date <= date_range[2]
            )

        activity[[i]] <- rbind(stu_map[[i]], stu_states[[i]])

        activity[[i]] <- activity[[i]][order(activity[[i]]$date), ]
        activity[[i]] <- add_subset(activity[[i]])

    }

    names(activity) <- students
    if (verbose) print('Done.')

    return(activity)

}

