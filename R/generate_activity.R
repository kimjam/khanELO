#' Clean and merge MAP, Khan, and item difficulty data.
#'
#' @param map Dataframe containing student MAP test data provided by NWEA. Looks
#' like dataframe found in data(sample_map_data)
#' @param khan_exers Dataframe containing student data for Khan Academy excises.
#' Looks like dataframe found in data(simulated_khanmath)
#' @param khan_states Dataframe containing student state change data for Khan
#' Academy exercises. Looks like dataframe found in data(simulated_khanstate)
#' @param colnames vector of map colnames that contain studentid, rit score, and
#' test date. Default is c('studentid', 'testritscore', 'teststartdate')
#' @param estimate_type String to indicate which type (NWEA, tree, inhouse)
#' of Khan item estimate to use. Defaults to tree (more coverage than nwea). If
#' 'inhouse' is chosen, user must provide filepath to csv of inhouse estimates
#' containing exercise title and estimated difficulty. Can be created using
#' update_diff() function.
#' @param numeric_sid Conditional indicating whether studentid is numeric or
#' not. Default is TRUE
#' @param inhouse_path filepath indicating where inhouse estimates are kept
#' @param verbose Default is TRUE to print status updates.
#'
#' @return Returns list of dataframes. Each dataframe will contain a student's
#' Khan Activity as well as his / her MAP test history.
#' @export

generate_activity <- function(
    map,
    khan_exers,
    khan_states,
    colnames = c('studentid', 'testritscore', 'teststartdate'),
    numeric_sid = TRUE,
    estimate_type = 'tree',
    inhouse_path = NA,
    verbose = TRUE
) {

    if (colnames[1] != 'studentid' & 'studentid' %in% names(map)) {
        names(map)[match('studentid', names(map))] <- 'unused_sid'
        names(map)[match(colnames[-1], names(map))] <- c(
            'testritscore',
            'teststartdate'
        )
    }

    names(map)[match(colnames, names(map))] <- c(
        'studentid',
        'testritscore',
        'teststartdate'
    )

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

        if (is.na(inhouse_path)) {
            stop('Must provide csv of in house estimates if
                 estimate_type = inhouse'
            )
        } else {
            inhouse <- read.csv(inhouse_path, stringsAsFactors = FALSE)
            names(inhouse)[1:2] <- c('slug', 'rit_estimate')
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

        min_date <- min(stu_states[[i]]$date, na.rm = TRUE)
        diff <- min_date - stu_map[[i]]$date
        min_ind <- match(min(diff[diff >= 0], na.rm = TRUE), diff)
        date_range <- c(
            stu_map[[i]]$date[min_ind],
            max(stu_states[[i]]$date, na.rm = TRUE) + lubridate::ddays(60)
        )

        stu_map[[i]] <- stu_map[[i]] %>%
            dplyr::filter(
                date >= date_range[1],
                date <= date_range[2]
            )

        if (nrow(stu_states[[i]]) == 0 | nrow(stu_map[[i]]) == 0) {
            activity[[i]] <- NA
        } else {

            activity[[i]] <- rbind(stu_map[[i]], stu_states[[i]])

            activity[[i]] <- activity[[i]][order(activity[[i]]$date), ]
            activity[[i]] <- add_subset(activity[[i]])
        }

    }

    names(activity) <- students

    mask <- sapply(activity, function(x) 'data.frame' %in% class(x))
    activity <- activity[mask]
    if (verbose) print('Done.')

    return(activity)

}

