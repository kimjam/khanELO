#' Clean MAP data so it can be joined with Khan exercise and state change data.
#'
#' @param map Dataframe of MAP data provided by NWEA. Looks like
#' dataframe in data(sample_map_data)
#' @param numeric_sid conditional indicating whether studentid is numeric or
#' not. default is TRUE
#'
#' @return returns a cleaned dataframe
#' @export

clean_map <- function(
    mapdata,
    numeric_sid = TRUE
) {

    cleaned <- mapdata %>%
        dplyr::filter(measurementscale == 'Mathematics')

    if (numeric_sid) {
        cleaned <- cleaned %>%
            dplyr::select(
                studentid,
                teststartdate,
                testritscore
            ) %>%
            dplyr::mutate(
                studentid = as.numeric(
                    as.character(
                        studentid
                        )
                    ),
                teststartdate = as.POSIXct(
                    as.character(
                        teststartdate
                        ),
                    tz = 'EST'
                    ),
                testritscore = as.numeric(
                    as.character(
                        testritscore
                        )
                )
            ) %>%
            dplyr::arrange(
                studentid,
                teststartdate
            )
    } else {
        cleaned <- cleaned %>%
            dplyr::select(
                studentid,
                teststartdate,
                testritscore
            ) %>%
            dplyr::mutate(
                teststartdate = as.POSIXct(
                    as.character(
                        teststartdate
                    ),
                    tz = 'EST'
                ),
                testritscore = as.numeric(
                    as.character(
                        testritscore
                        )
                )
            ) %>%
            dplyr::arrange(
                studentid,
                teststartdate
            )
    }

    return(cleaned)
}

#' Clean Khan exercise data so it can be joined with state change and MAP data.
#'
#' @param khanmath dataframe containing composite exercise data for students.
#' should look like dataframe in data(simulated_khanmath)
#' @param numeric_sid conditional indicating whether studentid is numeric or
#' not. default is TRUE
#'
#' @return returns a cleaned dataframe
#' @export

clean_khanmath <- function(
    khanmath,
    numeric_sid = TRUE
) {

    # change studentid to numeric if numeric_sid = TRUE
    if (numeric_sid) {
        khanmath$studentid <- as.numeric(as.character(khanmath$studentid))
    }

    # convert grade_level to numeric
    khanmath$grade_level <- as.numeric(as.character(khanmath$grade_level))

    # convert toal_done to numeric
    mask <- c(1:nrow(khanmath))[khanmath$total_done == 'NULL']
    khanmath$total_done[mask] <- 0
    khanmath$total_done <- as.numeric(
        as.character(
            khanmath$total_done
            )
        )

    # convert total_correct to numeric
    mask <- c(1:nrow(khanmath))[khanmath$total_correct == 'NULL']
    khanmath$total_correct[mask] <- 0
    khanmath$total_correct <- as.numeric(
        as.character(
            khanmath$total_correct
            )
        )

    # convert streak to numeric
    mask <- c(1:nrow(khanmath))[khanmath$streak == 'NULL']
    khanmath$streak[mask] <- 0
    khanmath$streak <- as.numeric(
        as.character(
            khanmath$streak
            )
        )

    # convert mastered_dummy to numeric
    khanmath$mastered_dummy <- ifelse(khanmath$exercise_status == 'mastery3',
                                      1,
                                      0
    )

    return(khanmath)
}

#' Cleans Khan state change data so it can be joined with exercise and map data.
#'
#' @param khanstates dataframe containing state change data for students. should
#' look like dataframe in data(simulated_khanstate.rda)
#' @param numeric_sid conditional indicating whether studentid is numeric or
#' not. default is TRUE
#'
#' @return returns a cleaned dataframe
#' @export

clean_states <- function(
    khanstates,
    numeric_sid = TRUE
) {

    # convert studentid to numeric if numeric_sid = TRUE
    if (numeric_sid) {
        khanstates$studentid <- as.numeric(
            as.character(
                khanstates$studentid
            )
        )
    }

    return(khanstates)

}

#' Reshapes Khan state data that has been cleaned into wide format to generate
#' Khan Activity for each student
#'
#' @param states_long dataframe containing cleaned Khan state change data
#'
#' @return returns a reshaped dataframe
#' @export

reshape_states <- function(
    states_long
) {

    states_long <- states_long %>%
        dplyr::filter(
            exercise_status != 'unstarted'
        )

    states_wide <- dcast(states_long,
                         exercise + studentid ~ exercise_status,
                         value.var = 'date',
                         fun.aggregate = first
    )

    clean_dates <- function(dates) {
        dates <- gsub('T', ' ', dates)
        dates <- gsub('Z', '', dates)

        return(dates)
    }
    states_wide$mastery1 <- clean_dates(states_wide$mastery1)
    states_wide$mastery2 <- clean_dates(states_wide$mastery2)
    states_wide$mastery3 <- clean_dates(states_wide$mastery3)
    states_wide$practiced <- clean_dates(states_wide$practiced)

    states_wide$mastery1 <- as.POSIXct(states_wide$mastery1,
                                       format = '%Y-%m-%d %H:%M:%S',
                                       tz = 'EST'
    )
    states_wide$mastery2 <- as.POSIXct(states_wide$mastery2,
                                       format = '%Y-%m-%d %H:%M:%S',
                                       tz = 'EST'
    )
    states_wide$mastery3 <- as.POSIXct(states_wide$mastery3,
                                       format = '%Y-%m-%d %H:%M:%S',
                                       tz = 'EST'
    )
    states_wide$practiced <- as.POSIXct(states_wide$practiced,
                                        format = '%Y-%m-%d %H:%M:%S',
                                        tz = 'EST'
    )

    states_wide$mastered_dummy <- ifelse(
        is.na(states_wide$mastery3),
        0,
        1
    )

    states_wide$timetomaster <- as.numeric(
        states_wide$mastery3 - states_wide$practiced,
        units = 'mins'
    )

    mask <- is.na(states_wide$timetomaster)
    states_wide$timetomaster[mask] <- as.numeric(
        states_wide$mastery3[mask] - states_wide$mastery1[mask],
        units = 'mins'
    )

    mask <- is.na(states_wide$timetomaster)
    states_wide$timetomaster[mask] <- as.numeric(
        states_wide$mastery3[mask] - states_wide$mastery2[mask],
        units = 'mins'
    )

    states_wide <- states_wide %>%
        dplyr::arrange(
            studentid,
            exercise
        )

    return(states_wide)

}

#' Take Khan state change data in wide format and turn it into student
#' activty dataframe.
#'
#' @param states_wide Dataframe of Khan state change data in wide format
#' generated by reshape_states()
#'
#' @return a parsed dataframe
#' @export

parse_wide_states <- function (
    states_wide
) {
    stu_split <- split(states_wide, states_wide$studentid)

    split_states <- vector('list', length(stu_split))
    for (i in 1:length(stu_split)) {

        student <- stu_split[[i]]

        student <- student[!duplicated(student$exercise), ]
        student <- melt(student,
                        id.vars = c('exercise', 'rit_estimate', 'studentid'),
                        measure.vars = c('mastery1', 'mastery2', 'mastery3'),
                        variable.name = 'exercise_status',
                        value.name = 'date'
        ) %>%
            dplyr::rename(
                rit = rit_estimate
            ) %>%
            dplyr::select(
                studentid,
                exercise,
                exercise_status,
                date,
                rit
            )

        split_states[[i]] <- student
    }

    return(split_states)
}

#' Add needed columns to student's map data to concatenate with state change
#' data
#'
#' @param student_map dataframe containing studentid, teststartdate, and
#' testritscore
#'
#' @return returns list of dataframes
#' @export

populate_map <- function(
    student_map
) {
    split_students <- split(student_map, student_map$studentid)

    populated <- vector('list', length(split_students))
    for (i in 1:length(split_students)) {

        populated[[i]] <- split_students[[i]]
        names(populated[[i]]) <- c('studentid', 'date', 'rit')
        populated[[i]]$exercise <- 'maptest'
        populated[[i]]$exercise_status <- 'map'

        cols <- c('studentid', 'exercise', 'exercise_status', 'date', 'rit')
        populated[[i]] <- populated[[i]][cols]

    }

    return(populated)
}

#' Adds subset column to student activity dataframes
#'
#' @param student_df A dataframe containing a student's map and khan activity
#'
#' @return returns dataframe with subset column
#' @export

add_subset <- function(
    student_df
) {

    map_inds <- c(1:nrow(student_df))[student_df$exercise == 'maptest']
    student_df$subset <- 0

    if (length(map_inds) < 2) {
        student_df$subset[student_df$exercise != 'maptest'] <- 1
    } else {

        for (i in 1:(length(map_inds) - 1)) {
            student_df$subset[(map_inds[i] + 1):(map_inds[i + 1] - 1)] <- i
        }

    }

    return(student_df)
}

#' Get an item's mastery history
#'
#'@param student_activity A list of dataframes created by generate_activty()
#'@param item_title name of Khan exercise to get history for
#'
#'@return returns a dataframe containing mastery history and rit estimate
#'@export

get_item_history <- function(
    student_activity,
    item_title
) {

    student_map <- lapply(
        student_activity,
        function(x) x %>%
            dplyr::filter(
                exercise == 'maptest'
            )
    )

    count <- 1
    for (i in 1:length(student_activity)) {

        activity <- student_activity[[i]] %>%
            dplyr::filter(
                exercise == item_title
            ) %>%
            dplyr::arrange(
                exercise_status
            )

        rit_score <- vector(length = length(student_activity))
        mastered_dummy <- vector(length = length(student_activity))
        if (nrow(activity) == 0) {
            next
        } else {
            subset <- tail(activity$subset, n = 1)
            if (subset == 0) {
                rit_score[count] <- tail(student_map[[i]]$rit, n = 1)
            } else {
                rit_score[count] <- student_map[[i]]$rit[subset]
            }
            if (tail(activity$exercise_status, n = 1) == 'mastery3') {

                mastered_dummy[count] <- 0

            } else {

                mastered_dummy[count] <- 1

            }
        }
        count <- count + 1

    }

    history <- data.frame(
        rit_score = rit_score[1:(count - 1)],
        mastered_dummy = mastered_dummy[1:(count - 1)]
    )

    return(history)
}

