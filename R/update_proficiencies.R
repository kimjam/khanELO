#' Uses the ELO Rating system to estimate a student's current proficiecy level
#' on the RIT scale using his / her Khan activity.
#' @param studentids the studentids of the student proficiencies you want
#' to estimate
#' @param student_activity list of dataframes returned by generate_activity()
#' @param start_date date of MAP test used as initial estimate
#' @param end_date date to project until. Not required unless start_date is
#' before most recent MAP test event
#'
#' @return returns a dataframe containing studentids and estimated proficiency
#' @export

update_proficiencies <- function(
    studentids,
    student_activity,
    start_date,
    end_date = NA
) {

    stu_activity <- student_activity[studentids]

    start_time <- as.POSIXct(start_date, tz = 'EST')

    if (is.na(end_date)) {
        end_date <- Sys.time()
    } else {
        end_date <- as.POSIXct(end_date, tz = 'EST')
    }

    stu_activity <- lapply(
        stu_activity,
        function(x) x %>%
            dplyr::filter(
                date >= start_date,
                date <= end_date
            ) %>%
            dplyr::mutate(
                rit = (rit - 200) / 10
            )
    )

    map_data <- lapply(
        stu_activity,
        function(x) x %>%
            dplyr::filter(
                exercise == 'maptest'
            ) %>%
            head(n = 1)
    )

    stu_activity <- lapply(
        stu_activity,
        function(x) x %>%
            dplyr::filter(
                exercise != 'maptest'
            )
    )

    estimated_profs <- vector(length = length(stu_activity))
    last_updated <- vector(length = length(stu_activity))
    for (i in 1:length(stu_activity)) {
        exers <- stu_activity[[i]] %>%
            dplyr::group_by(
                exercise
            ) %>%
            dplyr::summarise(
                last_status = last(exercise_status),
                last_date = last(date),
                rit = last(rit)
            ) %>%
            dplyr::mutate(
                mastered_dummy = ifelse(last_status == 'mastery3', 1, 0)
            ) %>%
            dplyr::filter(
                !is.na(rit)
            ) %>%
            dplyr::arrange(
                last_date
            )

        rit <- map_data[[1]]$rit
        if (nrow(exers) != 0) {
            engagement <- sum(exers$mastered_dummy) / nrow(exers)
            if (engagement > .8) {
                for (n in 1:nrow(exers)) {
                    spread <- abs(rit - exers$rit[n])
                    if (spread > .2) {
                        w = .2
                    } else {
                        w = .05
                    }
                    rit <- rit + w * (exers$mastered_dummy[n] -
                                          (exp(rit - exers$rit[n])) /
                                          (1 + exp(rit - exers$rit[n]))
                    )
                }
                estimated_profs[i] <- rit * 10 + 200
                last_updated[i] <- max(exers$date, na.rm = TRUE)
            } else {
                estimated_profs[i] <- NA
            }
        } else {
            estimated_profs[i] <- NA
        }
    }

    est_profs <- data.frame(
        studentid = names(stu_activity),
        estimated_proficiency = estimated_profs,
        last_updated = last_updated
    )

    return(est_profs)

}
