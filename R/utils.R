#' Clean MAP data so it is compatable with other functions
#'
#' @param map Dataframe of MAP data provided by NWEA. Looks like
#' dataframe in sample_map_data.rda
#'
#' @return returns a cleaned dataframe
#' @export

clean_map <- function(
    map
) {
    # change date to POSIXct
    map$teststartdate <- as.POSIXct(as.character(map$teststartdate), tz = 'EST')


}
