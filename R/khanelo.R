#' Clean and merge MAP, Khan, and item difficulty data.
#'
#' @param map dataframe containing student MAP test data provided by NWEA. Looks
#' like dataframe found in sample_map_data.rda
#' @param khan_exers dataframe containing student data for Khan Academy excises.
#' Looks like dataframe found in simulated_khanmath.rda
#' @param khan_states dataframe containing student state change data for Khan
#' Academy exercises. Looks like dataframe found in simulated_khanstate.rda
#' @param estimate_type string to indicated which type, NWEA or tree, of Khan
#' item estimate to use. Defaults to tree (more coverage).
#'
#' @return Returns list of dataframes. Each dataframe will contain a student's
#' Khan Activity as well as his / her MAP test history.
#' @export

khanelo <- function(
    map,
    khan_exers,
    khan_states,
    estimate_type = 'tree'
) {

}

