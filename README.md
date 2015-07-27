# khanELO
An R package to update Khan item difficulties and to estimate student RIT scores using the ELO Rating System.

This package was create to make my work generalizable and actionable. The package allows the user to take data from Khan Academy along with MAP data provided by the NWEA to ultimately predict a studet's RIT score.

## Workflow

This is the process to create your own item difficulty estimates and student RIT score predictions. The 'tree' estimates will be used as priors, which is recommended for more coverage. 

First, load in the sample data from the package.* Then, create the list of dataframes that are used to update item difficulties and make RIT score predictions.

```r
data(sample_map_data)
data(simulated_khanmath)
data(simulated_khanstate)

activity <- khanELO::generate_activity(
    map = sample_map_data,
    khan_exers = sim_khanmath,
    khan_states = sim_khanstate,
    colnames = c('studentid', 'teststartdate', 'testritscore'),
    numeric_sid = FALSE,
    estimate_type = 'tree',
    verbose = TRUE
)
```
If you want to see a visualization of a student's Khan Academy actvity as well as his / her MAP test history, you can use the plot_activity function.

```r
khanELO::plot_activity(activity[['F08000002']])
```

Next, update the item difficulties. Save the new estiamtes to a file you can update or so you can load them into a database.

```r
khanELO::update_diffs(
    student_activity = activity,
    priors = 'tree',
    priorpath = NA,
    filepath = '~/myestimates/inhouse_estimates.csv',
    numeric_sid = FALSE,
    plot = FALSE,
    verbose = TRUE
)
```

This process takes a long time, but it should only be done before each MAP test so you can make the best predictions for students. 

Next, if you want to use these in house estimates to predict a student's RIT score, you must generate the list of dataframes using generate_activity() again, setting estimate_type to 'inhouse' and providing the file path via the inhouse_path parameter. If you want to use these estimates as the priors for the next time you update the item difficulties, just provide the filepath to the priorpath parameter and set the priors parameter to 'inhouse' when using the update_diffs() function.

```r
activity <- khanELO::generate_activity(
    map = sample_map_data,
    khan_exers = sim_khanmath,
    khan_states = sim_khanstate,
    colnames = c('studentid', 'teststartdate', 'testritscore'),
    numeric_sid = FALSE,
    estimate_type = 'inhouse',
    inhouse_path = '~/myestimates/inhouse_estimates.csv',
    verbose = TRUE
)
```

Next, using the updated activity list, you can make predictions for a chosen set of students. 

```r
predicted_rit <- khanELO::update_proficiencies(
    studentids = names(activity)[1:20],
    student_activity = activity,
    start_date = '2014-05-21',
    end_date = '2014-12-10'
)
```

In practice, the end_date should not need to be included if the predictions are made close to, but not after the MAP test you are preparing for. The start date should be a date to capture the testing data for the most recent MAP test. It was included in the code above to work with the sample data.

*When using the sample data provided, the activity plots, updated difficulties, and estimated RIT scores should not be used to validate the study. These datasets were simulated for privacy and do not represent actual Khan Academy activity. The sample_map_data dataset is a sample provided by the NWEA.

