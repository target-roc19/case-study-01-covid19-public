#' @title Clean Vaccine Records
#'
#' @description This function removes vaccine records within a specified number of days from the last occurrence.
#' For example, for a person that had a vaccine on 1/1/2022, 2/1/2022 and 3/1/2022, you can remove the record with 2/1/2022 using
#' a washout period (rec.period) of 2 days.
#'
#' @author Roel Elbers, Albert Cid Royo (edited v281022)
#'
#' @param dataset A data.table dataframe
#' @param person_id A string with the column name where the person identifier is stored in dataset
#' @param rec_period A number or a vector of numbers representing the number of days within rows/vaccines are removed.
#' If a vector, the number of vaccines per person will be equal to the length of the vector.
#' If a number, the output file will give back the maximum number of vaccines per person.
#' @param c_date A string with the column name where the date of the vaccine is stored in dataset
#' @param concept An optional string representing the name of the vaccine of interest.
#' @param c_concept If concept is filled, A string with the column name where the name of the vaccine is stored in dataset
#' @param c_hierarchy column name with rank values. The numbers define the order of priority of each of the records
#'
#' @return A data.table data frame with the vaccines cleaned from vaccines within the specified rec.period or washout period.
#' @export

clean_vaccine_records <- function(
  dataset,
  person_id,
  rec_period,
  c_date,
  concept = NULL,
  c_concept = NULL,
  c_hierarchy = NULL,
  keep.searching = FALSE,
  unique_identifiers = c("ori_table", "unique_id")
) {
  library(data.table)
  # Copy over the data frame to prevent that the input dataset is changed outside the function
  dataset_temp <- data.table::copy(dataset)

  # Take only the concept of interest as specified in concept and c_concept
  if (!is.null(concept)) dataset_temp <- dataset_temp[get(c_concept) == concept, ]

  # Retrieve the column names the file where the table as appended to
  results <- data.table::copy(dataset_temp[0])[, iteration := as.integer()]
  cols <- colnames(results)

  # Set a variable to 1. This variable serves as a counter for the while loop.
  iteration <- 1
  # Make a boolean variable that serves a check to go into the next loop
  check <- TRUE

  # Execute the process. Start with the first concept per person_id and append them to the results file.
  # Per loop go to the next concept after removing rows too close to the previous event
  while (check) {
    # Distinct between 1 rec_period or differing rec_period per iteration
    if (length(rec_period) == 1) rec_period_temp <- rec_period
    if (length(rec_period) > 1 & iteration <= length(rec_period)) rec_period_temp <- rec_period[iteration]
    if (length(rec_period) > 1 & iteration > length(rec_period)) rec_period_temp <- rec_period[length(rec_period)]

    # Determine the difference to the first occurrence in days
    setorderv(dataset_temp, c("person_id", c_date))
    dataset_temp <- dataset_temp[, previous_date := data.table::shift(get(c_date)), by = c("person_id")]
    dataset_temp <- dataset_temp[, dif := get(c_date) - previous_date]
    dataset_temp <- dataset_temp[is.na(dif), dif := 0][, dif := as.numeric(dif)]
    dataset_temp <- dataset_temp[, cumdif := cumsum(dif), by = c(person_id)]

    # Collect the first record concept representing the first if in loop 1, the second record if in loop 2 and so on.
    dataset_temp2 <- dataset_temp[cumdif <= rec_period_temp, ]


    by_cols <- c("person_id", c_date)
    if (!is.null(c_hierarchy)) {
      by_cols <- c("person_id", c_date, c_hierarchy)
    }

    # Selecting first vaccination date
    date_hierarchy_dataset <- copy(dataset_temp2)
    setorderv(date_hierarchy_dataset, c("person_id", c_date))
    date_hierarchy_dataset <- date_hierarchy_dataset[, c("person_id", c_date), with = F]
    date_hierarchy_dataset <- date_hierarchy_dataset[, .SD[1], by = c("person_id")]

    if (!is.null(c_hierarchy)) { # For pharmo
      key_cols <- c("person_id", c_hierarchy, c_date, unique_identifiers)
      data.table::setorderv(dataset_temp2, key_cols, rep(1, length(key_cols)))
    } else {
      key_cols <- c(by_cols, unique_identifiers)
      data.table::setorderv(dataset_temp2, key_cols, rep(1, length(key_cols)))
    }
    dataset_temp2 <- dataset_temp2[
      , .SD[which.max(!is.na(vx_manufacturer))], # Select the first non-NA row or first row if all are NA
      by = "person_id"
    ]
    dataset_temp2 <- dataset_temp2[, iteration := iteration][, cols, with = FALSE][, eval(c_date) := NULL]

    dataset_temp2 <- merge(date_hierarchy_dataset, dataset_temp2, by = "person_id", all.x = TRUE)
    rm(date_hierarchy_dataset)


    # Add the collected concepts to the main file. Also, the iteration is stored representing the following number of the event.
    results <- rbindlist(list(results, dataset_temp2), fill = FALSE, use.names = TRUE)
    rm(dataset_temp2)

    # Remove the rows that were within the rec_period - in other words, keep the rows that were outisde the rec_period
    dataset_temp <- dataset_temp[cumdif > rec_period_temp, ]

    # Remove the temporary columns that will be recreated the next round
    lapply(c("dif", "cumdif", "previous_date"), function(x) {
      dataset_temp <- dataset_temp[, eval(x) := NULL]
    })
    print(paste0("Cycle ", iteration))
    print(paste0("Window length ", rec_period_temp))
    iteration <- iteration + 1
    gc()

    if (length(rec_period) == 1) check <- nrow(dataset_temp) > 0
    if (length(rec_period) > 1 & keep.searching == FALSE) check <- iteration <= length(rec_period)
    if (length(rec_period) > 1 & keep.searching == TRUE) check <- nrow(dataset_temp) > 0
  }

  rm(dataset_temp, iteration)
  gc()

  if (is.null(c_hierarchy)) {
    setorderv(results, c(person_id, c_date))
  } else {
    setorderv(results, c(person_id, c_date, c_hierarchy))
  }

  return(results)
}
