testthat::test_that("SV_HU_CONTACT correction counts contacts within 365 days", {
  library(data.table)
  library(DBI)
  library(duckdb)
  
  # Create mock healthcare contact data
  pp_hu_contact_raw <- data.table(
    person_id = c("P001", "P001", "P001", "P002", "P002"),
    date = as.Date(c("2021-01-01", "2021-02-15", "2021-11-01", "2021-06-01", "2021-12-01")),
    value = c("PRIMARY_CARE", "HOSPITALIZATION", "PRIMARY_CARE", "PRIMARY_CARE", "FALSE")
  )
  
  # Create mock episodes data
  episodes <- data.table(
    person_id = c("P001", "P001", "P002", "P002"),
    start_episode = as.Date(c("2021-02-01", "2022-02-01", "2021-06-15", "2021-12-01"))
  )
  
  # Setup DuckDB connection
  db <- duckdb::duckdb()
  con <- DBI::dbConnect(db)
  
  tryCatch({
    # Register tables
    DBI::dbWriteTable(con, "episodes", episodes, overwrite = TRUE)
    DBI::dbWriteTable(con, "pp_hu_contact_raw", pp_hu_contact_raw, overwrite = TRUE)
    
    # Run the DuckDB query
    result <- DBI::dbGetQuery(con, "
      SELECT 
        e.person_id,
        e.start_episode,
        COALESCE(COUNT(p.person_id), 0) AS SV_HU_CONTACT
      FROM episodes e
      LEFT JOIN pp_hu_contact_raw p 
        ON e.person_id = p.person_id
        AND p.date <= e.start_episode
        AND p.date > e.start_episode - INTERVAL 365 DAY
        AND p.value IS NOT NULL 
        AND p.value != 'FALSE'
      GROUP BY e.person_id, e.start_episode
      ORDER BY e.person_id, e.start_episode
    ")
    
    result <- data.table::as.data.table(result)
    
    # Test 1: Check that valid contacts are counted
    # P001 on 2021-02-01 should have 1 contact (2021-01-01 is before start_episode, 2021-02-15 is after)
    p001_feb <- result[person_id == "P001" & start_episode == as.Date("2021-02-01"), SV_HU_CONTACT]
    testthat::expect_equal(p001_feb, 1L)
    
    # Test 2: Check that FALSE values are excluded
    # P002 on 2021-12-01 should have 1 contact (2021-06-01 is PRIMARY_CARE, 2021-12-01 is FALSE)
    p002_dec <- result[person_id == "P002" & start_episode == as.Date("2021-12-01"), SV_HU_CONTACT]
    testthat::expect_equal(p002_dec, 1L)
    
    # Test 3: Check 365-day window boundary
    # P001 on 2022-02-01 should have 2 contacts (2021-02-15 and 2021-11-01 are within 365 days)
    p001_next_year <- result[person_id == "P001" & start_episode == as.Date("2022-02-01"), SV_HU_CONTACT]
    testthat::expect_equal(p001_next_year, 2L)
    
    # Test 4: Check date boundary (exactly 365 days)
    # P002 on 2021-06-15 should have 1 contact (2021-06-01 is exactly 14 days before)
    p002_jun <- result[person_id == "P002" & start_episode == as.Date("2021-06-15"), SV_HU_CONTACT]
    testthat::expect_equal(p002_jun, 1L)
    
  }, finally = {
    DBI::dbDisconnect(con)
    duckdb::duckdb_shutdown(db)
  })
})


testthat::test_that("SV_HU_CONTACT correction handles empty results", {
  library(data.table)
  library(DBI)
  library(duckdb)
  
  # Create mock data with no matching contacts
  pp_hu_contact_raw <- data.table(
    person_id = character(0),
    date = as.Date(character(0)),
    value = character(0)
  )
  
  episodes <- data.table(
    person_id = c("P001", "P002"),
    start_episode = as.Date(c("2021-02-01", "2021-06-15"))
  )
  
  db <- duckdb::duckdb()
  con <- DBI::dbConnect(db)
  
  tryCatch({
    DBI::dbWriteTable(con, "episodes", episodes, overwrite = TRUE)
    DBI::dbWriteTable(con, "pp_hu_contact_raw", pp_hu_contact_raw, overwrite = TRUE)
    
    result <- DBI::dbGetQuery(con, "
      SELECT 
        e.person_id,
        e.start_episode,
        COALESCE(COUNT(p.person_id), 0) AS SV_HU_CONTACT
      FROM episodes e
      LEFT JOIN pp_hu_contact_raw p 
        ON e.person_id = p.person_id
        AND p.date <= e.start_episode
        AND p.date > e.start_episode - INTERVAL 365 DAY
        AND p.value IS NOT NULL 
        AND p.value != 'FALSE'
      GROUP BY e.person_id, e.start_episode
    ")
    
    result <- data.table::as.data.table(result)
    
    # Test: All episodes should have 0 contacts when no healthcare records exist
    testthat::expect_true(all(result$SV_HU_CONTACT == 0))
    testthat::expect_equal(nrow(result), 2L)
    
  }, finally = {
    DBI::dbDisconnect(con)
    duckdb::duckdb_shutdown(db)
  })
})


testthat::test_that("SV_HU_CONTACT correction merges correctly with episodes data", {
  library(data.table)
  library(DBI)
  library(duckdb)
  
  # Create mock d3_multivariate_episodes data
  d3_multivariate_episodes <- data.table(
    person_id = c("P001", "P001", "P002"),
    start_episode = as.Date(c("2021-02-01", "2022-02-01", "2021-06-15")),
    SV_HOSP_6WEEKS = c(TRUE, FALSE, TRUE),
    other_col = c("A", "B", "C")
  )
  
  pp_hu_contact_raw <- data.table(
    person_id = c("P001", "P001"),
    date = as.Date(c("2021-01-01", "2021-02-15")),
    value = c("PRIMARY_CARE", "HOSPITALIZATION")
  )
  
  db <- duckdb::duckdb()
  con <- DBI::dbConnect(db)
  
  tryCatch({
    # Extract unique episodes
    episodes <- d3_multivariate_episodes[, .(person_id, start_episode)] |> unique()
    
    DBI::dbWriteTable(con, "episodes", episodes, overwrite = TRUE)
    DBI::dbWriteTable(con, "pp_hu_contact_raw", pp_hu_contact_raw, overwrite = TRUE)
    
    hu_contact_summary <- DBI::dbGetQuery(con, "
      SELECT 
        e.person_id,
        e.start_episode,
        COALESCE(COUNT(p.person_id), 0) AS SV_HU_CONTACT
      FROM episodes e
      LEFT JOIN pp_hu_contact_raw p 
        ON e.person_id = p.person_id
        AND p.date <= e.start_episode
        AND p.date > e.start_episode - INTERVAL 365 DAY
        AND p.value IS NOT NULL 
        AND p.value != 'FALSE'
      GROUP BY e.person_id, e.start_episode
    ")
    
    hu_contact_summary <- data.table::as.data.table(hu_contact_summary)
    hu_contact_summary[, SV_HU_CONTACT := as.integer(SV_HU_CONTACT)]
    
    # Perform merge
    d3_episodes_updated <- merge(
      d3_multivariate_episodes,
      hu_contact_summary,
      by = c("person_id", "start_episode"),
      all.x = TRUE
    )
    
    # Test 1: Merge preserves all rows
    testthat::expect_equal(nrow(d3_episodes_updated), nrow(d3_multivariate_episodes))
    
    # Test 2: SV_HU_CONTACT column is present and correct type
    testthat::expect_true("SV_HU_CONTACT" %in% colnames(d3_episodes_updated))
    testthat::expect_true(is.integer(d3_episodes_updated$SV_HU_CONTACT))
    
    # Test 3: Original columns are preserved
    testthat::expect_true("other_col" %in% colnames(d3_episodes_updated))
    testthat::expect_equal(d3_episodes_updated[1, other_col], "A")
    
    # Test 4: Correct values are assigned
    p001_feb <- d3_episodes_updated[person_id == "P001" & start_episode == as.Date("2021-02-01"), SV_HU_CONTACT]
    testthat::expect_equal(p001_feb, 1L)
    
  }, finally = {
    DBI::dbDisconnect(con)
    duckdb::duckdb_shutdown(db)
  })
})


testthat::test_that("SV_HU_CONTACT correction excludes NULL and FALSE values", {
  library(data.table)
  library(DBI)
  library(duckdb)
  
  # Create healthcare contact data with various value states
  pp_hu_contact_raw <- data.table(
    person_id = c("P001", "P001", "P001", "P001", "P001"),
    date = as.Date(c("2021-01-01", "2021-01-10", "2021-01-20", "2021-01-25", "2021-02-01")),
    value = c("PRIMARY_CARE", "HOSPITALIZATION", "FALSE", NA, "PRIMARY_CARE")
  )
  
  episodes <- data.table(
    person_id = "P001",
    start_episode = as.Date("2021-02-15")
  )
  
  db <- duckdb::duckdb()
  con <- DBI::dbConnect(db)
  
  tryCatch({
    DBI::dbWriteTable(con, "episodes", episodes, overwrite = TRUE)
    DBI::dbWriteTable(con, "pp_hu_contact_raw", pp_hu_contact_raw, overwrite = TRUE)
    
    result <- DBI::dbGetQuery(con, "
      SELECT 
        e.person_id,
        e.start_episode,
        COALESCE(COUNT(p.person_id), 0) AS SV_HU_CONTACT
      FROM episodes e
      LEFT JOIN pp_hu_contact_raw p 
        ON e.person_id = p.person_id
        AND p.date <= e.start_episode
        AND p.date > e.start_episode - INTERVAL 365 DAY
        AND p.value IS NOT NULL 
        AND p.value != 'FALSE'
      GROUP BY e.person_id, e.start_episode
    ")
    
    result <- data.table::as.data.table(result)
    
    # Test: Should count only valid values (PRIMARY_CARE and HOSPITALIZATION)
    # Should exclude FALSE and NULL values
    # Expected: 3 contacts (2021-01-01, 2021-01-10, 2021-02-01)
    testthat::expect_equal(result$SV_HU_CONTACT, 3L)
    
  }, finally = {
    DBI::dbDisconnect(con)
    duckdb::duckdb_shutdown(db)
  })
})
