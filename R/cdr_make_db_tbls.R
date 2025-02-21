cdr_make_db_tbls <- function(conn_pool, db_tbl, key_field = NULL, schema = "public") {
  cat("\n--Running: crudr::cdr_make_db_tbls()\n")

  db_tbl_name <- rlang::as_name(rlang::enquo(db_tbl))
  db_tbl <- tibble::tibble(db_tbl)

  if (pool::dbExistsTable(conn = conn_pool, name = DBI::Id(schema = schema, table = db_tbl_name))) {
    cat(glue::glue("\n\nDid not create new table for '{db_tbl_name}'. Table already exists in the database.\n\n"))
  } else {
    cat(glue::glue("\nTable to convert '{db_tbl_name}' has format:\n\n"))
    print(db_tbl)
    cat(glue::glue("\nCreating and populating table '{db_tbl_name}'\n\n"))

    if (is.null(key_field)) {
      message("\nNo key Unique Key field specified.")
      db_tbl <- cdr_make_unique_ids(db_tbl)
    } else if (!key_field %in% names(db_tbl)) {
      message(glue::glue("\nSpecified field '{key_field}' does not exist."))
      db_tbl <- cdr_make_unique_ids(db_tbl)
    } else if (anyDuplicated(db_tbl[[key_field]])) {
      message(glue::glue("\nKey values in field '{key_field}' are not unique."))
      db_tbl <- cdr_make_unique_ids(db_tbl)
    } else if (!is.character(db_tbl[[key_field]])) {
      message(glue::glue("\nKey field '{key_field}' is not a varchar. Casting Key_field as a character vector."))
      db_tbl[[key_field]] <- as.character(db_tbl[[key_field]])
    }

    cat(glue::glue("\nCreating database table '{db_tbl_name}'.\n\n"))
    pool::dbCreateTable(conn = conn_pool, name = DBI::Id(schema = schema, table = db_tbl_name), fields = db_tbl)
    cdr_append_tbl(conn_pool, db_tbl, db_tbl_name, schema = schema)
  }

  chg_log_tbl_name <- cdr_name_delta_tbl(db_tbl_name)

  if (pool::dbExistsTable(conn = conn_pool, name = DBI::Id(schema = schema, table = chg_log_tbl_name))) {
    cat(glue::glue("\nDid not create new table for '{chg_log_tbl_name}'. Table already exists in the database.\n\n"))
  } else {
    cat(glue::glue("\nCreating new change tracking table '{chg_log_tbl_name}' from:\n\n"))
    delta_tbl <- tibble::tibble(OBS_ID = character(), FIELD = character(), CHG_FROM = character(), CHG_TO = character(), WHO_EDITED = character(), WHEN_EDITED = as.POSIXct(NA))
    pool::dbCreateTable(conn_pool, DBI::Id(schema = schema, table = chg_log_tbl_name), delta_tbl)
    print(delta_tbl)
  }
}
