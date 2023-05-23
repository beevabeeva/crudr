#' adjust for timezone treatment between databases
#'
#' @param conn_pool database connection of type pool
#'
#' @return 'UTC' or empty string
#' @export
#'
#' @examples \dontrun{ cdr_adj_timezone(conn_pool) }
#'
cdr_adj_timezone <- function(conn_pool){

  #Handle S4 connection object
  # NOTE: just ham-fisting this in like this for now. It is terrible, redundant etc.
  # But want to see what eauleaf will say/how they'd do it.
  if (isS4(conn_pool)) {
    pg_tz <- paste(pool::dbGetQuery(conn_pool, "SELECT current_setting('TIMEZONE')"))


    # can't really get a fool-proof way to do this with S4 at the moment.
    # dplyr::case_when(
    # stringr::str_detect(conn_pool@timezone,"(?i)Snowflake") ~ 'UTC',
    # stringr::str_detect(conn_pool@timezone,"(?i)sqlite") ~ '',
    # stringr::str_detect(conn_pool@timezone,"(?i)postgres") ~ pg_tz,
    # T ~ Sys.timezone()
    # )

  } else{
      if(stringr::str_detect(conn_pool$objClass[[1]],"(?i)postgres")){
          pg_tz <- paste(pool::dbGetQuery(conn_pool, "SELECT current_setting('TIMEZONE')"))
      } else {
          pg_tz <- "UTC"
      }

      dplyr::case_when(
      stringr::str_detect(conn_pool$objClass[[1]],"(?i)Snowflake") ~ 'UTC',
      stringr::str_detect(conn_pool$objClass[[1]],"(?i)sqlite") ~ '',
      stringr::str_detect(conn_pool$objClass[[1]],"(?i)postgres") ~ pg_tz,
      T ~ Sys.timezone()
      )
  }





}
