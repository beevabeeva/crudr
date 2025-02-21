% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdr_manage_db_tbls.r
\name{cdr_manage_db_tbls}
\alias{cdr_manage_db_tbls}
\title{Server module to present and control a db table}
\usage{
cdr_manage_db_tbls(
  db_tbl_name,
  schema = "public",
  key_col,
  conn_pool,
  session,
  add_row_permission = F,
  del_row_permission = F,
  cell_edit_permission = F,
  lock_fields = c(),
  multiuser_update_wait = 0
)
}
\arguments{
\item{db_tbl_name}{primary table name - namespace ID corresponding to the}

\item{schema}{schema containing primary table}

\item{key_col}{name of the unique ID column in the db table (table must have
a unique ID column with unique IDs)}

\item{conn_pool}{db connection from package 'pool'}

\item{session}{current shiny session}

\item{add_row_permission}{T or F: allows user to add a row to the primary table of the module}

\item{del_row_permission}{T or F: allows user to delete a row on the primary table of the module}

\item{cell_edit_permission}{T or F: to make editable the primary table from the module
(cell_edit_permission = T means the user can change the data)
(cell_edit_permission = F means the user can only see the data)}

\item{lock_fields}{strings: a vector of field names from the database to lock from admin editing}

\item{multiuser_update_wait}{numeric: minimum time in seconds between checking for
and incorporating any data changes made by other users}
}
\value{
returns DT reactive tables to the shiny ui environment
}
\description{
This function is a server module that gets database tables from db specified by pool connection
and the db table name specified in 'db_tbl_name', server module manages and syncs
changes between the UI DT, in-server-memory tbl, and backend db, for the
primary as well as the corresponding deltas table (and joined table if it's
presented in the UI)
}
\examples{
\dontrun{
con <- pool::dbPool(DBI::dbConnect(RSQLite::SQLite(), 'iris.db'))
crudr::cdr_make_db_tbls(con, iris)
server <- function(input, output, session){
             r_tbl <- crudr::cdr_manage_db_tbls('iris', 'UID', con, session)
             output$iris <- DT::renderDT(r_tbl()) }
ui <- shiny::fluidPage(DT::DTOutput('iris'))
shiny::shinyApp(ui,server)
}
}
