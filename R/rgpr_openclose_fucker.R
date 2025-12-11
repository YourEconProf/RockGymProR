#' Opens a connection to RGP
#'
#' Opens connection to an RGP instance
#'
#' @param address URL of the RGP server
#' @param user username of database on RGP server
#' @param pass password of 'user'
#' @param db_name database name of RGP server
#'
#' @return dbRGP
#'
#' @export

RGPr_open_conn <- function(address=NULL,
                          user=NULL,
                          password=NULL,
                          db_name=NULL){
  if (is.null(address))
    stop("ERROR: open_RGP_conn is missing address.")
  if (is.null(user))
    stop("ERROR: open_RGP_conn is missing user")
  if (is.null(password))
    stop("ERROR: open_RGP_conn is missing password")
  if (is.null(db_name))
    stop("ERROR: open_RGP_conn is missing db_name")

  dbRGP <- DBI::dbConnect(drv=RMariaDB::MariaDB(),
                          dbname=db_name,
                          username=user,
                          password=password,
                          host=address)

  return(dbRGP)

}

#' Closes a connection to RGP
#'
#' Closes a dbRGP handle
#'
#' @param dbRGP handle returned by RGP_open_conn
#'
#' @importFrom DBI dbDisconnect
#'
#' @export

RGPr_close_conn <- function(dbRGP){
  dbRGP <- DBI::dbDisconnect(dbRGP)
}

#' Retrieves list of all facilities
#'
#' Imports information about each RGP install from a user-supplied csv file of the form
#' # First, you'll need to supply a file of gym database info in csv format
#' like this:
#'
#' TAG , ADDRESS          , USER  ,    PASSWORD    , DBNAME ,      X     ,   Y
#' VAO , rgpdb.domain.com ,  bob  ,  bobs-password ,  va513 ,  -82.994614, 40.098792
#' VTC , 192.168.1.3      ,   bob ,  bobs-password ,  vatc  ,   -82.994928, 40.094341
#'
#' @param filename connection handle from open_RGP_conn
#'
#' @return RGP_sites is a list of information about the install
#'
#'
#' @export


RGPr_Import_Gyms <- function(filename){

  rgp_databases  <- readr::read_csv(filename,col_names = TRUE)

  return(rgp_databases)

}


