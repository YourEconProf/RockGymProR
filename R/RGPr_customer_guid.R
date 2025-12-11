#' Get GUID-CUSTOMER_ID table
#'
#' This allows all calls, including calls for checkins and invoices, to return GUID
#'
#' @param RGPconn this is a connection to an RGP database from RGPr_open_conn
#'
#' @return a tibble containing GUID and CUSTOMER_ID for the current facility
#'
#' @importFrom DBI dbConnect
#' @import magrittr
#' @importFrom dplyr tbl select collect
#' @importFrom tibble as_tibble
#'
#' @export
#'

RGPr_customer_guid <- function(RGPconn=NULL){

  customer_guid <- tbl(RGPconn,"customers") %>%
    select(CUSTOMER_ID,GUID) %>%
    collect() %>%
    as_tibble()


}
