#' Get Invoices
#'
#' Gets primary data from customer data table using information on the
#' facilities obtained via a call to RGP_get_gyms. If multiple codes are
#' submitted, then this will loop over all locations and concatenate the results
#'
#' @param RGP_databases output from a previous call to RGPr_Import_Gyms
#' @param start_date pull checkins after this date
#' @param end_date pull checkins before this date
#' @param return_all TRUE/FALSE whether to return all columns
#'
#' @return all_customers a tibble
#'
#' @importFrom DBI dbConnect
#' @import magrittr
#' @importFrom dplyr tbl select filter mutate collect
#' @importFrom tibble as_tibble
#'
#' @export

RGPr_get_invoices <- function(RGP_databases,start_date=NULL,end_date=NULL, return_all=FALSE)
{

  RGP_db_tags <- RGP_databases$TAG

  all_invoices <- as_tibble()

  if (is.null(start_date)) start_date = "1901-01-01"
  if (is.null(end_date))   end_date   = Sys.time()

  for (location_tag in RGP_db_tags){
    # Open database connection
    this_location <- RGP_databases %>%
      filter(TAG==location_tag)

    RGPconn <- RGPr_open_conn(address=this_location$ADDRESS,
                             user=this_location$USER,
                             password=this_location$PASSWORD,
                             db_name=this_location$DBNAME)

    if (return_all){
      invoices <- tbl(RGPconn,"invoices") %>%
        filter(POSTDATE>start_date, POSTDATE<end_date) %>%
        collect() %>%
        left_join(RGPr_customer_guid(RGPconn),by=c("CUSTOMER_ID")) %>%
        mutate(FACILITY=location_tag,CUSTOMER_ID=paste0(location_tag,"-",CUSTOMER_ID)) %>%
        as_tibble()
    } else {
      invoices <- tbl(RGPconn,"invoices") %>%
        filter(POSTDATE>start_date, POSTDATE<end_date) %>%
        select(CUSTOMER_ID, POSTDATE, AMOUNT, INVTYPE, FACILITY_ID, CUSTOMER_LOC) %>%
        collect() %>%
        left_join(RGPr_customer_guid(RGPconn),by=c("CUSTOMER_ID")) %>%
        mutate(FACILITY=location_tag,CUSTOMER_ID=paste0(location_tag,"-",CUSTOMER_ID)) %>%
        as_tibble()
    }

    all_invoices <- all_invoices %>%
      rbind(invoices)

    RGPr_close_conn(RGPconn)

  }

 return(all_invoices)

}

