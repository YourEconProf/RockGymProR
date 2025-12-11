#' Get Customer Data
#'
#' Gets primary data from customer data table using information on the
#' facilities obtained via a call to RGP_get_gyms. If multiple codes are
#' submitted, then this will loop over all locations and concatenate the results
#'
#' @param RGP_databases output from a previous call to RGPr_Import_Gyms
#' @param return_all TRUE/FALSE whether to return all columns
#'
#' @return all_customers a tibble
#'
#' @import magrittr
#' @importFrom dplyr tbl select filter
#' @importFrom tibble as_tibble
#'
#' @export

RGPr_get_customers <- function(RGP_databases,return_all=FALSE)
{

  RGP_db_tags <- RGP_databases$TAG

  all_customers <- as_tibble()

  for (location_tag in RGP_db_tags){
    # Open database connection
    this_location <- RGP_databases %>%
      filter(TAG==location_tag)

    RGPconn <- RGPr_open_conn(address=this_location$ADDRESS,
                             user=this_location$USER,
                             password=this_location$PASSWORD,
                             db_name=this_location$DBNAME)

    if (return_all){
      customers <- tbl(RGPconn,"customers") %>%
        collect() %>%
        mutate(FACILITY=location_tag,CUSTOMER_ID=paste0(location_tag,"-",CUSTOMER_ID)) %>%
        as_tibble()
    } else {
      customers <- tbl(RGPconn,"customers") %>%
        select(CUSTOMER_ID, FIRSTNAME, LASTNAME, CUSTOMER_TYPE, ADDRESS1, CITY,
               STATE, CURRENT_STATUS, GUID, CUSTOMER_TYPE) %>%
        collect() %>%
        mutate(FACILITY=location_tag,CUSTOMER_ID=paste0(location_tag,"-",CUSTOMER_ID)) %>%
        as_tibble()
    }

    all_customers <- all_customers %>%
      rbind(customers)

    RGPr_close_conn(RGPconn)

  }

 return(all_customers)

}

