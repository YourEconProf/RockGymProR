% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RGPr_get_customers.R
\name{RGPr_get_customers}
\alias{RGPr_get_customers}
\title{Get Customer Data}
\usage{
RGPr_get_customers(RGP_databases, return_all = FALSE)
}
\arguments{
\item{RGP_databases}{output from a previous call to RGPr_Import_Gyms}

\item{return_all}{TRUE/FALSE whether to return all columns}
}
\value{
all_customers a tibble
}
\description{
Gets primary data from customer data table using information on the
facilities obtained via a call to RGP_get_gyms. If multiple codes are
submitted, then this will loop over all locations and concatenate the results
}
