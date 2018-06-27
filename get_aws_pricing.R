# Get Amazon Web Services Pricing
library(tidyverse)
library(RCurl)
library(XML)
library(jsonlite)

# Using the Bulk API 

# The AWS Price List API is actually a URL that provides
# up-to-date pricing information on the current AWS products and services. To
# access pricing information using the AWS Price List API, download the offer
# file:
#
# Offer file – A JSON or CSV file that lists the products and prices for either
# a single AWS service in all regions or a single AWS service in a specific
# region. For more information, see Downloading an Offer File.
#
# To find a list of all available offer files, download the offer index file:
#
# Offer index file – A JSON file that lists the supported AWS services, with a
# URL for each offer file where you can download pricing details. The file also
# includes metadata about the offer index file itself, URLs for service offer
# files, and URLs for regional offer index files. For more information, see
# Downloading an Offer Index File.
#
# Offer files do not include information about expiring free tier offers or
# Amazon EC2 Spot Instances.
#
# Note
#
# The AWS Price List API provides pricing details for your information only. If
# there is a discrepancy between the offer file and a service pricing page, AWS
# charges the prices that are listed on the service pricing page. For more
# information about AWS service pricing, see Cloud Services Pricing.


## Offers JSON is a directory of the partial pricing files for various price lists.  
## Each file can be downloaded from each region.  There *may* be price differences from region to region.
PricingJSON <- "https://pricing.us-east-1.amazonaws.com/offers/v1.0/aws/index.json"


doc <- getURL(PricingJSON)

root <- fromJSON(doc)

# Isolate the offers from the rest of the R object .
offers <- root$offers

off_names <-  map_chr(1:length(offers), ~ as.character(offers[[.x]]['offerCode']))

# Build tibble for Offer Names and corresponding URL

price_dir <- tibble(
  Item = c(1:length(offers)),
  offerName = off_names,
  pURL = map_chr(1:length(offers), ~ offers[[.x]]['currentVersionUrl']),
  JSON = map_chr(1:length(offers), ~ paste("https://pricing.us-east-1.amazonaws.com", pURL[.x], sep = "")),
  CSV  = map_chr(JSON, ~ gsub("json$", "csv", .x))
)

 p_tbl <- map(price_dir$CSV, ~ read_csv(.x, skip = 5))
