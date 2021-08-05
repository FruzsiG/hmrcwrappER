# Libraries you will need
for (i in c("dplyr","stringr","httr", "jsonlite")) {
  if (i %in% rownames(installed.packages())) {
    library(i, character.only = TRUE)
  } else {
    install.packages(i)
    library(i, character.only = TRUE)
  }
}
library(dplyr)
library(stringr)
library(httr)
library(jsonlite)
#library(htmlTable)


#'HMRC OTS query based on code and HS
#'
#'Querying the HMRC OTS data using HS nomenclature. The arguments are not case-sensitive the API itself is still in BETA thus we continue reporting issues.
#'
#' @param time_window like "MonthId gt 202000 and MonthId lt 202100" which is everything from 2020 or "MonthId gt 201908 and MonthId lt 202009" last 12 month up to end of August
#' @param filter_for_code A HS or CN commodity code e.g. "0202" or "01061410"
#' @param country_filter use ISO-2-alpha code e.g. "FR" check HMRC for inconsistencies such as former Yugoslavian countries.
#' @param flow_filter choose from "export" "import" or "EU"
#' @param groupby takes a vector of keywords the full range is  c("country", "year", "month", "HS2", "HS4", "HS6", "CN8")
#' @keywords HMRC API
#' @export
#' @import dplyr
#' @import jsonlite
#' @import stringr
#' @import httr
#' @examples
#'  call_hs_hmrc_ots_with_product_code(time_window = "MonthId eq 201901", flow_filter = "EU", groupby = c("HS4", "COUNTRY"))
#'  call_hs_hmrc_ots_with_product_code(time_window = "MonthId gt 201906 and MonthId lt 202008", country_filter = "FR", flow_filter = "EXport", groupby = c("HS2", "COUNTRY"))


call_hs_hmrc_ots_with_product_code <- function(time_window = NULL, # like "MonthId gt 202000 and MonthId lt 202100" which is everything from 2020 or "MonthId gt 201908 and MonthId lt 202009" last 12 month up to end of August
                                               filter_for_code = NULL, # A HS or CN commodity code e.g. "0202" or "01061410"
                                               country_filter = NULL, # use ISO2 code e.g. "FR"
                                               flow_filter = NULL, #"export", "import" or "eu"
                                               groupby = NULL # ("country", "year" or "month", "Hsx")
){


  base <-"https://api.uktradeinfo.com/" #https://frontdoor-hmrcukti-uat.azurefd.net/"

  #This is where you select "ots" for Overseas Trade Statistics and "rts" for Regional Trade statistics
  endpoint = "ots"
  #Decide an aggregation level from "Hs2", "Hs4", "Hs6" and "Cn8"

  if(!is.null(filter_for_code)){
    if(nchar(trimws(filter_for_code)) == 2){
      aggfilter = "Hs2"
    } else if(nchar(trimws(filter_for_code)) == 4){
      aggfilter = "Hs4"
    } else if(nchar(trimws(filter_for_code)) == 6){
      aggfilter = "Hs6"
    } else if(nchar(trimws(filter_for_code)) == 8){
      aggfilter = "Cn8"
    } else {
      print("Invalid product code format try again!")
      return()
    }
  }
  #Check if time window specified

  if( is.null(time_window)){

    print("You have to specify a time window!")
    return()

  } else {
    filter_crit = paste0(time_window, " and CommodityId gt 0 and SuppressionIndex eq 0") #
  }

  #Goods filter is not compulsory

  if(!is.null(filter_for_code)){

    filter_crit = paste0(filter_crit, " and Commodity/", aggfilter, "Code eq '", filter_for_code, "'")
  }

  #Country filter is not compulsory
  if(!is.null(country_filter)){

    if(nchar(trimws(country_filter)) == 2){

      filter_crit <- paste0(filter_crit, " and Country/CountryCodeAlpha eq '", country_filter, "'")
    } else {
      print("Please use ISO2-Alpha country code e.g. 'FR'.")
      return()
    }
  }

  #flow filter also not compulsory
  if(!is.null(flow_filter)){

    flow_filter = tolower(flow_filter)

    if(flow_filter == "import"){

      filter_crit <- paste0(filter_crit, " and (FlowTypeId eq 1 or FlowTYpeId eq 3)")

    } else if(flow_filter == "export"){

      filter_crit <- paste0(filter_crit, " and (FlowTypeId eq 2 or FlowTYpeId eq 4)")

    } else if(flow_filter == "eu"){

      filter_crit <- paste0(filter_crit, " and (FlowTypeId eq 1 or FlowTYpeId eq 2)")

    } else {
      print("Ambiguous flow type please specify 'import', 'export', 'eu' or nothing." )
      return()
    }

  }


  #the default is that the grouping only happens by flow
  grouping_by = "FlowTypeId"

  if(!is.null(groupby)){

    groupby = tolower(groupby)

    if(length(intersect(c("hs2", "hs4", "hs6", "cn8"), groupby)) > 0){

      aggreg = intersect(c("hs2", "hs4", "hs6", "cn8"), groupby)

      grouping_by <- paste0(grouping_by, ", Commodity/", aggreg, "Code, Commodity/", aggreg ,"Description")

      grouping_by = stringr::str_replace(grouping_by, "cn8Description", "cn8LongDescription")

    }
    if("country" %in% groupby){

      grouping_by <- paste0(grouping_by, ", Country/CountryCodeAlpha")

    }

    if("year" %in% groupby){

      grouping_by <- paste0(grouping_by, ", Date/Year")

    }

    if("month" %in% groupby){

      grouping_by <- paste0(grouping_by, ", Date/MonthId")

    }
  }
  query <- paste0("?$apply=filter(",
                  filter_crit,")/groupby((",
                  grouping_by,
                  "), aggregate(Value with sum as SumValue, Netmass with sum as weight_kg))&$count=true")

  full_url <- paste0(base, endpoint, gsub(" ","%20", query))
  message("Your query is : ", full_url)

  df <- hmrc_api_requester(full_url)


  #Commented out for now as it slows down bigger requests
  #print(htmlTable::htmlTable(df, rnames=FALSE))
  #Prep variable
  product_condition = TRUE
  if(!is.null(filter_for_code)){
    product_condition = nchar(filter_for_code) < 4
  }

  #Call EU correction
  if(length(intersect(c("hs4", "hs6", "cn8"),groupby)) == 0 &
                      product_condition &
                      is.null(country_filter)){

     get_eu_estimates(time_window = time_window,
                      code_filter = filter_for_code,
                      flow_filter = flow_filter,
                      code_type = "hs",
                      groupby = groupby) -> eu_corr
     df <- plyr::rbind.fill(df, eu_corr)

  }


  return(df)

}


#'HMRC OTS query base on key word and HS
#'
#'Querying the HMRC OTS data using HS nomenclature. The arguments are not case-sensitive the API itself is still in BETA thus we continue reporting issues.
#'
#' @param time_window like "MonthId gt 202000 and MonthId lt 202100" which is everything from 2020 or "MonthId gt 201908 and MonthId lt 202009" last 12 month up to end of August
#' @param key_word anything like "cheese" or "whisky" or "sprout" stick to single words
#' @param filter_agg the aggregation level at which you are looking for the keyword like "Cn8" or "hs6"
#' @param country_filter use ISO-2-alpha code e.g. "FR" check HMRC for inconsistencies such as former Yugoslavian countries.
#' @param flow_filter choose from "export" "import" or "EU"
#' @param groupby takes a vector of keywords the full range is  c("country", "year", "month", "HS2", "HS4", "HS6", "CN8")
#' @keywords HMRC API
#' @export
#' @import dplyr
#' @import jsonlite
#' @import stringr
#' @import httr
#' @examples
#'  call_hs_hmrc_ots_with_key_word_search(filter_agg = "cn8", time_window = "MonthId eq 201901", groupby = c("hs4", "country"))
#'  call_hs_hmrc_ots_with_key_word_search(key_word = "cheese", filter_agg = "hs4", time_window = "MonthId gt 201906 and MonthId lt 202008", country_filter = "FR", flow_filter = "EXport", groupby = c("CN8", "COUNTRY"))

#
call_hs_hmrc_ots_with_key_word_search <- function(time_window = NULL, # like "MonthId gt 202000 and MonthId lt 202100" which is everything from 2020 or "MonthId gt 201908 and MonthId lt 202009" last 12 month up to end of August
                                                  key_word = NULL, #e.g. "cheese"
                                                  filter_agg = "none", # "Hs2" "Hs4" or "Cn8"
                                                  country_filter = NULL, # use ISO2 code e.g. "FR"
                                                  flow_filter = NULL, #"export", "import", "eu"
                                                  groupby = NULL # (country, year or month, Hsx)
){


  base <-"https://api.uktradeinfo.com/" #https://frontdoor-hmrcukti-uat.azurefd.net/"
  filter_agg = tolower(filter_agg)
  #This is where you select "ots" for Overseas Trade Statistics and "rts" for Regional Trade statistics
  endpoint = "ots"
  #Decide an aggregation level from "Hs2", "Hs4", "Hs6" and "Cn8"

  if(!(filter_agg %in% c("hs2", "hs4", "hs6", "cn8"))){
    print("Invalid product code format try again!")
    return()

  }
  #Check if time window specified

  if( is.null(time_window)){

    print("You have to specify a time window!")
    return()

  } else {
    filter_crit = paste0(time_window, " and CommodityId ne 0 and SuppressionIndex eq 0")
  }

  #Goods filter is not compulsory

  if(!is.null(key_word)){

    filter_crit = paste0(filter_crit, " and contains(Commodity/",filter_agg,"Description, '",key_word,"')")
    filter_crit = stringr::str_replace(filter_crit, "cn8Description", "cn8LongDescription")
  }

  #Country filter is not compulsory
  if(!is.null(country_filter)){

    if(nchar(trimws(country_filter)) == 2){

      filter_crit <- paste0(filter_crit, " and Country/CountryCodeAlpha eq '", country_filter, "'")
    } else {
      print("Please use ISO2-Alpha country code e.g. 'FR'.")
      return()
    }
  }

  #flow filter also not compulsory
  if(!is.null(flow_filter)){
    flow_filter = tolower(flow_filter)

    if(tolower(flow_filter) == "import"){

      filter_crit <- paste0(filter_crit, " and (FlowTypeId eq 1 or FlowTYpeId eq 3)")

    } else if(tolower(flow_filter) == "export"){

      filter_crit <- paste0(filter_crit, " and (FlowTypeId eq 2 or FlowTYpeId eq 4)")

    } else if(flow_filter == "eu"){

      filter_crit <- paste0(filter_crit, " and (FlowTypeId eq 1 or FlowTYpeId eq 2)")
    } else {
      print("Ambiguous flow type please specify 'import', 'export', 'eu' or nothing." )
      return()
    }

  }
  #the default is that the grouping only happens by flow
  grouping_by = "FlowTypeId"

  if(!is.null(groupby)){

    groupby = tolower(groupby)

    if(length(intersect(c("hs2", "hs4", "hs6", "cn8"), groupby)) > 0){

      aggreg = intersect(c("hs2", "hs4", "hs6", "cn8"), groupby)

      grouping_by <- paste0(grouping_by, ", Commodity/", aggreg, "Code, Commodity/", aggreg ,"Description")
      grouping_by = stringr::str_replace(grouping_by, "cn8Description", "cn8LongDescription")
    }
    if("country" %in% groupby){

      grouping_by <- paste0(grouping_by, ", Country/CountryCodeAlpha")

    }

    if("year" %in% groupby){

      grouping_by <- paste0(grouping_by, ", Date/Year")

    }

    if("month" %in% groupby){

      grouping_by <- paste0(grouping_by, ", Date/MonthId")

    }
  }
  query <- paste0("?$apply=filter(",
                  filter_crit,")/groupby((",
                  grouping_by,
                  "), aggregate(Value with sum as SumValue, Netmass with sum as weight_kg))&$count=true")

  full_url <- paste0(base, endpoint, gsub(" ","%20", query))
  message("Your query is : ", full_url)

  df <- hmrc_api_requester(full_url)

  #Commented out for now as it slows down bigger requests
  #print(htmlTable::htmlTable(df, rnames=FALSE))

  #Call EU correction
  if(length(intersect(c("hs4", "hs6", "cn8"),groupby)) == 0 &
     is.null(country_filter)){

    get_eu_estimates(time_window = time_window,
                     code_filter = unique(df$Commodity.Hs2Code),
                     flow_filter = flow_filter,
                     code_type = "hs",
                     groupby = groupby) -> eu_corr
    df <- plyr::rbind.fill(df, eu_corr)

  }



  return(df)

}


#'HMRC OTS query base on code and SITC
#'
#'Querying the HMRC OTS  data using SITC nomenclature. The arguments are not case-sensitive the API itself is still in BETA thus we continue reporting issues.
#'
#' @param time_window like "MonthId gt 202000 and MonthId lt 202100" which is everything from 2020 or "MonthId gt 201908 and MonthId lt 202009" last 12 month up to end of August
#' @param filter_for_code A SITC commodity code e.g. "00" or "001300"
#' @param country_filter use ISO-2-alpha code e.g. "FR" check HMRC for inconsistencies such as former Yugoslavian countries.
#' @param flow_filter choose from "export" "import" or "EU"
#' @param groupby takes a vector of keywords the full range is  c("country", "year", "month", "SITC1", "SITC2", "SITC3", "SITC4")
#' @export
#' @import dplyr
#' @import jsonlite
#' @import stringr
#' @import httr
#' @examples
#'  call_sitc_hmrc_ots_with_product_code(filter_for_code = "02", time_window = "MonthId eq 201901", groupby = c("sitc4", "COUNTRY"))
#'  call_sitc_hmrc_ots_with_product_code(time_window = "MonthId gt 201906 and MonthId lt 202008", country_filter = "FR", flow_filter = "EXport", groupby = c("SITc2", "COUNTRY"))


call_sitc_hmrc_ots_with_product_code <- function(time_window = NULL, # like "MonthId gt 202000 and MonthId lt 202100" which is everything from 2020 or "MonthId gt 201908 and MonthId lt 202009" last 12 month up to end of August
                                             filter_for_code = NULL, # A HS or CN commodity code e.g. "0202" or "01061410"
                                             country_filter = NULL, # use ISO2 code e.g. "FR"
                                             flow_filter = NULL, #"export", "import" or "eu"
                                             groupby = NULL # ("country", "year" or "month", "Hsx")
){


  base <-"https://api.uktradeinfo.com/" #https://frontdoor-hmrcukti-uat.azurefd.net/"
  endpoint = "ots"

  #Decide an aggregation level from "Sitc1" to "Sitc5"

  if(!is.null(filter_for_code)){
    if(nchar(trimws(filter_for_code)) == 1){
      aggfilter = "Sitc1"
    } else if(nchar(trimws(filter_for_code)) == 2){
      aggfilter = "Sitc2"
    } else if(nchar(trimws(filter_for_code)) == 5 & as.numeric(filter_for_code)%%100 == 0){
      aggfilter = "Sitc3"
    } else if(nchar(trimws(filter_for_code)) == 5 & as.numeric(filter_for_code)%%10 == 0){
      aggfilter = "Sitc4"
      #    } else if(nchar(trimws(filter_for_code)) == 5 & as.numeric(filter_for_code)%%10 != 0 & endpoint == "ots"){
      #      aggfilter = "Sitc5"
    } else {
      print("Invalid product code format try again! PLease note RTS is only available at the SITC 1 and 2 levels.")
      return()
    }
  }
  #Check if time window specified

  if( is.null(time_window)){

    print("You have to specify a time window!")
    return()

  }

  filter_crit = paste0(time_window, " and CommoditySitcId ge 0 and SuppressionIndex eq 0")

  #Goods filter is not compulsory

  if(!is.null(filter_for_code)){

    filter_crit = paste0(filter_crit, " and SITC/", aggfilter, "Code eq '", filter_for_code, "'")
  }

  #Country filter is not compulsory
  if(!is.null(country_filter)){

    if(nchar(trimws(country_filter)) == 2){

      filter_crit <- paste0(filter_crit, " and Country/CountryCodeAlpha eq '", country_filter, "'")
    } else {
      print("Please use ISO2-Alpha country code e.g. 'FR'.")
      return()
    }
  }

  #flow filter also not compulsory
  if(!is.null(flow_filter)){

    flow_filter = tolower(flow_filter)

    if(flow_filter == "import"){

      filter_crit <- paste0(filter_crit, " and (FlowTypeId eq 1 or FlowTYpeId eq 3)")

    } else if(flow_filter == "export"){

      filter_crit <- paste0(filter_crit, " and (FlowTypeId eq 2 or FlowTYpeId eq 4)")

    } else if(flow_filter == "eu"){

      filter_crit <- paste0(filter_crit, " and (FlowTypeId eq 1 or FlowTYpeId eq 2)")

    } else {
      print("Ambiguous flow type please specify 'import', 'export', 'eu' or nothing." )
      return()
    }

  }


  #the default is that the grouping only happens by flow
  grouping_by = "FlowTypeId"
  if(endpoint == "rts"){
    grouping_by = "FlowtypeId, GovRegionId"
  }


  if(!is.null(groupby)){

    groupby = tolower(groupby)

    if(length(intersect(c("sitc1", "sitc2", "sitc3", "sitc4"), groupby)) > 0){

      aggreg = intersect(c("sitc1", "sitc2", "sitc3", "sitc4"), groupby)

      grouping_by <- paste0(grouping_by, ", SITC/", aggreg, "Code, SITC/", aggreg ,"Desc")

    }
    if("country" %in% groupby){

      grouping_by <- paste0(grouping_by, ", Country/CountryCodeAlpha")

    }

    if("year" %in% groupby){

      grouping_by <- paste0(grouping_by, ", Date/Year")

    }

    if("month" %in% groupby){

      grouping_by <- paste0(grouping_by, ", Date/MonthId")

    }
  }
  query <- paste0("?$apply=filter(",
                  filter_crit,")/groupby((",
                  grouping_by,
                  "), aggregate(Value with sum as SumValue, Netmass with sum as weight_kg))&$count=true")

  full_url <- paste0(base, endpoint, gsub(" ","%20", query))
  message("Your query is : ", full_url)

  df <- hmrc_api_requester(full_url)

  #Commented out for now as it slows down bigger requests
  #print(htmlTable::htmlTable(df, rnames=FALSE))
  #Call EU correction
  product_condition = TRUE
  if(!is.null(filter_for_code)){
    product_condition = nchar(filter_for_code) < 3
  }

  if(length(intersect(c("sitc3", "sitc4"),groupby)) == 0 &
     product_condition &
     is.null(country_filter)){

    get_eu_estimates(time_window = time_window,
                     code_filter = filter_for_code,
                     flow_filter = flow_filter,
                     code_type = "sitc",
                     groupby = groupby) -> eu_corr
    df <- plyr::rbind.fill(df, eu_corr)

  }

  return(df)

}

#'HMRC OTS query based on key word and SITC
#'
#'Querying the HMRC OTS data using SITC nomenclature. The arguments are not case-sensitive the API itself is still in BETA thus we continue reporting issues.
#'
#' @param time_window like "MonthId gt 202000 and MonthId lt 202100" which is everything from 2020 or "MonthId gt 201908 and MonthId lt 202009" last 12 month up to end of August
#' @param key_word anything like "cheese" or "whisky" or "sprout" stick to single words
#' @param filter_agg the aggregation level at which you are looking for the keyword like "sitc1" or "sitc4"
#' @param country_filter use ISO-2-alpha code e.g. "FR" check HMRC for inconsistencies such as former Yugoslavian countries.
#' @param flow_filter choose from "export" "import" or "EU"
#' @param groupby takes a vector of keywords the full range is  c("country", "year", "month", "SITC1", "SITC2", "SITC3", "SITC4")
#' @keywords HMRC API
#' @export
#' @import dplyr
#' @import jsonlite
#' @import stringr
#' @import httr
#' @examples
#'  call_sitc_hmrc_ots_with_key_word_search(key_word = "dairy", filter_agg = "sitc2", time_window = "MonthId eq 201901", groupby = c("sitc4", "COUNTRY"))
#'  call_sitc_hmrc_ots_with_key_word_search(filter_agg = "sitc1", time_window = "MonthId gt 201906 and MonthId lt 202008", country_filter = "FR", flow_filter = "EXport", groupby = c("SITc2", "COUNTRY"))

call_sitc_hmrc_ots_with_key_word_search <- function(time_window = NULL, # like "MonthId gt 202000 and MonthId lt 202100" which is everything from 2020 or "MonthId gt 201908 and MonthId lt 202009" last 12 month up to end of August
                                                key_word = NULL, #e.g. "cheese"
                                                filter_agg = "none", # "Hs2" "Hs4" or "Cn8"
                                                country_filter = NULL, # use ISO2 code e.g. "FR"
                                                flow_filter = NULL, #"export", "import" or "eu"
                                                groupby = NULL # (country, year or month, Hsx)
){


  base <-"https://api.uktradeinfo.com/" #https://frontdoor-hmrcukti-uat.azurefd.net/"
  endpoint = "ots"
  filter_agg = tolower(filter_agg)

  #Decide an aggregation level from sitc 1 to 4 (5 is no  yet available via the api only through build your own table)

  if(!(filter_agg %in% c("sitc1", "sitc2", "sitc3", "sitc4"))){
    print("Invalid product code format try again!")
    return()
  }
  #Check if time window specified

  if( is.null(time_window)){

    print("You have to specify a time window!")
    return()

  }
  filter_crit = paste0(time_window, " and CommoditySitcId ge 0 and SuppressionIndex eq 0")



  #Goods filter is not compulsory

  if(!is.null(key_word)){

    filter_crit = paste0(filter_crit, " and contains(SITC/",filter_agg,"Desc, '",key_word,"')")
  }

  #Country filter is not compulsory
  if(!is.null(country_filter)){

    if(nchar(trimws(country_filter)) == 2){

      filter_crit <- paste0(filter_crit, " and Country/CountryCodeAlpha eq '", country_filter, "'")
    } else {
      print("Please use ISO2-Alpha country code e.g. 'FR'.")
      return()
    }
  }

  #flow filter also not compulsory
  if(!is.null(flow_filter)){

    flow_filter = tolower(flow_filter)

    if(flow_filter == "import"){

      filter_crit <- paste0(filter_crit, " and (FlowTypeId eq 1 or FlowTYpeId eq 3)")

    } else if(flow_filter == "export"){

      filter_crit <- paste0(filter_crit, " and (FlowTypeId eq 2 or FlowTYpeId eq 4)")

    } else if(flow_filter == "eu"){

      filter_crit <- paste0(filter_crit, " and (FlowTypeId eq 1 or FlowTYpeId eq 2)")

    } else {
      print("Ambiguous flow type please specify 'import', 'export', 'eu' or nothing." )
      return()
    }

  }
  #the default is that the grouping only happens by flow
  grouping_by = "FlowTypeId"

  if(!is.null(groupby)){
    groupby = tolower(groupby)
    if(length(intersect(c("sitc1", "sitc2", "sitc3", "sitc4"), groupby)) > 0){

      aggreg = intersect(c("sitc1", "sitc2", "sitc3", "sitc4"), groupby)

      grouping_by <- paste0(grouping_by, ", SITC/", aggreg, "Code, SITC/", aggreg ,"Desc")

    }
    if("country" %in% groupby){

      grouping_by <- paste0(grouping_by, ", Country/CountryCodeAlpha")

    }

    if("year" %in% groupby){

      grouping_by <- paste0(grouping_by, ", Date/Year")

    }

    if("month" %in% groupby){

      grouping_by <- paste0(grouping_by, ", Date/MonthId")

    }
  }
  query <- paste0("?$apply=filter(",
                  filter_crit,")/groupby((",
                  grouping_by,
                  "), aggregate(Value with sum as SumValue, Netmass with sum as weight_kg))&$count=true")

  full_url <- paste0(base, endpoint, gsub(" ","%20", query))
  message("Your query is : ", full_url)

  df <- hmrc_api_requester(full_url)

  #Commented out for now as it slows down bigger requests
  #print(htmlTable::htmlTable(df, rnames=FALSE))

  if(length(intersect(c("sitc3", "sitc4"),groupby)) == 0 &
     is.null(country_filter)){

    get_eu_estimates(time_window = time_window,
                     code_filter = unique(df$SITC.Sitc2Code),
                     flow_filter = flow_filter,
                     code_type = "sitc",
                     groupby = groupby) -> eu_corr
    df <- plyr::rbind.fill(df, eu_corr)

  }

  return(df)

}


#'HMRC query base on code and SITC
#'
#'Querying the HMRC RTS data using SITC nomenclature. The arguments are not case-sensitive the API itself is still in BETA thus we continue reporting issues.
#'
#' @param time_window like "MonthId gt 202000 and MonthId lt 202100" which is everything from 2020 or "MonthId gt 201908 and MonthId lt 202009" last 12 month up to end of August
#' @param filter_for_code A HS or CN commodity code e.g. "00" or "001300"
#' @param country_filter use ISO-2-alpha code e.g. "FR" check HMRC for inconsistencies such as former Yugoslavian countries.
#' @param flow_filter choose from "export" "import" or "EU"
#' @param groupby takes a vector of keywords the full range is  c("country", "year", "month", "SITC1", "SITC2", "SITC3", "SITC4")
#' @export
#' @import dplyr
#' @import jsonlite
#' @import stringr
#' @import httr
#' @examples
#'  call_sitc_hmrc_rts_with_product_code(filter_for_code = "02", time_window = "MonthId eq 201901", groupby = c("sitc4", "COUNTRY"))
#'  call_sitc_hmrc_rts_with_product_code(time_window = "MonthId gt 201906 and MonthId lt 202008", country_filter = "FR", flow_filter = "EXport", groupby = c("SITc2", "COUNTRY"))


call_sitc_hmrc_rts_with_product_code <- function(time_window = NULL, # like "MonthId gt 202000 and MonthId lt 202100" which is everything from 2020 or "MonthId gt 201908 and MonthId lt 202009" last 12 month up to end of August
                                             filter_for_code = NULL, # A HS or CN commodity code e.g. "0202" or "01061410"
                                             country_filter = NULL, # use ISO2 code e.g. "FR"
                                             flow_filter = NULL, #"export", "import" or "eu"
                                             groupby = NULL # ("country", "year" or "month", "Hsx")
){


  base <-"https://api.uktradeinfo.com/" #https://frontdoor-hmrcukti-uat.azurefd.net/"
  endpoint = "rts"

  #Decide an aggregation level from "Sitc1" to "Sitc2"

  if(!is.null(filter_for_code)){
    if(nchar(trimws(filter_for_code)) == 1){
      aggfilter = "Sitc1"
    } else if(nchar(trimws(filter_for_code)) == 2){
      aggfilter = "Sitc2"
    } else {
      print("Invalid product code format try again! Please note RTS is only available at the SITC 1 and 2 levels.")
      return()
    }
  }
  #Check if time window specified

  if( is.null(time_window)){

    print("You have to specify a time window!")
    return()

  } else {
    filter_crit = time_window #
  }

  #Goods filter is not compulsory

  if(!is.null(filter_for_code)){

    filter_crit = paste0(filter_crit, " and SITC/", aggfilter, "Code eq '", filter_for_code, "'")
  }

  #Country filter is not compulsory
  if(!is.null(country_filter)){

    if(nchar(trimws(country_filter)) == 2){

      filter_crit <- paste0(filter_crit, " and Country/CountryCodeAlpha eq '", country_filter, "'")
    } else {
      print("Please use ISO2-Alpha country code e.g. 'FR'.")
      return()
    }
  }

  #flow filter also not compulsory
  if(!is.null(flow_filter)){

    flow_filter = tolower(flow_filter)

    if(flow_filter == "import"){

      filter_crit <- paste0(filter_crit, " and (FlowTypeId eq 1 or FlowTYpeId eq 3)")

    } else if(flow_filter == "export"){

      filter_crit <- paste0(filter_crit, " and (FlowTypeId eq 2 or FlowTYpeId eq 4)")

    } else if(flow_filter == "eu"){

      filter_crit <- paste0(filter_crit, " and (FlowTypeId eq 1 or FlowTYpeId eq 2)")

    } else {
      print("Ambiguous flow type please specify 'import', 'export', 'eu' or nothing." )
      return()
    }

  }


  #the default is that the grouping only happens by flow
  grouping_by = "FlowtypeId, GovRegionId"


  if(!is.null(groupby)){

    groupby = tolower(groupby)

    if(length(intersect(c("sitc1", "sitc2"), groupby)) > 0){

      aggreg = intersect(c("sitc1", "sitc2"), groupby)

      grouping_by <- paste0(grouping_by, ", SITC/", aggreg, "Code, SITC/", aggreg ,"Desc")

    }
    if("country" %in% groupby){

      grouping_by <- paste0(grouping_by, ", Country/CountryCodeAlpha")

    }

    if("year" %in% groupby){

      grouping_by <- paste0(grouping_by, ", Date/Year")

    }

    if("month" %in% groupby){

      grouping_by <- paste0(grouping_by, ", Date/MonthId")

    }
  }
  query <- paste0("?$apply=filter(",
                  filter_crit,")/groupby((",
                  grouping_by,
                  "), aggregate(Value with sum as SumValue, Netmass with sum as weight_kg))&$count=true")

  full_url <- paste0(base, endpoint, gsub(" ","%20", query))
  message("Your query is : ", full_url)

  df <- hmrc_api_requester(full_url)

  #Commented out for now as it slows down bigger requests
  #print(htmlTable::htmlTable(df, rnames=FALSE))


  return(df)

}

#'HMRC RTS query base on key word and SITC
#'
#'Querying the HMRC OTS data using HS nomenclature. The arguments are not case-sensitive the API itself is still in BETA thus we continue reporting issues.
#'
#' @param time_window like "MonthId gt 202000 and MonthId lt 202100" which is everything from 2020 or "MonthId gt 201908 and MonthId lt 202009" last 12 month up to end of August
#' @param key_word anything like "cheese" or "whisky" or "sprout" stick to single words
#' @param filter_agg the aggregation level at which you are looking for the keyword like "sitc1" or "sitc4"
#' @param country_filter use ISO-2-alpha code e.g. "FR" check HMRC for inconsistencies such as former Yugoslavian countries.
#' @param flow_filter choose from "export" "import" or "EU"
#' @param groupby takes a vector of keywords the full range is  c("country", "year", "month", "SITC1", "SITC2", "SITC3", "SITC4")
#' @keywords HMRC API
#' @export
#' @import dplyr
#' @import jsonlite
#' @import stringr
#' @import httr
#' @examples
#'  call_sitc_hmrc_rts_with_key_word_search(key_word = "dairy", filter_agg = "sitc2", time_window = "MonthId eq 201901", groupby = c("sitc4", "COUNTRY"))
#'  call_sitc_hmrc_rts_with_key_word_search(filter_agg = "sitc1", time_window = "MonthId gt 201906 and MonthId lt 202008", country_filter = "FR", flow_filter = "EXport", groupby = c("SITc2", "COUNTRY"))

call_sitc_hmrc_rts_with_key_word_search <- function(time_window = NULL, # like "MonthId gt 202000 and MonthId lt 202100" which is everything from 2020 or "MonthId gt 201908 and MonthId lt 202009" last 12 month up to end of August
                                                key_word = NULL, #e.g. "cheese"
                                                filter_agg = "none", # "Hs2" "Hs4" or "Cn8"
                                                country_filter = NULL, # use ISO2 code e.g. "FR"
                                                flow_filter = NULL, #"export", "import" or "eu"
                                                groupby = NULL # (country, year or month, Hsx)
){


  base <-"https://api.uktradeinfo.com/" #https://frontdoor-hmrcukti-uat.azurefd.net/"
  endpoint = "rts"
  filter_agg = tolower(filter_agg)

  #Decide an aggregation level from "Hs2", "Hs4", "Hs6" and "Cn8"

  if(!(filter_agg %in% c("sitc1", "sitc2") & endpoint == "rts")){
    print("Invalid product code format try again!")
    return()
  }
  #Check if time window specified

  if( is.null(time_window)){

    print("You have to specify a time window!")
    return()

  } else {
    filter_crit = time_window #paste0(time_window, " and CommoditySITCId ge 0 and SuppressionIndex eq 0")
  }

  #Goods filter is not compulsory

  if(!is.null(key_word)){

    filter_crit = paste0(filter_crit, " and contains(SITC/",filter_agg,"Desc, '",key_word,"')")
  }

  #Country filter is not compulsory
  if(!is.null(country_filter)){

    if(nchar(trimws(country_filter)) == 2){

      filter_crit <- paste0(filter_crit, " and Country/CountryCodeAlpha eq '", country_filter, "'")
    } else {
      print("Please use ISO2-Alpha country code e.g. 'FR'.")
      return()
    }
  }

  #flow filter also not compulsory
  if(!is.null(flow_filter)){

    flow_filter = tolower(flow_filter)

    if(flow_filter == "import"){

      filter_crit <- paste0(filter_crit, " and (FlowTypeId eq 1 or FlowTYpeId eq 3)")

    } else if(flow_filter == "export"){

      filter_crit <- paste0(filter_crit, " and (FlowTypeId eq 2 or FlowTYpeId eq 4)")

    } else if(flow_filter == "eu"){

      filter_crit <- paste0(filter_crit, " and (FlowTypeId eq 1 or FlowTYpeId eq 2)")

    } else {
      print("Ambiguous flow type please specify 'import', 'export', 'eu' or nothing." )
      return()
    }

  }
  #the default is that the grouping only happens by flow
  grouping_by = "FlowTypeId"
  if(endpoint == "rts"){
    grouping_by = "FlowtypeId, GovRegionId"
  }

  if(!is.null(groupby)){
    groupby = tolower(groupby)
    if(length(intersect(c("sitc1", "sitc2"), groupby)) > 0){

      aggreg = intersect(c("sitc1", "sitc2"), groupby)

      grouping_by <- paste0(grouping_by, ", SITC/", aggreg, "Code, SITC/", aggreg ,"Desc")

    }
    if("country" %in% groupby){

      grouping_by <- paste0(grouping_by, ", Country/CountryCodeAlpha")

    }

    if("year" %in% groupby){

      grouping_by <- paste0(grouping_by, ", Date/Year")

    }

    if("month" %in% groupby){

      grouping_by <- paste0(grouping_by, ", Date/MonthId")

    }
  }
  query <- paste0("?$apply=filter(",
                  filter_crit,")/groupby((",
                  grouping_by,
                  "), aggregate(Value with sum as SumValue, Netmass with sum as weight_kg))&$count=true")

  full_url <- paste0(base, endpoint, gsub(" ","%20", query))
  message("Your query is : ", full_url)

  df <- hmrc_api_requester(full_url)

  new_df <- add_region_name(df)
  return(new_df)

}


#'HMRC OTS EU estimates corrector
#'
#'Mostly used to add EU estimates corrections to queries but can be used on its own
#'
#' @param time_window like "MonthId gt 202000 and MonthId lt 202100" which is everything from 2020 or "MonthId gt 201908 and MonthId lt 202009" last 12 month up to end of August. It is taken automatically from the query
#' @param code_filter like "02" only works with HS as there is no breakdown for SITC on the API. It is taken automatically from the query
#' @param flow_filter choose from "export" "import" or "EU"
#' @param code_type 'hs' or 'sitc' populated automatically in code
#' @param groupby takes a vector of keywords the full range is  c("country", "year", "month", "hs2"). Disaggregation for EU estimates is not available at a lower level or at SITC level. Taken automatically from function.
#' @keywords HMRC API
#' @export
#' @import dplyr
#' @import jsonlite
#' @import stringr
#' @import httr
#' @examples
#'  get_eu_estimates(time_window = "MonthId gt 201906 and MonthId lt 202008", code_type = 'hs', groupby = c("country", "year", "HS2"))
#'  get_eu_estimates(time_window = "MonthId gt 201906 and MonthId lt 202008", code_filter='02', code_type = 'hs', groupby = c("month", "HS2"))
#'  get_eu_estimates(time_window = "MonthId gt 202008", code_type = 'sitc', groupby = c("country", "year", "month"))


get_eu_estimates <- function(time_window,
                             code_filter = NULL,
                             flow_filter = "eu",
                             code_type,
                             groupby){
  base <-"https://api.uktradeinfo.com/"
  endpoint = "ots"
  if(is.null(flow_filter)){
    flow_filter = "eu"
  } else{
    flow_filter = tolower(flow_filter)
  }


  if(code_type == 'hs'){
  #Please note EU estimates only apply on the HS2 level and to the whole of the EU
    query <- paste0("?$apply=filter(",
                    time_window, " and CommodityId lt 0)&$count=true")
  } else if(code_type == 'sitc'){
    query <- paste0("?$apply=filter(",
                    time_window, " and CommoditySitcId lt -1)&$count=true")
  }
  full_url <- paste0(base, endpoint, gsub(" ","%20", query))
  message("Your query is : ", full_url)

  my_df <- hmrc_api_requester(full_url)

  the_string = c("FlowTypeId")

  if(!is.null(groupby)){
    groupby <- tolower(groupby)

    if(length(intersect(c("hs2"), groupby)) > 0){
      my_df %>% dplyr::mutate(Commodity.Hs2Code = sprintf("%02d",CommodityId*(-0.1))) -> my_df
      if(!is.null(code_filter)){
        my_df %>% dplyr::filter(Commodity.Hs2Code %in% code_filter) -> my_df
      }
      the_string <- c(the_string, "Commodity.Hs2Code")
    }

    if(length(intersect(c("sitc1", "sitc2"), groupby)) > 0){
      my_df %>% dplyr::mutate(SITC.Sitc2Code = sprintf("%02d",floor(CommoditySitcId*(-0.1))))%>%
                dplyr::mutate(SITC.Sitc1Code = sprintf("%01d",floor(CommoditySitcId*(-0.01))))-> my_df
      if(!is.null(code_filter)){
        if(nchar(code_filter) == 2){
            my_df %>% dplyr::filter(SITC.Sitc2Code  %in% code_filter) -> my_df
        }else if(nchar(code_filter) == 1){
            my_df %>% dplyr::filter(SITC.Sitc1Code  %in% code_filter) -> my_df
        }
      }
      if(length(intersect(c("sitc2"), groupby)) > 0){
          the_string <- c(the_string, "SITC.Sitc2Code")
      } else{
          the_string <- c(the_string, "SITC.Sitc1Code")
      }
    }
    if("country" %in% groupby){
        my_df %>% dplyr::mutate(Country.CountryCodeAlpha = "EU_Estimate") -> my_df
      the_string <- c(the_string, "Country.CountryCodeAlpha")
    }

    if("year" %in% groupby){

      my_df %>% dplyr::mutate(Date.Year = str_sub(MonthId, 1,4)) -> my_df
        the_string <- c(the_string, "Date.Year")
    }

    if("month" %in% groupby){
        my_df %>% dplyr::mutate(Date.MonthId = MonthId)-> my_df
       the_string <- c(the_string, "Date.MonthId")
    }
  }

  print(flow_filter)
  if(flow_filter == "export"){
    my_df %>% dplyr::filter(FlowTypeId == "EU Exports") ->my_df
  } else if(flow_filter == "import"){
    my_df %>% dplyr::filter(FlowTypeId =="EU Imports") ->my_df
  }

  my_df %>% dplyr::group_by( .dots = (the_string))%>%
            dplyr::summarise(weight_kg = sum(NetMass),
                             SumValue = sum(Value))%>%
            dplyr::ungroup()  -> df


  return(df)
}

#'HMRC api query function
#'
#'Used by all wrapper functions it takes the URL replaces the spaces with '%20' so the query doesn't break and then tidies the output getting rid of unnecessary things. Gives you a warning if your query is too big.
#'
#' @param full_url
#' @keywords HMRC API
#' @export
#' @import dplyr
#' @import jsonlite
#' @import stringr
#' @import httr
#' @examples
#'  hmrc_api_requester("https://api.uktradeinfo.com/ots?$apply=filter(MonthId gt 202008 and CommoditySitcId lt -1)&$count=true")


hmrc_api_requester <- function(full_url) {
  i <- 1
  pagination <- NULL  # HMRC API will only return upto 30,000 records per page
  apidata <- list()
  while (i > 0) {
    req <- httr::GET(gsub(" ", "%20", paste0(full_url, pagination)))

    # Extract content (in json format)
    json_content <- httr::content(req, as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(flatten = TRUE)

    #check if call worked
       if(json_content$`@odata.count` == 0){
         print("Keyword resulted in 0 lines try a different keyword")
         return()
       }
    # Add page data to the list
    apidata[[i]] <- json_content$value

    # Check for more pages
    if (nrow(json_content$value) == 30000) {
      pagination <- paste0("&$skip=", i * 30000)
      i <- i + 1
    } else {
      i <- 0  # No more pages
    }

    # Prevent downloading too many rows
    if (i > 6) {
      i <- 0  # No more pages
      message("ERROR: Process stopped as this query results in more than 200,000 rows.\nRewrite your query.")
    }
  }

  # Convert list to dataframe, remove NA columns and recode
  df <- bind_rows(apidata) %>%
    select(-contains("@odata.id")) %>%
    mutate(FlowTypeId = dplyr::case_when(FlowTypeId == 1 ~ "EU Imports",
                                         FlowTypeId == 2 ~ "EU Exports",
                                         FlowTypeId == 3 ~ "Non-EU Imports",
                                         FlowTypeId == 4 ~ "Non-EU Exports"))
  return(df)
}

#'Region tidier
#'
#'Used by all RTS calls to add on the correct region name.
#'
#' @param df any dataframe that contains a GovRegionId column
#' @keywords HMRC API
#' @export
#' @import dplyr
#' @import jsonlite
#' @import stringr
#' @import httr
#' @examples
#'  add_region_name(df)

add_region_name <- function(df){

  full_url = "https://api.uktradeinfo.com/Region"
  req <- httr::GET(gsub(" ", "%20", full_url))
  json_content <- httr::content(req, as = "text", encoding = "UTF-8") %>%
                  jsonlite::fromJSON(flatten = TRUE)
  ref <- json_content$value
  new_df <- left_join(df,ref , by = c("GovRegionId" = "RegionId"))
  return(new_df)
}

