#'HMRC query base on code and SITC
#'
#'Querying the HMRC OTS and RTS data using SITC nomenclature. The arguments are not case-sensitive the API itself is still in BETA thus we continue reporting issues.
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
#'  call_sitc_hmrc_with_product_code(filter_for_code = "02", time_window = "MonthId eq 201901", endpoint = "rts", groupby = c("sitc4", "COUNTRY"))
#'  call_sitc_hmrc_with_product_code(time_window = "MonthId gt 201906 and MonthId lt 202008", endpoint = "ots", country_filter = "FR", flow_filter = "EXport", groupby = c("SITc2", "COUNTRY"))


call_sitc_hmrc_with_product_code <- function(time_window = NULL, # like "MonthId gt 202000 and MonthId lt 202100" which is everything from 2020 or "MonthId gt 201908 and MonthId lt 202009" last 12 month up to end of August
                                             endpoint = "none", # "ots" for overseas trade statistics or "rts" for regional trade statistics
                                             filter_for_code = NULL, # A HS or CN commodity code e.g. "0202" or "01061410"
                                             country_filter = NULL, # use ISO2 code e.g. "FR"
                                             flow_filter = NULL, #"export", "import" or "eu"
                                             groupby = NULL # ("country", "year" or "month", "Hsx")
){


  base <-"https://api.uktradeinfo.com/" #https://frontdoor-hmrcukti-uat.azurefd.net/"
  endpoint = tolower(endpoint)

  #This is where you select "ots" for Overseas Trade Statistics and "rts" for Regional Trade statistics
  if(!(endpoint %in% c("ots", "rts"))){
    print("Choose endpoint value as 'ots' or 'rts' ")
    return()
  }

  #Decide an aggregation level from "Sitc1" to "Sitc5"

  if(!is.null(filter_for_code)){
    if(nchar(trimws(filter_for_code)) == 1){
      aggfilter = "Sitc1"
    } else if(nchar(trimws(filter_for_code)) == 2){
      aggfilter = "Sitc2"
    } else if(nchar(trimws(filter_for_code)) == 5 & as.numeric(filter_for_code)%%100 == 0 & endpoint == "ots"){
      aggfilter = "Sitc3"
    } else if(nchar(trimws(filter_for_code)) == 5 & as.numeric(filter_for_code)%%10 == 0 & endpoint == "ots"){
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

  } else {
    filter_crit = time_window #
    if(endpoint == "ots"){
      filter_crit = paste0(time_window, " and CommoditySitcId ge 0 and SuppressionIndex eq 0")
    }
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
  grouping_by = "FlowTypeId"
  if(endpoint == "rts"){
    grouping_by = "FlowtypeId, GovRegionId"
  }


  if(!is.null(groupby)){

    groupby = tolower(groupby)

    if(length(intersect(c("sitc1", "sitc2", "sitc3", "sitc4"), groupby)) > 0 & endpoint == "ots"){

      aggreg = intersect(c("sitc1", "sitc2", "sitc3", "sitc4"), groupby)

      grouping_by <- paste0(grouping_by, ", SITC/", aggreg, "Code, SITC/", aggreg ,"Desc")

    } else if(length(intersect(c("sitc1", "sitc2"), groupby)) > 0 & endpoint == "rts"){

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
    #grouping_by <- paste0("Commodity/", aggreg, "Code, Commodity/", aggreg ,"Description, Country/CountryCodeAlpha, FlowTypeId")
  }
  #query <- "?$filter= MonthId gt 202000" # Can leave empty (max 30k rows)
  #query <- "?$apply=filter(contains(SITC/SitcDesc, 'wood') and MonthId gt 202000 and MonthId lt 202100)/groupby((SITC/Sitc4Code, SITC/SitcDesc), aggregate(round(Value) with sum as SumValue))"
  #query <- "?$apply=filter(contains(Commodity/Hs2Description, 'wood') and MonthId gt 202000 and MonthId lt 202100)/groupby((Commodity/Hs2Code, Commodity/Hs2Description, Country/CountryCodeAlpha), aggregate(round(Value) with sum as SumValue))"
  query <- paste0("?$apply=filter(",
                  filter_crit,")/groupby((",
                  grouping_by,
                  "), aggregate(Value with sum as SumValue, Netmass with sum as weight_kg))&$count=true")

  full_url <- paste0(base, endpoint, gsub(" ","%20", query))
  message("Your query is : ", full_url)

  df <- hmrc_api_requester(full_url)
  # req <- httr::GET(full_url)
  #
  # # Extarct content (in json format)
  #
  # json_content <- httr::content(req, as = "text", encoding = "UTF-8")
  #
  # # Convert json to dataframe
  #
  # my_df <- jsonlite::fromJSON(json_content, flatten = TRUE) %>%
  #   data.frame()
  #
  #
  # df = dplyr::select(my_df, setdiff(colnames(my_df), "X.odata.nextLink"))
  #
  # i = 1
  # while (nrow(my_df) == 30000 & i < 6) {
  #
  #   count = 30000*i
  #   my_url <- paste0(full_url, "&$skip=",count)
  #   i = i+1
  #
  #   req <- httr::GET(my_url)
  #
  #   json_content <- httr::content(req, as = "text", encoding = "UTF-8")
  #
  #   my_df <- jsonlite::fromJSON(json_content, flatten = TRUE) %>%
  #     data.frame() %>%
  #     dplyr::select(setdiff(colnames(my_df), "X.odata.nextLink"))
  #
  #   if(ncol(my_df) != ncol(df)){
  #
  #     print("Skip 30k does not work for this call (yet). Try to alter your call and loop it")
  #   } else {
  #
  #     df = rbind(df,my_df)
  #   }
  # }
  #
  # #Drop call and recode flow type
  # df %>%
  #   dplyr::select(-X.odata.context) %>%
  #   dplyr::mutate(value.FlowTypeId = dplyr::case_when(value.FlowTypeId == 1 ~ "EU Imports",
  #                                       value.FlowTypeId == 2 ~ "EU Exports",
  #                                       value.FlowTypeId == 3 ~ "Non-EU imports",
  #                                       value.FlowTypeId == 4 ~ "Non-EU Exports")) -> df

  #Commented out for now as it slows down bigger requests
  #print(htmlTable::htmlTable(df, rnames=FALSE))
  #Call EU correction
  if(length(intersect(c("sitc1", "sitc2", "sitc3", "sitc4"),groupby)) == 0 &
     is.null(filter_for_code) &
     is.null(country_filter) &
     endpoint == "ots"){

    get_eu_estimates(time_window = time_window,
                     code_filter = NULL,
                     flow_filter = flow_filter,
                     code_type = "sitc",
                     groupby = groupby) -> eu_corr
    df <- plyr::rbind.fill(df, eu_corr)

  }

  return(df)

}

#'HMRC OTS and RTS query base on key word and SITC
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
#'  call_sitc_hmrc_with_key_word_search(key_word = "dairy", filter_agg = "sitc2", time_window = "MonthId eq 201901", endpoint = "rts", groupby = c("sitc4", "COUNTRY"))
#'  call_sitc_hmrc_with_key_word_search(filter_agg = "sitc1", time_window = "MonthId gt 201906 and MonthId lt 202008", endpoint = "ots", country_filter = "FR", flow_filter = "EXport", groupby = c("SITc2", "COUNTRY"))

call_sitc_hmrc_with_key_word_search <- function(time_window = NULL, # like "MonthId gt 202000 and MonthId lt 202100" which is everything from 2020 or "MonthId gt 201908 and MonthId lt 202009" last 12 month up to end of August
                                                endpoint = "none", # choose RTS or OTS
                                                key_word = NULL, #e.g. "cheese"
                                                filter_agg = "none", # "Hs2" "Hs4" or "Cn8"
                                                country_filter = NULL, # use ISO2 code e.g. "FR"
                                                flow_filter = NULL, #"export", "import" or "eu"
                                                groupby = NULL # (country, year or month, Hsx)
){


  base <-"https://api.uktradeinfo.com/" #https://frontdoor-hmrcukti-uat.azurefd.net/"
  endpoint = tolower(endpoint)
  filter_agg = tolower(filter_agg)

  #This is where you select "ots" for Overseas Trade Statistics and "rts" for Regional Trade statistics
  if(!(endpoint %in% c("ots", "rts"))){
    print("Choose endpoint value as 'ots' or 'rts' ")
    return()
  }
  #Decide an aggregation level from "Hs2", "Hs4", "Hs6" and "Cn8"

  if(!(filter_agg %in% c("sitc1", "sitc2", "sitc3", "sitc4") & endpoint == "ots") & !(filter_agg %in% c("sitc1", "sitc2") & endpoint == "rts")){
    print("Invalid product code format try again!")
    return()
  }
  #Check if time window specified

  if( is.null(time_window)){

    print("You have to specify a time window!")
    return()

  } else {
    filter_crit = time_window #paste0(time_window, " and CommoditySITCId ge 0 and SuppressionIndex eq 0")
    if(endpoint == "ots"){
      filter_crit = paste0(time_window, " and CommoditySitcId ge 0 and SuppressionIndex eq 0")
    }
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
    if(length(intersect(c("sitc1", "sitc2", "sitc3", "sitc4"), groupby)) > 0 & endpoint == "ots"){

      aggreg = intersect(c("sitc1", "sitc2", "sitc3", "sitc4"), groupby)

      grouping_by <- paste0(grouping_by, ", SITC/", aggreg, "Code, SITC/", aggreg ,"Desc")

    } else if(length(intersect(c("sitc1", "sitc2"), groupby)) > 0 & endpoint == "rts"){

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
    #grouping_by <- paste0("Commodity/", aggreg, "Code, Commodity/", aggreg ,"Description, Country/CountryCodeAlpha, FlowTypeId")
  }
  #query <- "?$filter= MonthId gt 202000" # Can leave empty (max 30k rows)
  #query <- "?$apply=filter(contains(SITC/SitcDesc, 'wood') and MonthId gt 202000 and MonthId lt 202100)/groupby((SITC/Sitc4Code, SITC/SitcDesc), aggregate(round(Value) with sum as SumValue))"
  #query <- "?$apply=filter(contains(Commodity/Hs2Description, 'wood') and MonthId gt 202000 and MonthId lt 202100)/groupby((Commodity/Hs2Code, Commodity/Hs2Description, Country/CountryCodeAlpha), aggregate(round(Value) with sum as SumValue))"
  query <- paste0("?$apply=filter(",
                  filter_crit,")/groupby((",
                  grouping_by,
                  "), aggregate(Value with sum as SumValue, Netmass with sum as weight_kg))&$count=true")

  full_url <- paste0(base, endpoint, gsub(" ","%20", query))
  message("Your query is : ", full_url)

  df <- hmrc_api_requester(full_url)
  # req <- httr::GET(full_url)
  #
  # # Extract content (in json format)
  #
  # json_content <- httr::content(req, as = "text", encoding = "UTF-8")
  #
  # #Catch cases when dataset is empty
  # if(str_detect(json_content, "@odata.count\":0")){
  #   print("Keyword resulted in 0 lines try a different keyword")
  #   return()
  # }
  #
  # # Convert json to dataframe
  #
  # my_df <- jsonlite::fromJSON(json_content, flatten = TRUE) %>%
  #   data.frame()
  #
  #
  # df = dplyr::select(my_df, setdiff(colnames(my_df), "X.odata.nextLink"))
  #
  # i = 1
  # while (nrow(my_df) == 30000 & i < 6) {
  #
  #   count = 30000*i
  #   my_url <- paste0(full_url, "&$skip=",count)
  #   i = i+1
  #
  #   req <- httr::GET(my_url)
  #
  #   json_content <- httr::content(req, as = "text", encoding = "UTF-8")
  #
  #   #check if call worked
  #   if(str_detect(json_content, "@odata.count\":0")){
  #     print("Keyword resulted in 0 lines try a different keyword")
  #     return()
  #   }
  #
  #   my_df <- jsonlite::fromJSON(json_content, flatten = TRUE) %>%
  #     data.frame() %>%
  #     dplyr::select(setdiff(colnames(my_df), "X.odata.nextLink"))
  #
  #   if(ncol(my_df) != ncol(df)){
  #
  #     print("Skip 30k does not work for this call (yet). Try to alter your call and loop it")
  #   } else {
  #
  #     df = rbind(df,my_df)
  #   }
  #
  # }
  #
  # # Drop call and Recode FlowType
  # df <- df  %>%
  #   dplyr::select(-X.odata.context)%>%
  #   dplyr::mutate(value.FlowTypeId = dplyr::case_when(value.FlowTypeId == 1 ~ "EU Imports",
  #                                       value.FlowTypeId == 2 ~ "EU Exports",
  #                                       value.FlowTypeId == 3 ~ "Non-EU imports",
  #                                       value.FlowTypeId == 4 ~ "Non-EU Exports"))
  #Commented out for now as it slows down bigger requests
  #print(htmlTable::htmlTable(df, rnames=FALSE))
  if(length(intersect(c("sitc1", "sitc2", "sitc3", "sitc4"),groupby)) == 0 &
     is.null(country_filter) &
     endpoint == "ots"){

    get_eu_estimates(time_window = time_window,
                     code_filter = NULL,
                     flow_filter = flow_filter,
                     code_type = "sitc",
                     groupby = groupby) -> eu_corr
    df <- plyr::rbind.fill(df, eu_corr)

  }

  return(df)

}
