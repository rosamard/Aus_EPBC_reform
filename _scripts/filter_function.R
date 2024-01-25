
#### building filter function for sankey plot ####

filter_artis <- function(data, prop_flow_cutoff = 0.05,
                         species = NA, years = NA, top_10 = NA,
                         producers = NA, exporters = NA, importers = NA,
                         hs_codes = NA, prod_method = NA, prod_environment = NA,
                         export_source = NA) {
  data <- data %>% 
    {if (sum(is.na(producers)) == 0)
      filter(., source_country_iso3c %in% producers)
      else .} %>%
    {if (sum(is.na(exporters)) == 0)
      filter(., exporter_iso3c %in% exporters)
      else .} %>%
    {if (sum(is.na(importers)) == 0)
      filter(., importer_iso3c %in% importers)
      else .} |> 
    rename(exporters = exporter_iso3c, 
           importers = importer_iso3c, 
           producers = source_country_iso3c, 
           prod_method = method)
  
  if (sum(is.na(prod_method)) == 0) {
    data <- data %>% 
      {if (prod_method == "aquaculture")
        filter(., prod_method == "aquaculture")
        else .} %>% 
      {if (prod_method == "capture")
        filter(., prod_method == "capture")
        else .}
  }
if (sum(is.na(years)) == 0) {
  data <- data |> 
    filter(year %in% years)
    }
  
data <- data %>%
    group_by(prod_method, producers, exporters, importers) |> 
    summarize(total_q = sum(live_weight_t))
  
  # Getting list of producers limited by proportional flow cutoff
  producers <- data %>%
    group_by(producers) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(total = sum(total_q, na.rm=TRUE)) %>%
    mutate(prop = total_q / total) %>%
    mutate(producers = case_when(
      prop < prop_flow_cutoff ~ "Other",
      TRUE ~ producers
    ))
  
  # Getting list of exporters limited by proportional flow cutoff
  exporters <- data %>%
    group_by(exporters) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(total = sum(total_q, na.rm=TRUE)) %>%
    mutate(prop = total_q / total) %>%
    mutate(exporters = case_when(
      prop < prop_flow_cutoff ~ "Other",
      TRUE ~ exporters
    ))
  
  # Getting list of importers limited by proportional flow cutoff
  importers <- data %>%
    group_by(importers) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(total = sum(total_q, na.rm=TRUE)) %>%
    mutate(prop = total_q / total) %>%
    mutate(importers = case_when(
      prop < prop_flow_cutoff ~ "Other",
      TRUE ~ importers
    ))
  
  # country names of producers, exporters, importers
  prop_cutoff_partners <- unique(
    c(producers$producers, 
      exporters$exporters, 
      importers$importers)
  )
  
  # Creating dataframe from sankey----------------------------------------------
  
  data <- data %>% #### check if I neeed to make links data vs just using data file
    # Filtering data based on prop flow cutoff
    filter(producers %in% prop_cutoff_partners,
           exporters %in% prop_cutoff_partners,
           importers %in% prop_cutoff_partners)
  return(data)
}
