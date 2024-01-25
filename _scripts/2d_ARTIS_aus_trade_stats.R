# check stats
library(tidyverse)
library(here) 
library(janitor)
library(readxl)
library(ggbreak)
library(ggpubr)

trade <- read_csv(here("_input_data/ARTIS/20231113_australia_ARTIS_australia_cites_iucn.csv"))

#FAO production data is already in live weight
production <- read_csv(here("_input_data", "FAO_fishstat/production_isscaap_species.csv")) |> 
  clean_names() |> 
  select(-unit) |>
  rename(country = country_name,
         sp_name = asfis_species_name_2, 
         isscaap_group = asfis_species_name_3,
         #isscaap_group = asfis_species_name_1,
         fishing_area = fao_major_fishing_area_name,
         # fishing_area_code = fao_major_fishing_area_code, 
         unit = unit_name ) |> 
  filter(country == "Australia") |> 
  pivot_longer(cols = -c(1:7), names_to = "year", values_to = "value") |> 
  mutate(year = gsub(pattern = "x", replacement = "", year) |> as.numeric()) 

production_2020 <- production |> 
  filter(unit != "Number", year == "2020") |> 
  reframe(t_total = sum(value))

# total production in Australia 2020
# A tibble: 1 × 1
# total
# <dbl>
# 279277

exports <- trade |> 
  filter(source_country_iso3c == "AUS", 
        # dom_source == "domestic",
         year == "2020") |>
  group_by(dom_source) |> 
  reframe(total = sum(live_weight_t), 
         prop = (total*100)/production_2020$t_total) |> 
  mutate(t_total = sum(total))

#consumption from ARTIS trade
imports <- trade |> 
  filter(importer_iso3c == "AUS", 
         year == "2020") |> 
  filter(source_country_iso3c != exporter_iso3c & exporter_iso3c != importer_iso3c) |> 
  group_by(method) |> 
  reframe(total = sum(live_weight_t)) |> 
  mutate(t_total = sum(total))

#calculate re-exports
re_exp <- trade |> 
  filter(source_country_iso3c == "AUS", 
         year == "2020", 
         source_country_iso3c == importer_iso3c) |> 
  group_by(method) |> 
  reframe(total = sum(live_weight_t)) |> 
  mutate(t_total= sum(total))

#reexports 
#569
(re_exp$t_total*100)/imports$t_total 

production_2020$t_total + imports$t_total - exports$t_total

#451367.3

# % of imports
(imports$t_total *100)/451367.3

(exports$t_total *100)/production_2020$t_total

#### check EPBC stats ####

epbc <-  read_excel(here("AusTrade_ThrSpp/_input_data/EPBC/EPBC_fauna_2023.xls")) |> clean_names() |> 
  mutate(species = tolower(species)) 

#join with production data

epbc_join <- epbc |> 
  mutate(common_name = tolower(common_name), 
         common_name = gsub(" nei", "", common_name),
         #common_name = gsub(",", "", common_name),
         common_name = gsub(", etc.", "", common_name),
         common_name = gsub(" etc.", "", common_name), 
         common_name = str_squish(str_squish(common_name))) |> 
  select(common_name) |> unique()


library(fuzzyjoin)


production_clean <-production |> filter(!is.na(sp_name)) |> mutate(sp_name= tolower(sp_name)) |> 
  filter(grepl("Capture", detailed_production_source_name)) |> 
  select(-c("country", "isscaap_group", "fishing_area", "detailed_production_source_name")) |> 
  rename(sciname = asfis_species_scientific_name) |> 
  mutate(sp_name = gsub(" nei", "", sp_name),
         sp_name = gsub(",", "", sp_name),
         sp_name = gsub(", etc.", "", sp_name),
         sp_name = gsub(" etc.", "", sp_name),
         sp_name = gsub("shark", "", sp_name),
         sp_name = str_squish(sp_name))  
  

production_vec <- production_clean |> 
  select(sp_name) |> unique()

epbc_prod <- production_clean |> 
  left_join(epbc_join, by = c("sp_name" = "common_name")) |> 
  filter(!is.na(category))

# 
# production_clean |> 
# filter(agrepl(sp_name, epbc_join$common_name))
epbc_vec <- epbc_join |> select(common_name) |> 
mutate(common_name = strsplit(as.character(common_name), ",")) %>%
  unnest(common_name) |> 
  mutate(common_name = str_squish(common_name))

# str <- paste0("(?=.*", production_vec,")", collapse="") 
# grepl(epbc_join$species, production_vec, perl=TRUE)

fuzzy_epbc_prod <- epbc_vec |> 
  stringdist_inner_join(production_vec, by = c(common_name = "sp_name"), max_dist = 6) 
  
fuzzy_epbc_prod <- epbc_join |>   
  stringdist_inner_join(production_vec, by = c(species = "sp_name"), max_dist =2)

#join with ARTIS  
trade

trade_epbc <- trade |> 
  filter(source_country_iso3c == "AUS") |> 
  left_join(epbc, by = c("sciname" = "species")) |> 
  mutate(listed = case_when(sciname %in% c("galeorhinus galeus", "hoplostethus atlanticus", 
                                           "thunnus maccoyii", "seriolella brama") ~ "listed", 
                            T ~ "not_listed")) 
#trade_epbc |> group_by(sciname, method) |> filter(!is.na(category)|!is.na(cites_appendix)) |> 
#count(category,cites_appendix) 

#trade |> filter(source_country_iso3c == "AUS", grepl("bidyanus", sciname)) |> 
 # group_by(method) |> reframe(total = sum(live_weight_t))
#silver perch and murray cod (maccullochella peelii) only exported from aquaculture production
#so excluded from this

trade_epbc |> filter(listed == "listed") |>  
  reframe(total = sum(live_weight_t)) 
group_by(sciname) |> 
  arrange(desc(total)) 
  get_summary_stats(total)

trade_epbc |> filter(listed == "listed") |>  group_by(sciname, exporter_iso3c) |> 
  reframe(total = sum(live_weight_t)) |> group_by(sciname) |> 
  arrange(desc(total))
  
### total volumes since they were listed 
listed_total <- trade_epbc |> filter(listed == "listed") |>
  mutate(bef_af = case_when(sciname == "hoplostethus atlanticus" & year >2006 ~ "after",
                            sciname == "galeorhinus galeus" & year > 2009 ~ "after", 
                            sciname == "seriolella brama" & year >2015 ~ "after", 
                            sciname == "thunnus maccoyii" & year >2010 ~ "after", 
                            T ~ "not_listed"))

                                     
listed_total |> filter(listed == 'listed') |> group_by(bef_af) |> reframe(total = sum(live_weight_t))
listed_total |> filter(listed == 'listed', bef_af == "after") |> group_by(sciname, year) |> reframe(total = sum(live_weight_t))
listed_total |> filter(listed == "listed") |> group_by(sciname, bef_af) |> reframe(total = sum(live_weight_t))
# A tibble: 8 × 3
# sciname                 bef_af        total
# <chr>                   <chr>         <dbl>
# galeorhinus galeus      after         689. 
# galeorhinus galeus      not_listed   1029. 
# hoplostethus atlanticus after        2871. 
# hoplostethus atlanticus not_listed  15932. 
# seriolella brama        after          33.6
# seriolella brama        not_listed   1113. 
# thunnus maccoyii        after      103022. 
# thunnus maccoyii        not_listed 121427.

# main importing countries = consumption
imp_countries <- trade_epbc |> filter(listed == "listed", 
                     exporter_iso3c == "AUS") |>
  select(-source_country_iso3c) |> 
  rename(source_country_iso3c = exporter_iso3c) |> 
  group_by(importer_iso3c) |> 
  reframe(total = sum(live_weight_t)) |> 
  arrange(desc(total)) |> 
  unique()

imp_countries2 <- trade_epbc |> filter(listed == "listed", 
                                       exporter_iso3c != "AUS") |> 
  group_by(importer_iso3c) |> 
  reframe(total = sum(live_weight_t)) |> 
  arrange(desc(total)) |> 
  unique()

all_imps <- rbind(imp_countries, imp_countries2) |> 
  group_by(importer_iso3c) |> 
  reframe(total = sum(total)) |> 
  arrange(desc(total))



#### % of exports for each species####
trade_epbc |> mutate(sp = case_when(sciname == "thunnus maccoyii" ~ "sp", 
                                        T ~ "other")) |> 
  filter(year > 2010) |> 
  group_by(sp) |> 
  reframe(total = sum(live_weight_t)) 
#103022*100/765845

trade_epbc |> mutate(sp = case_when(sciname == "seriolella brama" ~ "sp", 
                                      T ~ "other")) |> 
  filter(year > 2015) |> 
  group_by(sp) |> 
  reframe(total = sum(live_weight_t)) 
#33.6*100/451080

trade_epbc |> mutate(sp = case_when(sciname == "hoplostethus atlanticus" ~ "sp", 
                                    T ~ "other")) |> 
  filter(year > 2006) |> 
  group_by(sp) |> 
  reframe(total = sum(live_weight_t)) 
#689*100/973595 BLUE warehou
#2871*100/1324454 ORANGE roughy

ggplot(listed_total |> filter(listed == "listed", bef_af == "after") |> group_by(sciname, year) |> 
          reframe(total = sum(live_weight_t))) + 
   geom_line( aes(x = year, y = total, color = sciname)) + 
  scale_y_break(c(1000,8500))

 #### Sankey ####
epbc_trade_sankey <- trade_epbc |> filter(sciname == "thunnus maccoyii", year > 2010)

sankey_df<- filter_artis(data = epbc_trade_sankey, prod_method = "capture") 


sankey_df <- sankey_df %>% ungroup() |> 
  select(-prod_method) |> 
  # Tranforming into ggsankey format (x, node, next_x, next_node)
  ggsankey::make_long(producers, exporters, importers, value = total_q)

num_nodes <- length(unique(c(sankey_df$node,sankey_df$next_node)))

cols = brewer.pal(11, "Spectral")
cols <- as.vector(colorRampPalette(cols)(num_nodes))

# Visualizing sankey diagram--------------------------------------------------
tuna<- sankey_df %>%
  ggplot(aes(x = x, 
             next_x = next_x, 
             node = node, 
             next_node = next_node,
             fill = factor(node),
             value = value,
             label = node)) +
  geom_sankey(flow.alpha = 0.6) +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  theme_sankey(base_size = 18) +
  scale_fill_manual(values = cols) + 
  labs(x = NULL) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank()
  ) 


epbc_trade_sankey <- trade_epbc |> filter(sciname == "galeorhinus galeus", year > 2009)

sankey_df<- filter_artis(data = epbc_trade_sankey, prod_method = "capture") 


sankey_df <- sankey_df %>% ungroup() |> 
  select(-prod_method) |> 
  # Tranforming into ggsankey format (x, node, next_x, next_node)
  ggsankey::make_long(producers, exporters, importers, value = total_q)

num_nodes <- length(unique(c(sankey_df$node,sankey_df$next_node)))

cols = brewer.pal(11, "Spectral")
cols <- as.vector(colorRampPalette(cols)(num_nodes))

# Visualizing sankey diagram--------------------------------------------------
sch_shark<- sankey_df %>%
  ggplot(aes(x = x, 
             next_x = next_x, 
             node = node, 
             next_node = next_node,
             fill = factor(node),
             value = value,
             label = node)) +
  geom_sankey(flow.alpha = 0.6) +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  theme_sankey(base_size = 18) +
  scale_fill_manual(values = cols) + 
  labs(x = NULL) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank()
  ) 

# patchwork

(tuna + b_ware) / (sch_shark + or_rough) + 
  plot_annotation(tag_levels = c('A', '1'))


### patchwork for all 


all_sankey <- trade_epbc |> filter(sciname != "thunnys maccoyii", listed == "listed", year > 2000)

sankey_df<- filter_artis(data = all_sankey, prod_method = "capture") 


sankey_df <- sankey_df %>% ungroup() |> 
  select(-prod_method) |> 
  # Tranforming into ggsankey format (x, node, next_x, next_node)
  ggsankey::make_long(producers, exporters, importers, value = total_q)

num_nodes <- length(unique(c(sankey_df$node,sankey_df$next_node)))

cols = brewer.pal(11, "Spectral")
cols <- as.vector(colorRampPalette(cols)(num_nodes))

# Visualizing sankey diagram--------------------------------------------------
all_trade_sankey <- sankey_df %>%
  ggplot(aes(x = x, 
             next_x = next_x, 
             node = node, 
             next_node = next_node,
             fill = factor(node),
             value = value,
             label = node)) +
  geom_sankey(flow.alpha = 0.6) +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  theme_sankey(base_size = 18) +
  scale_fill_manual(values = cols) + 
  labs(x = NULL) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank()
  ) 



### OTHER ###
ggplot(trade_epbc |> group_by(listed, year) |> reframe(total = sum(live_weight_t)), 
       aes(x = year, y = total/1000, fill = listed)) + 
  geom_bar(stat = "identity")

#### BAR FIG ####
ggplot(trade_epbc |> filter(listed == "listed") |> group_by(sciname,year) |> reframe(total = sum(live_weight_t)), 
       aes(x = year, y = total/1000, fill = sciname)) + 
  geom_bar(stat = "identity") + 
  geom_segment(aes(x = 2000, y = -0.1, xend = 2000, yend = 12.95)) +  
  annotate("text", x = 2000, y = 13.4, label = "implementation\nof EPBC Act") + 
  scale_fill_brewer(palette = "YlGnBu") + 
  theme_light() +
  theme(legend.text = element_text(size = 12), 
        axis.title = element_text(size = 16), 
        axis.text = element_text(size = 12)) + 
  labs(y = "total tonnes (thousands LWE)")

trade_epbc |> filter(listed == "listed", year >1999) |> reframe(total = sum(live_weight_t))

#### tables and line fig#### 

# table with sciname, common name, EPBC listing, IUCN listing, year of listing, volume

table_dat_epbc <- trade_epbc |> filter(listed == "listed") |> group_by(sciname, year) |> 
  mutate(bef_af = case_when(sciname == "hoplostethus atlanticus" & year >2006 ~ "after",
                            sciname == "galeorhinus galeus" & year > 2009 ~ "after", 
                            sciname == "seriolella brama" & year >2015 ~ "after", 
                            sciname == "thunnus maccoyii" & year >2010 ~ "after", 
                            T ~ "before"), 
         iucn_epbc = "EPBC", 
         listing_date = case_when(sciname == "hoplostethus atlanticus" ~ 2006,
                            sciname == "galeorhinus galeus"~ 2009, 
                            sciname == "seriolella brama" ~2015, 
                            sciname == "thunnus maccoyii" ~2010, 
                            T ~ NA)) |> 
  group_by(sciname, bef_af) |> 
  reframe(total_exported = sum(live_weight_t), common_name, listing_date, iucn_epbc, iucn_redlist_category) |> unique()

ggplot(trade_epbc |> filter(listed == "listed") |> mutate(common_name = case_when(sciname =="hoplostethus atlanticus" ~ "Orange Roughy", 
                                                                                  sciname == "galeorhinus galeus" ~ "School Shark", 
                                                                                  T ~ common_name)) |> 
         group_by(common_name, year) |> reframe(total = sum(live_weight_t))) + 
  geom_line(aes(x = year, y = total, color = common_name), linewidth = 0.75) + 
  scale_y_break(c(4000,6000)) +
  scale_color_brewer(palette = "Spectral") +
  geom_segment(aes(x = 2000, y = -0.1, xend = 2000, yend = 12000)) + #,arrow = arrow(length = unit(2, "mm"))) +  
  annotate("text", x = 2000, y = 12500, label = "implementation\nof EPBC Act") + 
  geom_segment(aes(x = 2006, y = -0.1, xend = 2006, yend = 3000), color = "grey", alpha =0.4, linetype = "dashed") +  
  #annotate(geom = "point", x = 2006, y = 4500) + 
  annotate("text", x = 2006, y = 3500, label = "Orange\nRoughy") + 
  geom_segment(aes(x = 2009, y = -0.1, xend = 2009, yend = 3000), color = "grey", alpha = 0.4, linetype = "dashed") +  
  #annotate(geom = "point", x = 2009, y = 5500) + 
  annotate("text", x = 2009, y = 3300, label = "School\nShark") + 
  geom_segment(aes(x = 2015, y = -0.1, xend = 2015, yend = 3000), color = "grey", alpha = 0.4, linetype = "dashed") +  
  #annotate(geom = "point", x = 2015, y = 4500) + 
  annotate("text", x = 2015, y = 3500, label = "Blue\nWarehou") +
  geom_segment(aes(x = 2010, y = -0.1, xend = 2010, yend = 11050),color = "grey", alpha = 0.4, linetype = "dashed") +  
  #annotate(geom = "point", x = 2010, y = 11500) + 
  annotate("text", x = 2010, y = 11300, label = "Southern\nBluefin Tuna") + 
  theme_pubr(legend = "right") + 
  theme(axis.text.x = element_text(angle = 60, hjust=1), 
        text = element_text(size=16),
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14),
        legend.text = element_text(size=14),
        legend.key.size = unit(0.6, "cm"), 
        plot.margin = margin(1,1,1,1, "cm")) + 
  labs(y = "total tonnes exported (LWE)", color = "common name")




trade_epbc |> filter(grepl("macco", sciname), year >2000) |> reframe(total = sum(live_weight_t))
trade_epbc |> filter(listed == "listed", year >2000) |> reframe(total = sum(live_weight_t))
# 210945

trade_epbc |> filter(year >2000) |> reframe(total = sum(live_weight_t))
#2174661

210945*100/2174661 

trade_epbc |> filter(listed == "listed", year >2000) |> group_by(exporter_iso3c) |> reframe(total = sum(live_weight_t)) |> 
  arrange(desc(total)) |> filter(exporter_iso3c != "AUS") |> slice(1:10)
#most processed in AUS 196734

trade_epbc |> filter(listed == "listed", year >2000, exporter_iso3c == "AUS") |> group_by(importer_iso3c) |> reframe(total = sum(live_weight_t)) |> 
  arrange(desc(total)) |> slice(1:10)

table_dat_iucn <- trade_epbc |> 
  filter(method == "capture", 
         iucn_redlist_category %in% c("Critically Endangered", "Endangered", "Near Threatened", "Vulnerable"), 
         sciname != c("hoplostethus atlanticus", "galeorhinus galeus", "seriolella brama","thunnus maccoyii")) |> 
  mutate(listing_date = case_when(sciname == "galeorhinus galeus" ~ 2000, 
                                  sciname == "thunnus maccoyii" ~ 1996, 
                                  sciname == "anguilla australis" ~ 2019, 
                                  sciname == "atractoscion aequidens" ~ 2020, 
                                  sciname == "bathyraja irrasa" ~ 2009, 
                                  sciname == "bidyanus bidyanus" ~ 1996, 
                                  sciname == "carcharhinus amboinensis" ~ 2000,    ## DD before 2021
                                  sciname == "carcharhinus sorrah" ~ 2009,
                                  sciname == "cherax destructor" ~ 1996,  #not assessed since
                                  sciname == "eusphyra blochii" ~ 2003, 
                                  sciname == "haliotis rubra" ~ 2021, 
                                  sciname == "istiophorus platypterus" ~ 2022, # 2011 least concern 
                                  sciname == "lamna nasus" ~ 1996, 
                                  sciname == "makaira nigricans" ~ 2011, 
                                  sciname == "pomatomus saltatrix" ~ 2015, 
                                  sciname == "rhizoprionodon acutus" ~ 2020, #2003 LC, 
                                  sciname == "scomberomorus commerson" ~ 2011,
                                  sciname == "somniosus pacificus" ~ 2009, #DD before 2021
                                  sciname == "thunnus obesus" ~ 1996, 
                                  sciname == "xiphias gladius" ~ 1996, 
                                  T ~ NA),                           
         common_name = case_when(sciname == "anguilla australis" ~ "shortfin eel", 
                                 sciname == "atractoscion aequidens" ~ "african weakfish", 
                                 sciname == "bathyraja irrasa" ~ "kerguelen sandpaper skate", 
                                 sciname == "bidyanus bidyanus" ~ "silver perch", 
                                 sciname == "carcharhinus amboinensis" ~ "pigeye shark",
                                 sciname == "carcharhinus sorrah" ~ "spottal shark",
                                 sciname == "eusphyra blochii" ~ "winghead shark",
                                 sciname == "haliotis rubra" ~ "blacklip abalone", 
                                 sciname == "istiophorus platypterus" ~ "sailfish", 
                                 sciname == "lamna nasus" ~ "porbeagle", 
                                 sciname == "makaira nigricans" ~ "blue marlin", 
                                 sciname == "pomatomus saltatrix" ~ "blue fish", 
                                 sciname == "rhizoprionodon acutus" ~ "milk shark", 
                                 sciname == "scomberomorus commerson" ~ "narrow-barred spanish mackerel", 
                                 sciname == "somniosus pacificus" ~ "pacific sleeper shark", 
                                 sciname == "thunnus obesus" ~ "bigeye tuna",
                                 sciname == "thunnus maccoyii" ~ "southern bluefin tuna",
                                 sciname == "xiphias gladius" ~ "swordfish", 
                                 sciname == "galeorhinus galeus" ~ "school shark",
                                 T ~ sciname), 
         iucn_epbc = "IUCN",
         bef_af = case_when(year > listing_date ~ "after", 
                            T ~ "before")) |> 
  group_by(sciname, bef_af) |>
  reframe(total_exported = sum(live_weight_t), common_name, listing_date, iucn_epbc, iucn_redlist_category) |> unique()

table_listed <-table_dat_epbc |> 
  rbind(table_dat_iucn) |> distinct()

porbeagle<- trade_epbc |> filter(grepl("lamna", sciname)) |> mutate(bef_af = case_when(year > 2009 ~ "after", 
                                                                                       T~ "before")) |> 
  group_by(year, bef_af) |> 
  reframe(total = sum(live_weight_t))

#write.csv(table_listed, here("AusTrade_ThrSpp/tables/exported_vols_iucn-epbc.csv"), row.names = FALSE)

#### species proposed####

not_assessed <-read_excel(here("AusTrade_ThrSpp/_input_data/EPBC/EPBC_nominated_sp_11Dec2023.xlsx"), sheet = 1) |> 
  clean_names() |> mutate(sciname = tolower(species_name)) |> 
  select(-species_name)

not_assessed_trade <- not_assessed |> left_join(trade_epbc, by = "sciname", relationship = "many-to-many") |> 
  filter(!is.na(source_country_iso3c))

assessed <- read_excel(here("AusTrade_ThrSpp/_input_data/EPBC/EPBC_nominated_sp_11Dec2023.xlsx"), sheet = 2) |> 
  clean_names() |> mutate(sciname = tolower(sciname))

assessed_trade <- assessed |> left_join(trade_epbc, by = "sciname", relationship = "many-to-many") |> 
  filter(!is.na(source_country_iso3c))


# check with CITES 

CITES <- read_csv(here("_input_data/CITES/Index_of_CITES_Species_2024-01-03 05 25.csv")) |> 
  filter(Kingdom == "Animalia") |> 
  mutate(sciname = tolower(FullName)) |> 
    select(sciname, CurrentListing, CitesAccepted)

CITES_epbc <- trade_epbc |> 
  left_join(CITES, by = "sciname") 

CITES_epbc |> filter(!is.na(CurrentListing)) |> count(sciname, CurrentListing, year)

