rm(list=ls())
library(tidyverse)
library(readxl)
library(scales)
library(lubridate)
library(tidycensus)
select<-dplyr::select

load("Data/bchc_cleaned_bundle_sept29.rdata")
all_vax<-tidy_zcta_vax %>% filter(grepl("Fully", outcome)) %>% 
  mutate(zcta=as.numeric(GEOID),
         prop=value/100) %>% 
  select(zcta, prop)
table(tidy_zcta_vax$city, tidy_zcta_vax$data_date)

# generate SVI at the ZCTA level [Ran Li's Code]
# ## Load Crosswalks
# load("../../Crosswalks/Clean/xwalk_county_bchc.rdata")
# load("../../Crosswalks/Clean/xwalk_zcta_bchc.rdata")
## Themes Crosswalk
xwalk_themes = tibble(theme = c("RPL_THEME1",
                                "RPL_THEME2",
                                "RPL_THEME3",
                                "RPL_THEME4"),
                      var = list(c("POV","UNEMP","PCI","NOHSDP"),
                                 c("AGE65","AGE17","DISABL","SNGPNT"),
                                 c("MINORITY","LIMENG"),
                                 c("MUNIT","MOBILE","CROWD","NOVEH","GROUPQ")) ) %>% 
  unnest() 

## Variables To Pull
varsBG <- c(
  ## Total Population (TOTPOP)
  'B01003_001',
  #### THEME1 ####
  ## 	Ratio Of Income To Poverty Level In The Past 12 Months (POV)
  'C17002_001','C17002_002','C17002_003','C17002_004',
  ## Employment status 16 years and over (UNEMP)
  'B23025_003','B23025_005',
  ## Per Capita Income (PCI)
  'B19301_001',
  
  
  #### THEME2 ####
  ## Sex By Age (AGE17, AGE65)
  'B01001_020','B01001_021','B01001_022','B01001_023','B01001_024','B01001_025','B01001_044',
  'B01001_045','B01001_046','B01001_047','B01001_048','B01001_049','B01001_003','B01001_004',
  'B01001_005','B01001_006','B01001_027','B01001_028','B01001_029','B01001_030',
  ## 	Educational Attainment For The Population 25 Years And Over (NOHSDP)
  'B15003_001','B15003_016','B15003_017','B15003_018','B15003_019','B15003_020',
  'B15003_021','B15003_022','B15003_023','B15003_024','B15003_025',
  
  #### THEME3 ####
  ## Minority (MINORITY)
  'B03002_003','B03002_003','B03002_001','B25002_001','B25002_003',
  ## Language Spoken At Home By Ability To Speak English (LIMENG)
  "B16004_001",
  "B16004_007","B16004_008","B16004_012","B16004_013","B16004_017","B16004_018",
  "B16004_022","B16004_023","B16004_029","B16004_030","B16004_034","B16004_035",
  "B16004_039","B16004_040","B16004_044","B16004_045","B16004_051","B16004_052",
  "B16004_056","B16004_057","B16004_061","B16004_062","B16004_066","B16004_067",
  ## Single Parent (SNGPNT)
  "B23008_001","B23008_008","B23008_021",
  
  #### THEME4 ####
  ## Units In Structure (MUNIT)
  'B25024_001','B25024_007','B25024_008','B25024_009',
  ## Total Pop in occupied Housing unite by tenure (MOBILE)
  'B25033_001','B25033_006','B25033_007','B25033_012','B25033_013',
  ## Tenure by occupant per room (CROWD)
  'B25014_001','B25014_005','B25014_006','B25014_007','B25014_011','B25014_012','B25014_013',
  ## Tenure by Vehicle available (NOVEH)
  'B25044_001','B25044_003','B25044_010'
)

varsCT <- c(
  "B01003_001",
  #### THEME2 ####
  ## Disability Status (DISABL)
  'B18101_025','B18101_026','B18101_006','B18101_007',
  ## Disability Status by poverty status (DISABL)
  'C18130_009','C18130_010','C18130_016','C18130_017',
  #### THEME3 ####
  ## Group Quarters Population (GROUPQ)
  'B26001_001'
)

varsZCTA = c(varsBG,varsCT)
## Pull ZCTA Data
zcta_pull_raw = get_acs(geography  = "zcta",
                                  year = 2019,
                                  variables=varsZCTA)
## Assign tract variables
zcta_pull = zcta_pull_raw %>% 
  mutate(zcta = GEOID) %>% 
  select(zcta, variable, estimate) %>% 
  spread(variable, estimate)
## ----echo=TRUE----------------------------------------------------------------------------------------------------------------------
zcta_variables = zcta_pull %>% 
  mutate(TOTPOP  = B01003_001, 
         POV = (C17002_002+C17002_003)/C17002_001,
         UNEMP = B23025_005/B23025_003,
         PCI = B19301_001,
         NOHSDP = 1-((B15003_016+B15003_017+B15003_018+B15003_019+
                        B15003_020+B15003_021+B15003_022+B15003_023
                      +B15003_024+B15003_025)/B15003_001),
         LIMENG =  (( 
           B16004_007+B16004_008+
             B16004_012+B16004_013+
             B16004_017+B16004_018+
             B16004_022+B16004_023+
             B16004_029+B16004_030+
             B16004_034+B16004_035+
             B16004_039+B16004_040+
             B16004_044+B16004_045+
             B16004_051+B16004_052+
             B16004_056+B16004_057+
             B16004_061+B16004_062+
             B16004_066+B16004_067)/B16004_001),
         AGE65 = ( B01001_020+B01001_021+B01001_022+B01001_023+B01001_024+
                     B01001_025+B01001_044+B01001_045+B01001_046+B01001_047+
                     B01001_048+B01001_049)/B01003_001,
         AGE17 = (B01001_003+B01001_004+B01001_005+B01001_006+B01001_027+
                    B01001_028+B01001_029+B01001_030)/B01003_001,
         DISABL =(B18101_026+B18101_007+C18130_010+C18130_017)/
           (B18101_025+B18101_006+C18130_009+C18130_016) ,
         SNGPNT =(B23008_008+B23008_021)/B23008_001,
         MUNIT =(B25024_007+B25024_008+B25024_009)/B25024_001,
         MOBILE =(B25033_006+B25033_007+B25033_012+B25033_013)/B25033_001,
         CROWD =(B25014_005+B25014_006+B25014_007+B25014_011+B25014_012+B25014_013)/B25014_001 ,
         NOVEH =(B25044_003+B25044_010)/B25044_001, 
         GROUPQ =B26001_001/B01003_001,
         MINORITY = 1-(B03002_003/B03002_001)
  ) %>% 
  select(-names(zcta_pull %>% select(-zcta))) %>% 
  pivot_longer(-c(zcta),
               names_to = "var",
               values_to = "est")
zcta_rankings = zcta_variables %>% 
  filter(!is.na(est)) %>%
  mutate(reverse = ifelse(var=="PCI",T,F)) %>% 
  group_by(var) %>%
  group_modify(~.x %>% 
                 mutate(rank = ifelse(reverse,
                                      rank(x = est, na.last = "keep", ties.method = "min"),
                                      rank(x = -est, na.last = "keep", ties.method = "min")))) %>% 
  ungroup() %>% 
  select(-reverse)
zcta_pctRank  = zcta_rankings  %>% 
  left_join(xwalk_themes) %>% 
  filter(!is.na(rank)) %>% 
  filter(!is.na(theme)) %>% 
  select(zcta,theme, var, rank) %>% 
  group_by(theme, var) %>% 
  group_modify(~.x %>% 
                 mutate(n_ranks = nrow(.x)) %>% 
                 mutate(pct_rank = (rank-1)/(n_ranks-1)) 
  ) %>% 
  ungroup() %>% 
  select(theme, var, zcta,pct_rank)
zcta_component_themes_pctRank = zcta_pctRank  %>% 
  ## for each theme, sum pct_rank of component variables
  group_by(theme, zcta) %>% 
  summarise(sum_pct_rank = sum(pct_rank)) %>% 
  ungroup() %>% 
  ## ordered the summed percentiles for each theme to determine theme-specific percentile rankings.
  group_by(theme) %>% 
  group_modify(~.x %>% 
                 ## Theme specific Order (aka rank)
                 mutate(rank_theme = rank(x = -sum_pct_rank, 
                                          na.last = "keep", 
                                          ties.method = "min")) %>% 
                 ## THeme specfic ranking
                 mutate(n_ranks = nrow(.x)) %>% 
                 mutate(pct_rank_theme = (rank_theme-1)/(n_ranks-1)) ) %>% 
  ungroup() %>% 
  select(theme,zcta, pct_rank_theme)




## -----------------------------------------------------------------------------------------------------------------------------------
zcta_themes_pctRank = zcta_component_themes_pctRank %>% 
  ## Aggregate  themes 1-4
  group_by(zcta) %>% 
  summarise(sum_pct_rank_theme = sum(pct_rank_theme)) %>% 
  ungroup() %>% 
  mutate(rank_themes = rank(x = sum_pct_rank_theme, 
                            na.last = "keep",
                            ties.method = "min"))  %>% 
  mutate(n_ranks = nrow(.)) %>% 
  mutate(pct_rank_themes = (rank_themes-1)/(n_ranks-1)) %>% 
  select(zcta, RPL_THEMES = pct_rank_themes)

## Compile calculatd SVI
zcta_component_themes_pctRank_wide = zcta_component_themes_pctRank %>% 
  pivot_wider(names_from = theme, values_from = pct_rank_theme)

svi = zcta_themes_pctRank %>% 
  left_join(zcta_component_themes_pctRank_wide) %>% 
  #left_join(xwalk_zcta_bchc) %>% 
  #filter(!is.na(city)) %>% 
  select(zcta, svi = RPL_THEMES  ,
         svi1 = RPL_THEME1  ,
         svi2 = RPL_THEME2  ,
         svi3 = RPL_THEME3  ,
         svi4 = RPL_THEME4   ) %>% 
  mutate_all(as.numeric)

load("data/clean_tidy_zcta_pop.rdata")
pop<-tidy_zcta_pop %>% select(city, zcta, pop) %>% 
  mutate_at(-1, as.numeric) %>% 
  rename(total_pop=pop)
# get % aged 65 or above
zcta_age<-get_acs(geography = "zcta",
                  variables=c(paste0("B01001_", sprintf("%03d", 1:49)),
                              # education
                              "B15003_022","B15003_023","B15003_024","B15003_025",
                              "B15003_001"),
                  year=2019) %>% 
  mutate(GEOID=substr(NAME, 7, 11)) %>% 
  select(GEOID, variable, estimate) %>% 
  spread(variable, estimate) %>% 
  mutate(GEOID=as.numeric(GEOID)) %>% 
  rowwise() %>% 
  mutate(pct_college=sum(c(B15003_022,B15003_023,B15003_024,B15003_025))/B15003_001,
         pct_age1844=sum(c(B01001_007,B01001_008,B01001_009,B01001_010,B01001_011,B01001_012,
                           B01001_013,B01001_014,B01001_031,B01001_032,B01001_033,B01001_034,
                           B01001_035,B01001_036,B01001_037,B01001_038))/B01001_001,
         pct_age4564=sum(c(B01001_015,B01001_016,B01001_017,B01001_018,B01001_019,
                           B01001_039,B01001_040,B01001_041,B01001_042,B01001_043))/B01001_001,
         pct_age6574=sum(c(B01001_020,B01001_021,B01001_022,B01001_044,B01001_045,
                           B01001_046))/B01001_001,
         pct_age7584=sum(c(B01001_023,B01001_024,B01001_047,B01001_048))/B01001_001,
         pct_age85plus=sum(c(B01001_025,B01001_049))/B01001_001,
         pct_age65plus=pct_age6574+pct_age7584+pct_age85plus) %>% 
  select(zcta=GEOID, pct_age1844, pct_age4564,pct_age65plus)

zcta_data<-right_join(svi, pop) %>% 
  left_join(zcta_age) %>% 
  group_by(city) %>% 
  mutate(svi_sd=as.numeric(scale(svi, center=T, scale=T)),
         svi1_sd=as.numeric(scale(svi1, center=T, scale=T)),
         svi2_sd=as.numeric(scale(svi2, center=T, scale=T)),
         svi3_sd=as.numeric(scale(svi3, center=T, scale=T)),
         svi4_sd=as.numeric(scale(svi4, center=T, scale=T)),
         # 0 to 1 by city
         svi_std=(svi-min(svi, na.rm=T))/(max(svi, na.rm=T)-min(svi, na.rm=T)),
         svi1_std=(svi1-min(svi1, na.rm=T))/(max(svi1, na.rm=T)-min(svi1, na.rm=T)),
         svi2_std=(svi2-min(svi2, na.rm=T))/(max(svi2, na.rm=T)-min(svi2, na.rm=T)),
         svi3_std=(svi3-min(svi3, na.rm=T))/(max(svi3, na.rm=T)-min(svi3, na.rm=T)),
         svi4_std=(svi4-min(svi4, na.rm=T))/(max(svi4, na.rm=T)-min(svi4, na.rm=T))) %>% 
  ungroup() %>% 
  mutate(svi_std_quint=as.numeric(cut(svi_std, breaks=quantile(svi_std, probs=seq(0, 1, by=1/5),
                                                          na.rm=T), include.lowest=T)),
         svi_std_quint_f=factor(svi_std_quint, levels=c(1:5),
                            labels=c("Low", "Low-Medium", "Medium", "Medium-High", "High")),
         svi_quint=as.numeric(cut(svi, breaks=quantile(svi, probs=seq(0, 1, by=1/5),
                                                          na.rm=T), include.lowest=T)),
         svi_quint_f=factor(svi_quint, levels=c(1:5),
                            labels=c("Low", "Low-Medium", "Medium", "Medium-High", "High")))

# put everything together
all_vax<-inner_join(all_vax, zcta_data) %>% 
  mutate(fully_vaccinated=prop*total_pop,
         state=case_when(
           grepl("Beach|Angeles|Francisco|Diego|Jose|Oakland", city) ~ "CA",
           grepl("Austin|Dallas|Worth|Houston|Antonio", city) ~ "TX",
           grepl("Chicago", city) ~ "IL",
           grepl("Phila", city) ~ "PA",
           grepl("York", city) ~ "NY",
           grepl("Minnea", city) ~ "MN",
           grepl("Phoenix", city) ~ "AZ",
           grepl("Indiana", city) ~ "IN"),
         region=case_when(
           state %in% c("CA", "AZ") ~ "West",
           state %in% c("TX") ~ "South",
           state %in% c("IL","IN", "MN") ~ "Midwest",
           state %in% c("NY", "PA") ~ "Northeast"))
summary(all_vax)
table(all_vax$city, all_vax$state)
table(all_vax$city, all_vax$region)
all_vax %>% filter(is.na(fully_vaccinated))
all_vax %>% filter(is.na(pct_age65plus))
all_vax %>% filter(is.na(svi))
# all good
all_vax<-all_vax %>% filter(!is.na(fully_vaccinated), !is.na(svi)) %>% 
  mutate(fully_vaccinated_prop=fully_vaccinated/total_pop) %>% 
  mutate(city_state=paste0(city, ", ", state)) %>% 
  ungroup() %>% 
  mutate(region=factor(region, levels=c("South","Midwest",
                                        "West", "Northeast")))

# also get a few city things
city_values<-tidy_city_exposures %>% filter(outcome%in%c("pct_poverty","pop", "pct_nhwhite")) %>% 
  select(city, outcome, value) %>% 
  spread(outcome, value) %>% 
  mutate(pct_nonwhite=100-pct_nhwhite) %>% 
  select(city, pop, pct_nonwhite, pct_poverty)


save(all_vax, zcta_data, city_values, file="Data/Clean_Data.Rdata")

