library(formatR)
library(openxlsx)
library(dplyr)
library(plyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(zoo)
library(sf)
library(scales)
library(ggpubr)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(raster)
library(Hmisc)
library(plotly)
library(jtools)
library(huxtable)
library(officer)
library(flextable)


# Import datasets
crime_2019 <- read_csv(
  'D:/ROB/MICA/Spring 2021/MVIS 5301.01/Final Project/data/Crime_Incidents_in_2019.csv'
)
crime_2020 <- read_csv(
  'D:/ROB/MICA/Spring 2021/MVIS 5301.01/Final Project/data/Crime_Incidents_in_2020.csv'
)
crime_2021 <- read_csv(
  'D:/ROB/MICA/Spring 2021/MVIS 5301.01/Final Project/data/Crime_Incidents_in_2021.csv'
)
hh_income_2019 <- read_csv(
  'D:/ROB/MICA/Spring 2021/MVIS 5301.01/Final Project/data/ACS_2019_Median_Household_Income_Variables_Tract.csv'
)
demo_census_tract <- read_csv(
  'D:/ROB/MICA/Spring 2021/MVIS 5301.01/Final Project/data/ACS_Demographic_Characteristics_DC_Census_Tract.csv'
)


# Process HH Income 2019 Data
hh_income_2019 <- dplyr::rename(hh_income_2019, objectid = OBJECTID, 
                                geoid = GEOID, 
                                aland = ALAND, 
                                awater = AWATER, 
                                name = NAME, 
                                state = State, 
                                county = County, 
                                med_hh_income = B19049_001E, 
                                med_hh_income_under25 = B19049_002E, 
                                med_hh_income_25_44 = B19049_003E, 
                                med_hh_income_45_64 = B19049_004E, 
                                med_hh_income_65over = B19049_005E, 
                                med_hh_income_afr_am = B19013B_001E, 
                                med_hh_income_am_ind = B19013C_001E, 
                                med_hh_income_asian = B19013D_001E, 
                                med_hh_income_hi = B19013E_001E, 
                                med_hh_income_other_race = B19013F_001E, 
                                med_hh_income_2plus_race = B19013G_001E, 
                                med_hh_income_white_nonhispanic = B19013H_001E, 
                                med_hh_income_hispanice = B19013I_001E, 
                                total_hh = B19053_001E, 
                                hh_self_employment = B19053_002E, 
                                hh_no_self_employment = B19053_003E, 
                                pct_hh_any_self_employment = B19053_calc_pctSelfempE, 
                                shape_area = SHAPE_Length, 
                                shape_length = SHAPE_Area
)


hh_income_2019 <- select(hh_income_2019, 
                         objectid,
                         geoid,
                         aland,
                         awater,
                         name,
                         state,
                         county,
                         med_hh_income,
                         med_hh_income_under25,
                         med_hh_income_25_44,
                         med_hh_income_45_64,
                         med_hh_income_65over,
                         med_hh_income_afr_am,
                         med_hh_income_am_ind,
                         med_hh_income_asian,
                         med_hh_income_hi,
                         med_hh_income_other_race,
                         med_hh_income_2plus_race,
                         med_hh_income_white_nonhispanic,
                         med_hh_income_hispanice,
                         total_hh,
                         hh_self_employment,
                         hh_no_self_employment,
                         pct_hh_any_self_employment,
                         shape_area,
                         shape_length
)

# Create Census Tract Variable
hh_income_2019$census_tract <- hh_income_2019$geoid
hh_income_2019$census_tract <- str_sub(hh_income_2019$census_tract, -6, -1)

# Select just Census Tract and Med HH Income vars
census_tract_inc <- dplyr::select(hh_income_2019, census_tract,med_hh_income,total_hh)

# Process Demographics Dataset
demo_census_tract <- dplyr::rename(demo_census_tract,objectid = OBJECTID, 
                                   statefp = STATEFP, 
                                   countyfp = COUNTYFP, 
                                   census_tract = TRACTCE, 
                                   geoid = GEOID, 
                                   tract_name = NAME, 
                                   name_lsad = NAMELSAD, 
                                   mtfcc = MTFCC, 
                                   funcstat = FUNCSTAT, 
                                   aland = ALAND, 
                                   awater = AWATER, 
                                   intptlat = INTPTLAT, 
                                   intpltlon = INTPTLON, 
                                   sex_age_total_pop = DP05_0001E, 
                                   sex_age_male = DP05_0002E, 
                                   sex_age_female = DP05_0003E, 
                                   sex_age_sex_ratio = DP05_0004E, 
                                   age_under5 = DP05_0005E, 
                                   age_5_9 = DP05_0006E, 
                                   age_10_14 = DP05_0007E, 
                                   age_15_19 = DP05_0008E, 
                                   age_20_24 = DP05_0009E, 
                                   age_25_34 = DP05_0010E, 
                                   age_35_44 = DP05_0011E, 
                                   age_45_54 = DP05_0012E, 
                                   age_55_59 = DP05_0013E, 
                                   age_60_64 = DP05_0014E, 
                                   age_65_74 = DP05_0015E, 
                                   age_75_84 = DP05_0016E, 
                                   age_85over = DP05_0017E, 
                                   age_median = DP05_0018E, 
                                   age_under18 = DP05_0019E, 
                                   age_16over = DP05_0020E, 
                                   age_18over = DP05_0021E, 
                                   age_21over = DP05_0022E, 
                                   age_62over = DP05_0023E, 
                                   age_65over = DP05_0024E, 
                                   age_18over2 = DP05_0025E, 
                                   age_18over_male = DP05_0026E, 
                                   age_18over_female = DP05_0027E, 
                                   age_18over_sex_ratio = DP05_0028E, 
                                   age_65over2 = DP05_0029E, 
                                   age_65over_male = DP05_0030E, 
                                   age_65over_female = DP05_0031E, 
                                   age_65ovr_sex_ratio = DP05_0032E, 
                                   race_total_pop = DP05_0033E, 
                                   race_1_race = DP05_0034E, 
                                   race_2plus_race = DP05_0035E, 
                                   race_1_race2 = DP05_0036E, 
                                   race_1_white = DP05_0037E, 
                                   race_1_black = DP05_0038E, 
                                   race_1_am_ind = DP05_0039E, 
                                   race_1_am_ind_cherokee = DP05_0040E, 
                                   race_1_am_ind_chippewa = DP05_0041E, 
                                   race_1_am_ind_navajo = DP05_0042E, 
                                   race_1_am_ind_sioux = DP05_0043E, 
                                   race_1_asian = DP05_0044E, 
                                   race_1_asian_indian = DP05_0045E, 
                                   race_1_asian_chinese = DP05_0046E, 
                                   race_1_asian_filipino = DP05_0047E, 
                                   race_1_asian_japanese = DP05_0048E, 
                                   race_1_asian_korean = DP05_0049E, 
                                   race_1_asian_vietnamese = DP05_0050E, 
                                   race_1_asian_other = DP05_0051E, 
                                   race_1_hawaiian_pi = DP05_0052E, 
                                   race_1_hawaiian_pi_native_hi = DP05_0053E, 
                                   race_1_hawaiian_pi_guamanian = DP05_0054E, 
                                   race_1_hawaiian_pi_samoan = DP05_0055E, 
                                   race_1_hawaiian_pi_other = DP05_0056E, 
                                   race_1_other = DP05_0057E, 
                                   race_2plus_race2 = DP05_0058E, 
                                   race_2plus_white_black = DP05_0059E, 
                                   race_2plus_white_am_ind = DP05_0060E, 
                                   race_2plus_white_asian = DP05_0061E, 
                                   race_2plus_black_am_ind = DP05_0062E, 
                                   race_alone_combo_total_pop = DP05_0063E, 
                                   race_alone_combo_total_pop_white = DP05_0064E, 
                                   race_alone_combo_total_pop_black = DP05_0065E, 
                                   race_alone_combo_total_pop_am_ind = DP05_0066E, 
                                   race_alone_combo_total_pop_asian = DP05_0067E, 
                                   race_alone_combo_total_pop_hawaiian_pi = DP05_0068E, 
                                   race_alone_combo_total_pop_other = DP05_0069E, 
                                   hispanic_latino_total_pop = DP05_0070E, 
                                   hispanic_latino_any = DP05_0071E, 
                                   hispanic_latino_mexican = DP05_0072E, 
                                   hispanic_latino_puerto_rican = DP05_0073E, 
                                   hispanic_latino_cuban = DP05_0074E, 
                                   hispanic_latino_other = DP05_0075E, 
                                   not_hispanic_latino = DP05_0076E, 
                                   not_hispanic_latino_white = DP05_0077E, 
                                   not_hispanic_latino_black = DP05_0078E, 
                                   not_hispanic_latino_am_ind = DP05_0079E, 
                                   not_hispanic_latino_asian = DP05_0080E, 
                                   not_hispanic_latino_hawaiian_pi = DP05_0081E, 
                                   not_hispanic_latino_other = DP05_0082E, 
                                   not_hispanic_latino_2plus = DP05_0083E, 
                                   not_hispanic_latino_2plus_other_race = DP05_0084E, 
                                   not_hispanic_latino_3pus = DP05_0085E, 
                                   total_housing_units = DP05_0086E, 
                                   voting_age_18over = DP05_0087E, 
                                   voting_age_18over_male = DP05_0088E, 
                                   voting_age_18over_female = DP05_0089E, 
                                   shape_area = SHAPEAREA, 
                                   shape_len = SHAPELEN
)

# Calculate % race and population density by census tract

demo_census_tract2 <- dplyr::select(demo_census_tract,census_tract,aland,
                                    sex_age_sex_ratio,age_median,
                                    race_total_pop,race_1_race,race_2plus_race,not_hispanic_latino,not_hispanic_latino_white,
                                    not_hispanic_latino_black,not_hispanic_latino_am_ind,not_hispanic_latino_asian,
                                    not_hispanic_latino_hawaiian_pi,not_hispanic_latino_other)

demo_census_tract2 <- demo_census_tract2 %>% 
  mutate(
    pop_dens = race_total_pop / (aland*0.000621371),
    hispanic_latino_num = race_total_pop - not_hispanic_latino,
    pct_white = not_hispanic_latino_white/race_total_pop,
    pct_black = not_hispanic_latino_black/race_total_pop,
    pct_am_ind = not_hispanic_latino_am_ind/race_total_pop,
    pct_asian = not_hispanic_latino_asian/race_total_pop,
    pct_hawaiian_pi = not_hispanic_latino_hawaiian_pi/race_total_pop,
    pct_hispanic_latino = hispanic_latino_num/race_total_pop,
    pct_other = not_hispanic_latino_other / race_total_pop,
    pct_2_races = race_2plus_race / race_total_pop,
    sum_pct = pct_white + pct_black + pct_am_ind + pct_asian + pct_hawaiian_pi + pct_hispanic_latino + pct_other + pct_2_races
  )
demo_census_tract3 <- dplyr::select(demo_census_tract2,census_tract,pop_dens,sex_age_sex_ratio,age_median,pct_white,pct_black,pct_am_ind,pct_asian,
                                    pct_hawaiian_pi, pct_hispanic_latino,pct_other,pct_2_races)

demo_census_tract3 <- filter(demo_census_tract3, !is.na(demo_census_tract3$age_median))


# Process 2019, 2020, and 2021 Crime Datasets
crime_2019 <- dplyr::rename(crime_2019, x = X, 
                            y = Y, 
                            ccn = CCN, 
                            report_date_str = REPORT_DAT, 
                            shift = SHIFT, 
                            method = METHOD, 
                            offense = OFFENSE, 
                            block = BLOCK, 
                            xblock = XBLOCK, 
                            yblock = YBLOCK, 
                            ward = WARD, 
                            anc = ANC, 
                            district = DISTRICT, 
                            psa = PSA, 
                            neighborhood_cluster = NEIGHBORHOOD_CLUSTER, 
                            block_group = BLOCK_GROUP, 
                            census_tract = CENSUS_TRACT, 
                            voting_precinct = VOTING_PRECINCT, 
                            latitude = LATITUDE, 
                            longitude = LONGITUDE, 
                            bid = BID, 
                            start_date = START_DATE, 
                            end_date = END_DATE, 
                            objectid = OBJECTID, 
                            octo_record_id = OCTO_RECORD_ID
)

crime_2020 <- dplyr::rename(crime_2020, x = X, 
                            y = Y, 
                            ccn = CCN, 
                            report_date_str = REPORT_DAT, 
                            shift = SHIFT, 
                            method = METHOD, 
                            offense = OFFENSE, 
                            block = BLOCK, 
                            xblock = XBLOCK, 
                            yblock = YBLOCK, 
                            ward = WARD, 
                            anc = ANC, 
                            district = DISTRICT, 
                            psa = PSA, 
                            neighborhood_cluster = NEIGHBORHOOD_CLUSTER, 
                            block_group = BLOCK_GROUP, 
                            census_tract = CENSUS_TRACT, 
                            voting_precinct = VOTING_PRECINCT, 
                            latitude = LATITUDE, 
                            longitude = LONGITUDE, 
                            bid = BID, 
                            start_date = START_DATE, 
                            end_date = END_DATE, 
                            objectid = OBJECTID, 
                            octo_record_id = OCTO_RECORD_ID
)


crime_2021 <- dplyr::rename(crime_2021, x = X, 
                            y = Y, 
                            ccn = CCN, 
                            report_date_str = REPORT_DAT, 
                            shift = SHIFT, 
                            method = METHOD, 
                            offense = OFFENSE, 
                            block = BLOCK, 
                            xblock = XBLOCK, 
                            yblock = YBLOCK, 
                            ward = WARD, 
                            anc = ANC, 
                            district = DISTRICT, 
                            psa = PSA, 
                            neighborhood_cluster = NEIGHBORHOOD_CLUSTER, 
                            block_group = BLOCK_GROUP, 
                            census_tract = CENSUS_TRACT, 
                            voting_precinct = VOTING_PRECINCT, 
                            latitude = LATITUDE, 
                            longitude = LONGITUDE, 
                            bid = BID, 
                            start_date = START_DATE, 
                            end_date = END_DATE, 
                            objectid = OBJECTID, 
                            octo_record_id = OCTO_RECORD_ID
)

# Separate report date and report time from current report_date variable
crime_2019_clean <- crime_2019 %>% 
  separate(report_date_str, c("report_date","report_time")," ")

crime_2020_clean <- crime_2020 %>% 
  separate(report_date_str, c("report_date","report_time")," ")

crime_2021_clean <- crime_2021 %>% 
  separate(report_date_str, c("report_date","report_time")," ")

crime_2019_clean$report_time <- str_sub(crime_2019_clean$report_time, 1, 8)
crime_2020_clean$report_time <- str_sub(crime_2020_clean$report_time, 1, 8)
crime_2021_clean$report_time <- str_sub(crime_2021_clean$report_time, 1, 8)

# Convert to R date and time variables
crime_2019_clean$report_date <- ymd(crime_2019_clean$report_date)
crime_2019_clean$report_time <- hms(crime_2019_clean$report_time)

crime_2020_clean$report_date <- ymd(crime_2020_clean$report_date)
crime_2020_clean$report_time <- hms(crime_2020_clean$report_time)

crime_2021_clean$report_date <- ymd(crime_2021_clean$report_date)
crime_2021_clean$report_time <- hms(crime_2021_clean$report_time)

# Get report weekday, month, and hour for counts later
crime_2019_clean <- crime_2019_clean %>%  
  mutate(dow = wday(report_date,label=TRUE),
         mth = month(report_date),
         yr = year(report_date),
         hr = hour(report_time))

crime_2019_clean$yrmo <- as.Date(as.yearmon(paste(crime_2019_clean$yr, crime_2019_clean$mth), "%Y %m"))

crime_2020_clean <- crime_2020_clean %>%  
  mutate(dow = wday(report_date,label=TRUE),
         mth = month(report_date),
         yr = year(report_date),
         hr = hour(report_time))

crime_2020_clean$yrmo <- as.Date(as.yearmon(paste(crime_2020_clean$yr, crime_2020_clean$mth), "%Y %m"))

crime_2021_clean <- crime_2021_clean %>%  
  mutate(dow = wday(report_date,label=TRUE),
         mth = month(report_date),
         yr = year(report_date),
         hr = hour(report_time))


crime_2021_clean$yrmo <- as.Date(as.yearmon(paste(crime_2021_clean$yr, crime_2021_clean$mth), "%Y %m"))

# Stack all years
crime_all_clean <- rbind(crime_2019_clean, crime_2020_clean, crime_2021_clean)

# Aggregate 2019 for later merging

crime_2019_agg_geo_offense <- crime_2019_clean %>% 
  group_by(census_tract, offense) %>% 
  dplyr::summarise(
    crime_count = n()
  )

crime_2019_agg_geo <- crime_2019_clean %>% 
  group_by(census_tract, ward) %>% 
  dplyr::summarise(
    crime_count = n()
  )

crime_2019_agg_census_tract <- crime_2019_clean %>% 
  group_by(census_tract) %>% 
  dplyr::summarise(
    crime_count = n()
  )


# Merge HH Income and Demographics

df_merged <- merge(census_tract_inc, demo_census_tract3, by.x=c("census_tract"), by.y=c("census_tract"), sort = FALSE)


# Merge df_merged to aggregated 2019 crime datasets
df_merged_geo_offense <- merge(crime_2019_agg_geo_offense, df_merged, by.x=c("census_tract"), by.y=c("census_tract"), sort = FALSE)
df_merged_geo_offense_nona <- filter(df_merged_geo_offense, !is.na(df_merged_geo_offense$med_hh_income))

df_merged_geo <- merge(crime_2019_agg_geo, df_merged, by.x=c("census_tract"), by.y=c("census_tract"), sort = FALSE)
df_merged_geo_nona <- filter(df_merged_geo, !is.na(df_merged_geo$med_hh_income))

df_merged_census_tract <- merge(crime_2019_agg_census_tract, df_merged, by.x=c("census_tract"), by.y=c("census_tract"), sort = FALSE)
df_merged_census_tract_nona <- filter(df_merged_census_tract, !is.na(df_merged_census_tract$med_hh_income))

df_report_date <- crime_2019_clean %>%
  group_by(report_date) %>%
  dplyr::summarise(
    crime_count = n()
  )

# EXPLORATORY DATA ANALYSIS - Crime 2019 Dataset

# Report Date
df_report_date_chart <- plot_ly(df_report_date, x = ~report_date, y = ~crime_count, type = 'bar')
df_report_date_chart <- df_report_date_chart %>% 
  layout(title = "Daily Reported Crime Counts \n Jan-Dec 2019",
         yaxis = list(title = 'Reported Crime Count'),
         xaxis = list(type = 'date', title = 'Date'))

df_report_date_chart

# Report Day of Week
df_report_dow <- crime_2019_clean %>%
  group_by(dow) %>%
  dplyr::summarise(
    crime_count = n()
  )

df_report_dow_chart <- plot_ly(df_report_dow, x = ~dow, y = ~crime_count, type = 'bar')
df_report_dow_chart <- df_report_dow_chart %>% 
  layout(title = "Reported Crime Counts by Day of the Week \n 2019",
         yaxis = list(title = 'Reported Crime Count'),
         xaxis = list(title = 'Day of the Week'))

df_report_dow_chart

df_report_dow2 <- crime_2020_clean %>%
  group_by(dow) %>%
  dplyr::summarise(
    crime_count = n()
  )

df_report_dow_chart2 <- plot_ly(df_report_dow2, x = ~dow, y = ~crime_count, type = 'bar')
df_report_dow_chart2 <- df_report_dow_chart2 %>% 
  layout(title = "Reported Crime Counts by Day of the Week \n 2020",
         yaxis = list(title = 'Reported Crime Count'),
         xaxis = list(title = 'Day of the Week'))

df_report_dow_chart2

# Report Time of Day
df_report_hr <- crime_2019_clean %>%
  group_by(hr) %>%
  dplyr::summarise(
    crime_count = n()
  )

df_report_hr_chart <- plot_ly(df_report_hr, x = ~hr, y = ~crime_count, type = 'bar')
df_report_hr_chart <- df_report_hr_chart %>% 
  layout(title = "Reported Crime Counts by Hour \n 2019",
         yaxis = list(title = 'Reported Crime Count'),
         xaxis = list(title = 'Hour'))

df_report_hr_chart

df_report_hr2 <- crime_2020_clean %>%
  group_by(hr) %>%
  dplyr::summarise(
    crime_count = n()
  )

df_report_hr_chart2 <- plot_ly(df_report_hr2, x = ~hr, y = ~crime_count, type = 'bar')
df_report_hr_chart2 <- df_report_hr_chart2 %>% 
  layout(title = "Reported Crime Counts by Hour \n 2020",
         yaxis = list(title = 'Reported Crime Count'),
         xaxis = list(title = 'Hour'))

df_report_hr_chart2

# Shift
df_report_shift <- crime_2019_clean %>%
  group_by(shift) %>%
  dplyr::summarise(
    crime_count = n()
  )

df_report_shift_chart <- plot_ly(df_report_shift, x = ~shift, y = ~crime_count, type = 'bar')
df_report_shift_chart <- df_report_shift_chart %>% 
  layout(title = "Reported Crime Counts by Shift \n 2019",
         yaxis = list(title = 'Reported Crime Count'),
         xaxis = list(title = 'Shift'))

df_report_shift_chart

# Method

df_report_method <- crime_2019_clean %>%
  group_by(method) %>%
  dplyr::summarise(
    crime_count = n()
  )

df_report_method_chart <- plot_ly(df_report_method, x = ~method, y = ~crime_count, type = 'bar')
df_report_method_chart <- df_report_method_chart %>% 
  layout(title = "Reported Crime Counts by Method \n 2019",
         yaxis = list(title = 'Reported Crime Count'),
         xaxis = list(title = 'Method'))

df_report_method_chart

# Offense
df_report_offense <- crime_2019_clean %>%
  group_by(offense) %>%
  dplyr::summarise(
    crime_count = n()
  )

df_report_offense_chart <- plot_ly(df_report_offense, x = ~offense, y = ~crime_count, type = 'bar')
df_report_offense_chart <- df_report_offense_chart %>% 
  layout(title = "Reported Crime Counts by offense \n 2019",
         yaxis = list(title = 'Reported Crime Count'),
         xaxis = list(title = 'Offense'))

df_report_offense_chart

df_report_offense2 <- crime_2020_clean %>%
  group_by(offense) %>%
  dplyr::summarise(
    crime_count = n()
  )

df_report_offense2_chart <- plot_ly(df_report_offense2, x = ~offense, y = ~crime_count, type = 'bar')
df_report_offense2_chart <- df_report_offense2_chart %>% 
  layout(title = "Reported Crime Counts by offense2 \n 2020",
         yaxis = list(title = 'Reported Crime Count'),
         xaxis = list(title = 'Offense'))

df_report_offense2_chart

# Ward
df_report_ward <- crime_2019_clean %>%
  group_by(ward) %>%
  dplyr::summarise(
    crime_count = n()
  )

df_report_ward_chart <- plot_ly(df_report_ward, x = ~ward, y = ~crime_count, type = 'bar')
df_report_ward_chart <- df_report_ward_chart %>% 
  layout(title = "Reported Crime Counts by Ward \n 2019",
         yaxis = list(title = 'Reported Crime Count'),
         xaxis = list(title = 'Ward'))

df_report_ward_chart

wtd_avg_hh_income_ward <- df_merged_geo_nona %>% 
  group_by(ward) %>% 
  dplyr::summarise(wtd_avg_hh_income_ward = weighted.mean(med_hh_income,total_hh))

df_report_wardinc_chart <- plot_ly(wtd_avg_hh_income_ward, x = ~ward, y = ~wtd_avg_hh_income_ward, type = 'bar')
df_report_wardinc_chart <- df_report_wardinc_chart %>% 
  layout(title = "Reported Crime Counts by Ward \n 2019",
         yaxis = list(title = 'Weighted Average HH Income'),
         xaxis = list(title = 'Ward'))

df_report_wardinc_chart

# Census Tract
df_report_census_tract <- crime_2019_clean %>%
  group_by(census_tract) %>%
  dplyr::summarise(
    crime_count = n()
  )

df_report_census_tract_chart <- plot_ly(df_report_census_tract, x = ~census_tract, y = ~crime_count, type = 'bar')
df_report_census_tract_chart <- df_report_census_tract_chart %>% 
  layout(title = "Reported Crime Counts by Census_Tract \n 2019",
         yaxis = list(title = 'Reported Crime Count'),
         xaxis = list(title = 'Census Tract'))

df_report_census_tract_chart

# EXPLORATORY DATA ANALYSIS - Median Household Income and Demographics

# Median Household Income
med_hh_income_chart <- plot_ly(df_merged, x = ~med_hh_income, type = 'histogram', bingroup=10000)
med_hh_income_chart <- med_hh_income_chart %>%
  layout(title = "Median HH Income Distribution")

med_hh_income_chart

boxplot(df_merged$med_hh_income)

# Population Density (Number of People per Square Mile)
pop_dens_chart <- plot_ly(df_merged, x = ~pop_dens, type = 'histogram', bingroup=1)
pop_dens_chart <- pop_dens_chart %>%
  layout(title = "Population Density (# of People per Sq Mi) Distribution")

pop_dens_chart

boxplot(df_merged$pop_dens)

# Sex Ratio (number of males per 100 females)
sex_ratio_chart <- plot_ly(df_merged, x = ~sex_age_sex_ratio, type = 'histogram', bingroup=1)
sex_ratio_chart <- sex_ratio_chart %>%
  layout(title = "Sex Ratio (# of Males per 100 Females) Distribution")

sex_ratio_chart

boxplot(df_merged$sex_age_sex_ratio)

df_merged_no006804<- filter(df_merged, census_tract != "006804")
sex_ratio_chart2 <- plot_ly(df_merged_no006804, x = ~sex_age_sex_ratio, type = 'histogram', bingroup=1)
sex_ratio_chart2 <- sex_ratio_chart2 %>%
  layout(title = "Sex Ratio (# of Males per 100 Females) Distribution")

sex_ratio_chart2

boxplot(df_merged_no006804$sex_age_sex_ratio)

# Median Age
age_median_chart <- plot_ly(df_merged, x = ~age_median, type = 'histogram', bingroup=1)
age_median_chart <- age_median_chart %>%
  layout(title = "Median Age Distribution")

age_median_chart

boxplot(df_merged$age_median)

# % of each race recorded in the Census
boxplot(df_merged$pct_white,
        df_merged$pct_black,
        df_merged$pct_asian,
        df_merged$pct_hawaiian_pi,
        df_merged$pct_hispanic_latino,
        df_merged$pct_2_races,
        df_merged$pct_other)
# 1 = % White, 2 = % Black, 3 = % Asian, 4 = % Hawaiian/Pacific Islander, 5 = Hispanic/Latino, 6 = 2 or more Races, 7 = Other Race


pct_white_chart <- plot_ly(df_merged, x = ~pct_white, type = 'histogram', bingroup=.1)
pct_white_chart <- pct_white_chart %>%
  layout(title = "% White Distribution")
pct_white_chart
boxplot(df_merged$pct_white)

pct_black_chart <- plot_ly(df_merged, x = ~pct_black, type = 'histogram', bingroup=.1)
pct_black_chart <- pct_black_chart %>%
  layout(title = "% Black Distribution")
pct_black_chart
boxplot(df_merged$pct_black)

pct_asian_chart <- plot_ly(df_merged, x = ~pct_asian, type = 'histogram', bingroup=.1)
pct_asian_chart <- pct_asian_chart %>%
  layout(title = "% Asian Distribution")
pct_asian_chart
boxplot(df_merged$pct_asian)

pct_hawaiian_pi_chart <- plot_ly(df_merged, x = ~pct_hawaiian_pi, type = 'histogram', bingroup=.05)
pct_hawaiian_pi_chart <- pct_hawaiian_pi_chart %>%
  layout(title = "% Hawaiian or Pacific Islander Distribution")
pct_hawaiian_pi_chart
boxplot(df_merged$pct_hawaiian_pi)

pct_hispanic_latino_chart <- plot_ly(df_merged, x = ~pct_hispanic_latino, type = 'histogram', bingroup=.1)
pct_hispanic_latino_chart <- pct_hispanic_latino_chart %>%
  layout(title = "% Hispanic or Latino Distribution")
pct_hispanic_latino_chart
boxplot(df_merged$pct_hispanic_latino)

pct_2_races_chart <- plot_ly(df_merged, x = ~pct_2_races, type = 'histogram', bingroup=.1)
pct_2_races_chart <- pct_2_races_chart %>%
  layout(title = "% 2 or More Races Distribution")
pct_2_races_chart
boxplot(df_merged$pct_2_races)

pct_other_chart <- plot_ly(df_merged, x = ~pct_other, type = 'histogram', bingroup=.1)
pct_other_chart <- pct_other_chart %>%
  layout(title = "% Other Race Distribution")
pct_other_chart
boxplot(df_merged$pct_other)



# EXPLORATORY DATA ANALYSIS - Pre vs. Post Period
# Look at the effect of the pandemic on crime counts by separating the data from a 12-month "pre-period" (March 2019 - February 2020) and a 12-month "post-period" (March 2020 - February 2021).
crime_all_clean_counts <- crime_all_clean %>%
  group_by(yrmo) %>%
  dplyr::summarise(
    crime_count = n()
  )

counts_chart_all <- plot_ly(crime_all_clean_counts, x = ~yrmo, y = ~crime_count, type = 'bar')
counts_chart_all <- counts_chart_all %>% 
  layout(title = "Monthly Reported Crime Counts \n Jan 2019 - Apr 2021",
         yaxis = list(title = 'Reported Crime Count'),
         xaxis = list(type = 'date', tickformat = "%b %Y", title = 'Month'))

counts_chart_all

# Look at average monthly mean in the pre period vs. post period across all offenses
prepost_period_crime <- filter(crime_all_clean, report_date >= as.Date("2019-03-01"), report_date <= as.Date("2021-02-28"))

prepost_period_crime_counts <- prepost_period_crime %>%
  group_by(yrmo) %>%
  dplyr::summarise(
    crime_count = n()
  )

counts_chart_prepost <- plot_ly(prepost_period_crime_counts, x = ~yrmo, y = ~crime_count, type = 'bar')
counts_chart_prepost <- counts_chart_prepost %>% 
  layout(title = "Monthly Reported Crime Counts \n Mar 2019 - Feb 2021",
         yaxis = list(title = 'Reported Crime Count'),
         xaxis = list(type = 'date', tickformat = "%b %Y", title = 'Month'))

counts_chart_prepost

pre_period_crime <- filter(crime_all_clean, report_date >= as.Date("2019-03-01"), report_date <= as.Date("2020-02-29"))

post_period_crime <- filter(crime_all_clean, report_date >= as.Date("2020-03-01"), report_date <= as.Date("2021-02-28"))

#Calculate Monthly Crime Counts

pre_period_crime_counts <- pre_period_crime %>% 
  group_by(yrmo) %>% 
  dplyr::summarise(
    crime_count = n()
  )

post_period_crime_counts <- post_period_crime %>% 
  group_by(yrmo) %>% 
  dplyr::summarise(
    crime_count = n()
  )

boxplot(pre_period_crime_counts$crime_count,post_period_crime_counts$crime_count)

#T-Test
t.test(pre_period_crime_counts$crime_count,post_period_crime_counts$crime_count, mu = 0, conf.level = 0.95, alternative = "two.sided")

# Look at average monthly mean in the pre period vs. post period by each offense type
pre_period_crime_offense_counts <- pre_period_crime %>% 
  group_by(yrmo, offense) %>% 
  dplyr::summarise(
    crime_count = n()
  )

pre_period_crime_offense_counts_merge <- pre_period_crime_offense_counts %>%
  mutate(month = month(yrmo)) %>%
  dplyr::rename(
    crime_count_pre = crime_count
  )
pre_period_crime_offense_counts_merge <- subset(pre_period_crime_offense_counts_merge, select = -c(yrmo))


post_period_crime_offense_counts <- post_period_crime %>% 
  group_by(yrmo, offense) %>% 
  dplyr::summarise(
    crime_count = n()
  )

post_period_crime_offense_counts_merge <- post_period_crime_offense_counts %>%
  mutate(month = month(yrmo)) %>%
  dplyr::rename(
    crime_count_post = crime_count
  )
post_period_crime_offense_counts_merge <- subset(post_period_crime_offense_counts_merge, select = -c(yrmo))


# Join
pre_post_crime_offense_counts_merge <- merge(pre_period_crime_offense_counts_merge, post_period_crime_offense_counts_merge, by.x=c("month", "offense"), by.y=c("month", "offense"), sort = FALSE)

offense_list <- unique(unlist(pre_post_crime_offense_counts_merge$offense))

for(i in offense_list) {
  print(i)
}

for (i in offense_list) {
  # sub_data <- subset(pre_post_crime_offense_counts_merge, date == uniq[i])
  print(i)
  sub_data <- pre_post_crime_offense_counts_merge %>% 
    filter(offense == i)
  
  boxplot(sub_data$crime_count_pre,sub_data$crime_count_post)
  print(t.test(sub_data$crime_count_pre,sub_data$crime_count_post, mu = 0, conf.level = 0.95, alternative = "two.sided"))
  
}

# Explore statistics and relationships - Census Tract Level

# Print quantiles of each variable
print("Crime Count")
print(quantile(df_merged_census_tract_nona$crime_count))

print("Median HH Income")
print(quantile(df_merged_census_tract_nona$med_hh_income))

print("Population Density")
print(quantile(df_merged_census_tract_nona$pop_dens))

print("Sex Ratio")
print(quantile(df_merged_census_tract_nona$sex_age_sex_ratio))

print("Median Age")
print(quantile(df_merged_census_tract_nona$age_median))

print("% White")
print(quantile(df_merged_census_tract_nona$pct_white))

print("% Black")
print(quantile(df_merged_census_tract_nona$pct_black))

print("% American Indian or Alaska Native")
print(quantile(df_merged_census_tract_nona$pct_am_ind))

print("% Hispanic or Latino")
print(quantile(df_merged_census_tract_nona$pct_hispanic_latino))

print("% Asian")
print(quantile(df_merged_census_tract_nona$pct_asian))

print("% Other Race")
print(quantile(df_merged_census_tract_nona$pct_other))

print("% 2 or More Races")
print(quantile(df_merged_census_tract_nona$pct_2_races))

# Create correlation matrix at the Census Tract level
df_merged_census_tract_nona_cor <-  dplyr::select(df_merged_census_tract_nona,-c("census_tract","total_hh"))
my_cor <- rcorr(as.matrix(df_merged_census_tract_nona_cor))
my_cor

# Run linear regressions at the Census Tract level, adding one explanatory variable at a time
print("Crime Count = Median HH Income")
lm_ct1 <- lm(df_merged_census_tract_nona$crime_count ~ df_merged_census_tract_nona$med_hh_income)

summ(lm_ct1)
# 
# lm1_coeff <- data.frame(summ(lm1)$coefficients)
# lm1_coeff <- select(lm1_coeff)
# lm1_adjr2 <- data.frame(summary(lm1)$adj.r.squared)
# lm1_adjr2$var <- "Adjusted R2"

print("Crime Count = Median HH Income + Population Density")
lm_ct2 <- lm(df_merged_census_tract_nona$crime_count ~ df_merged_census_tract_nona$med_hh_income 
           + df_merged_census_tract_nona$pop_dens
)

summ(lm_ct2)

print("Crime Count = Median HH Income + Population Density + Sex Ratio")
lm_ct3 <- lm(df_merged_census_tract_nona$crime_count ~ df_merged_census_tract_nona$med_hh_income 
           + df_merged_census_tract_nona$pop_dens 
           + df_merged_census_tract_nona$sex_age_sex_ratio
)

summ(lm_ct3)

print("Crime Count = Median HH Income + Population Density + Sex Ratio + Median Age")
lm_ct4 <- lm(df_merged_census_tract_nona$crime_count ~ df_merged_census_tract_nona$med_hh_income 
           + df_merged_census_tract_nona$pop_dens 
           + df_merged_census_tract_nona$sex_age_sex_ratio
           + df_merged_census_tract_nona$age_median
)

summ(lm_ct4)

print("Crime Count = Median HH Income + Population Density + Sex Ratio + Median Age + % Asian")
lm_ct5 <- lm(df_merged_census_tract_nona$crime_count ~ df_merged_census_tract_nona$med_hh_income 
           + df_merged_census_tract_nona$pop_dens 
           + df_merged_census_tract_nona$sex_age_sex_ratio
           + df_merged_census_tract_nona$age_median
           + df_merged_census_tract_nona$pct_asian
)

summ(lm_ct5)

print("Crime Count = Median HH Income + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander")
lm_ct6 <- lm(df_merged_census_tract_nona$crime_count ~ df_merged_census_tract_nona$med_hh_income 
           + df_merged_census_tract_nona$pop_dens 
           + df_merged_census_tract_nona$sex_age_sex_ratio
           + df_merged_census_tract_nona$age_median
           + df_merged_census_tract_nona$pct_asian
           + df_merged_census_tract_nona$pct_hawaiian_pi
)

summ(lm_ct6)

print("Crime Count = Median HH Income + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander + % White")
lm_ct7 <- lm(df_merged_census_tract_nona$crime_count ~ df_merged_census_tract_nona$med_hh_income 
           + df_merged_census_tract_nona$pop_dens 
           + df_merged_census_tract_nona$sex_age_sex_ratio
           + df_merged_census_tract_nona$age_median
           + df_merged_census_tract_nona$pct_asian
           + df_merged_census_tract_nona$pct_hawaiian_pi
           + df_merged_census_tract_nona$pct_white
)

summ(lm_ct7)

print("Crime Count = Median HH Income + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander + % White + % Black")
lm_ct8 <- lm(df_merged_census_tract_nona$crime_count ~ df_merged_census_tract_nona$med_hh_income 
           + df_merged_census_tract_nona$pop_dens 
           + df_merged_census_tract_nona$sex_age_sex_ratio
           + df_merged_census_tract_nona$age_median
           + df_merged_census_tract_nona$pct_asian
           + df_merged_census_tract_nona$pct_hawaiian_pi
           + df_merged_census_tract_nona$pct_white
           + df_merged_census_tract_nona$pct_black
)
summ(lm_ct8)

print("Crime Count = Median HH Income + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander + % White + % Black + % Hispanic or Latino")
lm_ct9 <- lm(df_merged_census_tract_nona$crime_count ~ df_merged_census_tract_nona$med_hh_income 
           + df_merged_census_tract_nona$pop_dens 
           + df_merged_census_tract_nona$sex_age_sex_ratio
           + df_merged_census_tract_nona$age_median
           + df_merged_census_tract_nona$pct_asian
           + df_merged_census_tract_nona$pct_hawaiian_pi
           + df_merged_census_tract_nona$pct_white
           + df_merged_census_tract_nona$pct_black
           + df_merged_census_tract_nona$pct_hispanic_latino
)
summ(lm_ct9)

print("Crime Count = Median HH Income + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander + % White + % Black + % Hispanic or Latino + % American Indian or Alaska Native")
lm_ct10 <- lm(df_merged_census_tract_nona$crime_count ~ df_merged_census_tract_nona$med_hh_income 
           + df_merged_census_tract_nona$pop_dens 
           + df_merged_census_tract_nona$sex_age_sex_ratio
           + df_merged_census_tract_nona$age_median
           + df_merged_census_tract_nona$pct_asian
           + df_merged_census_tract_nona$pct_hawaiian_pi
           + df_merged_census_tract_nona$pct_white
           + df_merged_census_tract_nona$pct_black
           + df_merged_census_tract_nona$pct_hispanic_latino
           + df_merged_census_tract_nona$pct_am_ind
)

summ(lm_ct10)

print("Crime Count = Median HH Income + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander + % White + % Black + % Hispanic or Latino + % American Indian or Alaska Native + % Other Race")
lm_ct11 <- lm(df_merged_census_tract_nona$crime_count ~ df_merged_census_tract_nona$med_hh_income 
           + df_merged_census_tract_nona$pop_dens 
           + df_merged_census_tract_nona$sex_age_sex_ratio
           + df_merged_census_tract_nona$age_median
           + df_merged_census_tract_nona$pct_asian
           + df_merged_census_tract_nona$pct_hawaiian_pi
           + df_merged_census_tract_nona$pct_white
           + df_merged_census_tract_nona$pct_black
           + df_merged_census_tract_nona$pct_hispanic_latino
           + df_merged_census_tract_nona$pct_am_ind
           + df_merged_census_tract_nona$pct_other
)

summ(lm_ct11)

print("Crime Count = Median HH Income + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander + % White + % Black + % Hispanic or Latino + % American Indian or Alaska Native + % Other Race + % 2 or More Races")
lm_ct12 <- lm(df_merged_census_tract_nona$crime_count ~ df_merged_census_tract_nona$med_hh_income 
           + df_merged_census_tract_nona$pop_dens 
           + df_merged_census_tract_nona$sex_age_sex_ratio
           + df_merged_census_tract_nona$age_median
           + df_merged_census_tract_nona$pct_asian
           + df_merged_census_tract_nona$pct_hawaiian_pi
           + df_merged_census_tract_nona$pct_white
           + df_merged_census_tract_nona$pct_black
           + df_merged_census_tract_nona$pct_hispanic_latino
           + df_merged_census_tract_nona$pct_am_ind
           + df_merged_census_tract_nona$pct_other
           + df_merged_census_tract_nona$pct_2_races
)

summ(lm_ct12)

coefs_names <- c("Constant" = "(Intercept)"
                 , "Median HH Income" = "df_merged_census_tract_nona$med_hh_income"
                 , "Population Density" = "df_merged_census_tract_nona$pop_dens"
                 , "Sex Ratio" = "df_merged_census_tract_nona$sex_age_sex_ratio"
                 , "Median Age" = "df_merged_census_tract_nona$age_median"
                 , "% Asian" = "df_merged_census_tract_nona$pct_asian"
                 , "% Hawaiian/Pacific Islander" = "df_merged_census_tract_nona$pct_hawaiian_pi"
                 , "% White" = "df_merged_census_tract_nona$pct_white"
                 , "% Black/African American" = "df_merged_census_tract_nona$pct_black"
                 , "% Hispanic/Latino" = "df_merged_census_tract_nona$pct_hispanic_latino"
                 , "% American Indian/Alaska Native" = "df_merged_census_tract_nona$pct_am_ind"
                 , "% Other Race" = "df_merged_census_tract_nona$pct_other"
                 , "% 2 or More Races"= "df_merged_census_tract_nona$pct_2_races")

export_summs(lm_ct1, lm_ct2, lm_ct3, lm_ct4, lm_ct5, lm_ct6, lm_ct7, lm_ct8, lm_ct9, lm_ct10, lm_ct11, lm_ct12
             , coefs = coefs_names
             , error_format = "p = {p.value}"
             , statistics = c(N = "nobs", AdjR2 = "adj.r.squared")
             , to.file = "xlsx"
             , file.name = "D:/ROB/MICA/Spring 2021/MVIS 5301.01/Final Project/Submitted Files/output/Regression Outputs_Census Tract Level.xlsx")

# Census Tract-Offense Level
# Print quantiles of each variable
("Crime Count")
print(quantile(df_merged_geo_offense_nona$crime_count))

print("Median HH Income")
print(quantile(df_merged_geo_offense_nona$med_hh_income))

print("Population Density")
print(quantile(df_merged_geo_offense_nona$pop_dens))

print("Sex Ratio")
print(quantile(df_merged_geo_offense_nona$sex_age_sex_ratio))

print("Median Age")
print(quantile(df_merged_geo_offense_nona$age_median))

print("% White")
print(quantile(df_merged_geo_offense_nona$pct_white))

print("% Black")
print(quantile(df_merged_geo_offense_nona$pct_black))

print("% American Indian or Alaska Native")
print(quantile(df_merged_geo_offense_nona$pct_am_ind))

print("% Hispanic or Latino")
print(quantile(df_merged_geo_offense_nona$pct_hispanic_latino))

print("% Asian")
print(quantile(df_merged_geo_offense_nona$pct_asian))

print("% Other Race")
print(quantile(df_merged_geo_offense_nona$pct_other))

print("% 2 or More Races")
print(quantile(df_merged_geo_offense_nona$pct_2_races))

# Create correlation matrix at the Census Tract-Offense level
df_merged_geo_offense_nona_cor <-  dplyr::select(df_merged_geo_offense_nona,-c("census_tract","total_hh","offense"))
my_cor2 <- rcorr(as.matrix(df_merged_geo_offense_nona_cor))
my_cor2

# Run linear regressions at the Census Tract-Offense level, adding one explanatory variable at a time
print("Crime Count = Median HH Income")
lm_ct_offense1 <- lm(df_merged_geo_offense_nona$crime_count ~ df_merged_geo_offense_nona$med_hh_income)

summ(lm_ct_offense1)

print("Crime Count = Median HH Income + Offense Type")
lm_ct_offense2 <- lm(df_merged_geo_offense_nona$crime_count ~ df_merged_geo_offense_nona$med_hh_income
           + df_merged_geo_offense_nona$offense)

summ(lm_ct_offense2)

print("Crime Count = Median HH Income + Offense Type + Population Density")
lm_ct_offense3 <- lm(df_merged_geo_offense_nona$crime_count ~ df_merged_geo_offense_nona$med_hh_income
           + df_merged_geo_offense_nona$offense
           + df_merged_geo_offense_nona$pop_dens
)

summ(lm_ct_offense3)

print("Crime Count = Median HH Income + Offense Type + Population Density + Sex Ratio")
lm_ct_offense4 <- lm(df_merged_geo_offense_nona$crime_count ~ df_merged_geo_offense_nona$med_hh_income
           + df_merged_geo_offense_nona$offense
           + df_merged_geo_offense_nona$pop_dens 
           + df_merged_geo_offense_nona$sex_age_sex_ratio
)

summ(lm_ct_offense4)

print("Crime Count = Median HH Income + Offense Type + Population Density + Sex Ratio + Median Age")
lm_ct_offense5 <- lm(df_merged_geo_offense_nona$crime_count ~ df_merged_geo_offense_nona$med_hh_income 
           + df_merged_geo_offense_nona$offense
           + df_merged_geo_offense_nona$pop_dens 
           + df_merged_geo_offense_nona$sex_age_sex_ratio
           + df_merged_geo_offense_nona$age_median
)
summ(lm_ct_offense5)

print("Crime Count = Median HH Income + Offense Type + Population Density + Sex Ratio + Median Age + % Asian")
lm_ct_offense6 <- lm(df_merged_geo_offense_nona$crime_count ~ df_merged_geo_offense_nona$med_hh_income 
           + df_merged_geo_offense_nona$offense
           + df_merged_geo_offense_nona$pop_dens 
           + df_merged_geo_offense_nona$sex_age_sex_ratio
           + df_merged_geo_offense_nona$age_median
           + df_merged_geo_offense_nona$pct_asian
)
summ(lm_ct_offense6)

print("Crime Count = Median HH Income + Offense Type + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander")
lm_ct_offense7 <- lm(df_merged_geo_offense_nona$crime_count ~ df_merged_geo_offense_nona$med_hh_income
           + df_merged_geo_offense_nona$offense
           + df_merged_geo_offense_nona$pop_dens 
           + df_merged_geo_offense_nona$sex_age_sex_ratio
           + df_merged_geo_offense_nona$age_median
           + df_merged_geo_offense_nona$pct_asian
           + df_merged_geo_offense_nona$pct_hawaiian_pi
)
summ(lm_ct_offense7)

print("Crime Count = Median HH Income + Offense Type + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander + % White")
lm_ct_offense8 <- lm(df_merged_geo_offense_nona$crime_count ~ df_merged_geo_offense_nona$med_hh_income 
           + df_merged_geo_offense_nona$offense
           + df_merged_geo_offense_nona$pop_dens 
           + df_merged_geo_offense_nona$sex_age_sex_ratio
           + df_merged_geo_offense_nona$age_median
           + df_merged_geo_offense_nona$pct_asian
           + df_merged_geo_offense_nona$pct_hawaiian_pi
           + df_merged_geo_offense_nona$pct_white
)
summ(lm_ct_offense8)


print("Crime Count = Median HH Income + Offense Type + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander + % White + % Black")
lm_ct_offense9 <- lm(df_merged_geo_offense_nona$crime_count ~ df_merged_geo_offense_nona$med_hh_income 
           + df_merged_geo_offense_nona$offense
           + df_merged_geo_offense_nona$pop_dens 
           + df_merged_geo_offense_nona$sex_age_sex_ratio
           + df_merged_geo_offense_nona$age_median
           + df_merged_geo_offense_nona$pct_asian
           + df_merged_geo_offense_nona$pct_hawaiian_pi
           + df_merged_geo_offense_nona$pct_white
           + df_merged_geo_offense_nona$pct_black
)
summ(lm_ct_offense9)

print("Crime Count = Median HH Income + Offense Type + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander + % White + % Black + % Hispanic or Latino")
lm_ct_offense10 <- lm(df_merged_geo_offense_nona$crime_count ~ df_merged_geo_offense_nona$med_hh_income
           + df_merged_geo_offense_nona$offense
           + df_merged_geo_offense_nona$pop_dens 
           + df_merged_geo_offense_nona$sex_age_sex_ratio
           + df_merged_geo_offense_nona$age_median
           + df_merged_geo_offense_nona$pct_asian
           + df_merged_geo_offense_nona$pct_hawaiian_pi
           + df_merged_geo_offense_nona$pct_white
           + df_merged_geo_offense_nona$pct_black
           + df_merged_geo_offense_nona$pct_hispanic_latino
)
summ(lm_ct_offense10)

print("Crime Count = Median HH Income + Offense Type + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander + % White + % Black + % Hispanic or Latino + % American Indian or Alaska Native")
lm_ct_offense11 <- lm(df_merged_geo_offense_nona$crime_count ~ df_merged_geo_offense_nona$med_hh_income 
           + df_merged_geo_offense_nona$offense
           + df_merged_geo_offense_nona$pop_dens 
           + df_merged_geo_offense_nona$sex_age_sex_ratio
           + df_merged_geo_offense_nona$age_median
           + df_merged_geo_offense_nona$pct_asian
           + df_merged_geo_offense_nona$pct_hawaiian_pi
           + df_merged_geo_offense_nona$pct_white
           + df_merged_geo_offense_nona$pct_black
           + df_merged_geo_offense_nona$pct_hispanic_latino
           + df_merged_geo_offense_nona$pct_am_ind
)

summ(lm_ct_offense11)

print("Crime Count = Median HH Income + Offense Type + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander + % White + % Black + % Hispanic or Latino + % American Indian or Alaska Native + % Other Race")
lm_ct_offense12 <- lm(df_merged_geo_offense_nona$crime_count ~ df_merged_geo_offense_nona$med_hh_income 
           + df_merged_geo_offense_nona$offense
           + df_merged_geo_offense_nona$pop_dens 
           + df_merged_geo_offense_nona$sex_age_sex_ratio
           + df_merged_geo_offense_nona$age_median
           + df_merged_geo_offense_nona$pct_asian
           + df_merged_geo_offense_nona$pct_hawaiian_pi
           + df_merged_geo_offense_nona$pct_white
           + df_merged_geo_offense_nona$pct_black
           + df_merged_geo_offense_nona$pct_hispanic_latino
           + df_merged_geo_offense_nona$pct_am_ind
           + df_merged_geo_offense_nona$pct_other
)
summ(lm_ct_offense12)

print("Crime Count = Median HH Income + Offense Type + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander + % White + % Black + % Hispanic or Latino + % American Indian or Alaska Native + % Other Race + % 2 or More Races")
lm_ct_offense13 <- lm(df_merged_geo_offense_nona$crime_count ~ df_merged_geo_offense_nona$med_hh_income 
           + df_merged_geo_offense_nona$offense
           + df_merged_geo_offense_nona$pop_dens 
           + df_merged_geo_offense_nona$sex_age_sex_ratio
           + df_merged_geo_offense_nona$age_median
           + df_merged_geo_offense_nona$pct_asian
           + df_merged_geo_offense_nona$pct_hawaiian_pi
           + df_merged_geo_offense_nona$pct_white
           + df_merged_geo_offense_nona$pct_black
           + df_merged_geo_offense_nona$pct_hispanic_latino
           + df_merged_geo_offense_nona$pct_am_ind
           + df_merged_geo_offense_nona$pct_other
           + df_merged_geo_offense_nona$pct_2_races
)
summ(lm_ct_offense13)

coefs_names2 <- c("Constant" = "(Intercept)"
                  , "Offense = Assault w/ Dangerous Weapon" = "df_merged_geo_offense_nona$offenseASSAULT W/DANGEROUS WEAPON"
                  , "Offense = Burglary" = "df_merged_geo_offense_nona$offenseBURGLARY"
                  , "Offense = Homicide" = "df_merged_geo_offense_nona$offenseHOMICIDE"
                  , "Offense = Motor Vehicle Theft" = "df_merged_geo_offense_nona$offenseMOTOR VEHICLE THEFT"
                  , "Offense = Robbery" = "df_merged_geo_offense_nona$offenseROBBERY"
                  , "Offense = Sex Abuse" = "df_merged_geo_offense_nona$offenseSEX ABUSE"
                  , "Offense = Theft From Auto" = "df_merged_geo_offense_nona$offenseTHEFT F/AUTO"
                  , "Offense = Theft/Other" = "df_merged_geo_offense_nona$offenseTHEFT/OTHER"
                  , "Median HH Income" = "df_merged_geo_offense_nona$med_hh_income"
                  , "Population Density" = "df_merged_geo_offense_nona$pop_dens"
                  , "Sex Ratio" = "df_merged_geo_offense_nona$sex_age_sex_ratio"
                  , "Median Age" = "df_merged_geo_offense_nona$age_median"
                  , "% Asian" = "df_merged_geo_offense_nona$pct_asian"
                  , "% Hawaiian/Pacific Islander" = "df_merged_geo_offense_nona$pct_hawaiian_pi"
                  , "% White" = "df_merged_geo_offense_nona$pct_white"
                  , "% Black/African American" = "df_merged_geo_offense_nona$pct_black"
                  , "% Hispanic/Latino" = "df_merged_geo_offense_nona$pct_hispanic_latino"
                  , "% American Indian/Alaska Native" = "df_merged_geo_offense_nona$pct_am_ind"
                  , "% Other Race" = "df_merged_geo_offense_nona$pct_other"
                  , "% 2 or More Races"= "df_merged_geo_offense_nona$pct_2_races")
export_summs(lm_ct_offense1, lm_ct_offense2, lm_ct_offense3, lm_ct_offense4, lm_ct_offense5, lm_ct_offense6, lm_ct_offense7, lm_ct_offense8, 
             lm_ct_offense9, lm_ct_offense10, lm_ct_offense11, lm_ct_offense12, lm_ct_offense13
             , coefs = coefs_names2
             , error_format = "p = {p.value}"
             , statistics = c(N = "nobs", AdjR2 = "adj.r.squared")
             , to.file = "xlsx"
             , file.name = "D:/ROB/MICA/Spring 2021/MVIS 5301.01/Final Project/Submitted Files/output/Regression Outputs_Census Tract-Offense Level.xlsx")

# Look at the correlation between Median HH Income and Crime Count in 2019 by Offense type
func <- function(df_merged_geo_offense_nona)
{
  return(data.frame(COR = cor(df_merged_geo_offense_nona$crime_count, df_merged_geo_offense_nona$med_hh_income)))
}

ddply(df_merged_geo_offense_nona, .(offense), func)

# Look at significance of these relationship by running the follow multivariate linear regression for each offense: Crime Count = Median HH Income + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian/Pacific Islander + % White + % Black + % Hispanic/Latino + % American Indian/Alaskan Native + % Other Race + % 2 or More Races
df_merged_geo_offense_nonasp <-df_merged_geo_offense_nona %>% 
  filter(offense == "ASSAULT W/DANGEROUS WEAPON") 
print("ASSAULT W/DANGEROUS WEAPON")
lm_ct_offensesp1 <- lm(df_merged_geo_offense_nonasp$crime_count ~ df_merged_geo_offense_nonasp$med_hh_income 
           + df_merged_geo_offense_nonasp$pop_dens 
           + df_merged_geo_offense_nonasp$sex_age_sex_ratio 
           + df_merged_geo_offense_nonasp$age_median 
           + df_merged_geo_offense_nonasp$pct_asian 
           + df_merged_geo_offense_nonasp$pct_hawaiian_pi
           + df_merged_geo_offense_nonasp$pct_white 
           + df_merged_geo_offense_nonasp$pct_black
           + df_merged_geo_offense_nonasp$pct_hispanic_latino
           + df_merged_geo_offense_nonasp$pct_am_ind
           + df_merged_geo_offense_nonasp$pct_other
           + df_merged_geo_offense_nonasp$pct_2_races)

summ(lm_ct_offensesp1)

df_merged_geo_offense_nonasp <-df_merged_geo_offense_nona %>% 
  filter(offense == "BURGLARY") 
print("BURGLARY")
lm_ct_offensesp2 <- lm(df_merged_geo_offense_nonasp$crime_count ~ df_merged_geo_offense_nonasp$med_hh_income 
           + df_merged_geo_offense_nonasp$pop_dens 
           + df_merged_geo_offense_nonasp$sex_age_sex_ratio 
           + df_merged_geo_offense_nonasp$age_median 
           + df_merged_geo_offense_nonasp$pct_asian 
           + df_merged_geo_offense_nonasp$pct_hawaiian_pi
           + df_merged_geo_offense_nonasp$pct_white 
           + df_merged_geo_offense_nonasp$pct_black
           + df_merged_geo_offense_nonasp$pct_hispanic_latino
           + df_merged_geo_offense_nonasp$pct_am_ind
           + df_merged_geo_offense_nonasp$pct_other
           + df_merged_geo_offense_nonasp$pct_2_races)
summ(lm_ct_offensesp2)


df_merged_geo_offense_nonasp <-df_merged_geo_offense_nona %>% 
  filter(offense == "HOMICIDE") 
print("HOMICIDE")
lm_ct_offensesp3 <- lm(df_merged_geo_offense_nonasp$crime_count ~ df_merged_geo_offense_nonasp$med_hh_income 
           + df_merged_geo_offense_nonasp$pop_dens 
           + df_merged_geo_offense_nonasp$sex_age_sex_ratio 
           + df_merged_geo_offense_nonasp$age_median 
           + df_merged_geo_offense_nonasp$pct_asian 
           + df_merged_geo_offense_nonasp$pct_hawaiian_pi
           + df_merged_geo_offense_nonasp$pct_white 
           + df_merged_geo_offense_nonasp$pct_black
           + df_merged_geo_offense_nonasp$pct_hispanic_latino
           + df_merged_geo_offense_nonasp$pct_am_ind
           + df_merged_geo_offense_nonasp$pct_other
           + df_merged_geo_offense_nonasp$pct_2_races)
summ(lm_ct_offensesp3)

df_merged_geo_offense_nonasp <-df_merged_geo_offense_nona %>% 
  filter(offense == "MOTOR VEHICLE THEFT") 
print("MOTOR VEHICLE THEFT")
lm_ct_offensesp4 <- lm(df_merged_geo_offense_nonasp$crime_count ~ df_merged_geo_offense_nonasp$med_hh_income 
           + df_merged_geo_offense_nonasp$pop_dens 
           + df_merged_geo_offense_nonasp$sex_age_sex_ratio 
           + df_merged_geo_offense_nonasp$age_median 
           + df_merged_geo_offense_nonasp$pct_asian 
           + df_merged_geo_offense_nonasp$pct_hawaiian_pi
           + df_merged_geo_offense_nonasp$pct_white 
           + df_merged_geo_offense_nonasp$pct_black
           + df_merged_geo_offense_nonasp$pct_hispanic_latino
           + df_merged_geo_offense_nonasp$pct_am_ind
           + df_merged_geo_offense_nonasp$pct_other
           + df_merged_geo_offense_nonasp$pct_2_races)
summ(lm_ct_offensesp4)

df_merged_geo_offense_nonasp <-df_merged_geo_offense_nona %>% 
  filter(offense == "ROBBERY") 
print("ROBBERY")
lm_ct_offensesp5 <- lm(df_merged_geo_offense_nonasp$crime_count ~ df_merged_geo_offense_nonasp$med_hh_income 
           + df_merged_geo_offense_nonasp$pop_dens 
           + df_merged_geo_offense_nonasp$sex_age_sex_ratio 
           + df_merged_geo_offense_nonasp$age_median 
           + df_merged_geo_offense_nonasp$pct_asian 
           + df_merged_geo_offense_nonasp$pct_hawaiian_pi
           + df_merged_geo_offense_nonasp$pct_white 
           + df_merged_geo_offense_nonasp$pct_black
           + df_merged_geo_offense_nonasp$pct_hispanic_latino
           + df_merged_geo_offense_nonasp$pct_am_ind
           + df_merged_geo_offense_nonasp$pct_other
           + df_merged_geo_offense_nonasp$pct_2_races)
summ(lm_ct_offensesp5)

df_merged_geo_offense_nonasp <-df_merged_geo_offense_nona %>% 
  filter(offense == "SEX ABUSE") 
print("SEX ABUSE")
lm_ct_offensesp6 <- lm(df_merged_geo_offense_nonasp$crime_count ~ df_merged_geo_offense_nonasp$med_hh_income 
           + df_merged_geo_offense_nonasp$pop_dens 
           + df_merged_geo_offense_nonasp$sex_age_sex_ratio 
           + df_merged_geo_offense_nonasp$age_median 
           + df_merged_geo_offense_nonasp$pct_asian 
           + df_merged_geo_offense_nonasp$pct_hawaiian_pi
           + df_merged_geo_offense_nonasp$pct_white 
           + df_merged_geo_offense_nonasp$pct_black
           + df_merged_geo_offense_nonasp$pct_hispanic_latino
           + df_merged_geo_offense_nonasp$pct_am_ind
           + df_merged_geo_offense_nonasp$pct_other
           + df_merged_geo_offense_nonasp$pct_2_races)
summ(lm_ct_offensesp6)

df_merged_geo_offense_nonasp <-df_merged_geo_offense_nona %>% 
  filter(offense == "THEFT F/AUTO") 
print("THEFT F/AUTO")
lm_ct_offensesp7 <- lm(df_merged_geo_offense_nonasp$crime_count ~ df_merged_geo_offense_nonasp$med_hh_income 
           + df_merged_geo_offense_nonasp$pop_dens 
           + df_merged_geo_offense_nonasp$sex_age_sex_ratio 
           + df_merged_geo_offense_nonasp$age_median 
           + df_merged_geo_offense_nonasp$pct_asian 
           + df_merged_geo_offense_nonasp$pct_hawaiian_pi
           + df_merged_geo_offense_nonasp$pct_white 
           + df_merged_geo_offense_nonasp$pct_black
           + df_merged_geo_offense_nonasp$pct_hispanic_latino
           + df_merged_geo_offense_nonasp$pct_am_ind
           + df_merged_geo_offense_nonasp$pct_other
           + df_merged_geo_offense_nonasp$pct_2_races)
summ(lm_ct_offensesp7)

df_merged_geo_offense_nonasp <-df_merged_geo_offense_nona %>% 
  filter(offense == "THEFT/OTHER") 
print("THEFT/OTHER")
lm_ct_offensesp8 <- lm(df_merged_geo_offense_nonasp$crime_count ~ df_merged_geo_offense_nonasp$med_hh_income 
           + df_merged_geo_offense_nonasp$pop_dens 
           + df_merged_geo_offense_nonasp$sex_age_sex_ratio 
           + df_merged_geo_offense_nonasp$age_median 
           + df_merged_geo_offense_nonasp$pct_asian 
           + df_merged_geo_offense_nonasp$pct_hawaiian_pi
           + df_merged_geo_offense_nonasp$pct_white 
           + df_merged_geo_offense_nonasp$pct_black
           + df_merged_geo_offense_nonasp$pct_hispanic_latino
           + df_merged_geo_offense_nonasp$pct_am_ind
           + df_merged_geo_offense_nonasp$pct_other
           + df_merged_geo_offense_nonasp$pct_2_races)
summ(lm_ct_offensesp8)

df_merged_geo_offense_nonasp <-df_merged_geo_offense_nona %>% 
  filter(offense == "ARSON") 
print("ARSON")
lm_ct_offensesp9 <- lm(df_merged_geo_offense_nonasp$crime_count ~ df_merged_geo_offense_nonasp$med_hh_income 
           + df_merged_geo_offense_nonasp$pop_dens 
           + df_merged_geo_offense_nonasp$sex_age_sex_ratio 
           + df_merged_geo_offense_nonasp$age_median 
           + df_merged_geo_offense_nonasp$pct_asian 
           + df_merged_geo_offense_nonasp$pct_hawaiian_pi
           + df_merged_geo_offense_nonasp$pct_white 
           + df_merged_geo_offense_nonasp$pct_black
           + df_merged_geo_offense_nonasp$pct_hispanic_latino
           + df_merged_geo_offense_nonasp$pct_am_ind
           + df_merged_geo_offense_nonasp$pct_other
           + df_merged_geo_offense_nonasp$pct_2_races)
summ(lm_ct_offensesp9)


coefs_names3 <- c("Constant" = "(Intercept)"
                  , "Median HH Income" = "df_merged_geo_offense_nonasp$med_hh_income"
                  , "Population Density" = "df_merged_geo_offense_nonasp$pop_dens"
                  , "Sex Ratio" = "df_merged_geo_offense_nonasp$sex_age_sex_ratio"
                  , "Median Age" = "df_merged_geo_offense_nonasp$age_median"
                  , "% Asian" = "df_merged_geo_offense_nonasp$pct_asian"
                  , "% Hawaiian/Pacific Islander" = "df_merged_geo_offense_nonasp$pct_hawaiian_pi"
                  , "% White" = "df_merged_geo_offense_nonasp$pct_white"
                  , "% Black/African American" = "df_merged_geo_offense_nonasp$pct_black"
                  , "% Hispanic/Latino" = "df_merged_geo_offense_nonasp$pct_hispanic_latino"
                  , "% American Indian/Alaska Native" = "df_merged_geo_offense_nonasp$pct_am_ind"
                  , "% Other Race" = "df_merged_geo_offense_nonasp$pct_other"
                  , "% 2 or More Races"= "df_merged_geo_offense_nonasp$pct_2_races")
export_summs(lm_ct_offensesp1, lm_ct_offensesp2, lm_ct_offensesp3, lm_ct_offensesp4, lm_ct_offensesp5, lm_ct_offensesp6, lm_ct_offensesp7, 
             lm_ct_offensesp8
             , coefs = coefs_names3
             , error_format = "p = {p.value}"
             , statistics = c(N = "nobs", AdjR2 = "adj.r.squared")
             , to.file = "xlsx"
             , file.name = "D:/ROB/MICA/Spring 2021/MVIS 5301.01/Final Project/Submitted Files/output/Regression Outputs_Census Tract-Offense Level2.xlsx")

# Census Tract-Offense Level, Group Offenses by "violent crime" or "not violent crime"
# For this analysis, I categorized the following offenses as violent crimes (violent_crime = 1): "ASSAULT W/DANGEROUS WEAPON", "BURGLARY", "HOMICIDE", "ROBBERy", "SEX ABUSE", "ARSON".  I categorized "MOTOR VEHICLE THEFT", "THEFT F/AUTO", and "THEFT/OTHER" as non-violent crimes (violent_crime = 0).  

crime_2019_clean_offensegroup <- crime_2019_clean %>%
  mutate(
    violent_crime = case_when(
      offense == "ASSAULT W/DANGEROUS WEAPON" ~ 1,
      offense == "BURGLARY" ~ 1,
      offense == "HOMICIDE" ~ 1,
      offense == "MOTOR VEHICLE THEFT" ~ 0,
      offense == "ROBBERY" ~ 1,
      offense == "SEX ABUSE" ~ 1,
      offense == "THEFT F/AUTO" ~ 0,
      offense == "THEFT/OTHER" ~ 0,
      offense == "ARSON" ~ 1
      
    )
  )

crime_2019_agg_geo_offensegroup <- crime_2019_clean_offensegroup %>% 
  group_by(census_tract, violent_crime) %>% 
  dplyr::summarise(
    crime_count = n()
  )

df_merged_geo_offensegroup <- merge(crime_2019_agg_geo_offensegroup, df_merged, by.x=c("census_tract"), by.y=c("census_tract"), sort = FALSE)
df_merged_geo_offensegroup_nona <- filter(df_merged_geo_offensegroup, !is.na(df_merged_geo_offensegroup$med_hh_income))

# Look at correlation matrix
df_merged_geo_offensegroup_nona_cor <-  dplyr::select(df_merged_geo_offensegroup_nona,-c("census_tract","total_hh"))
my_cor3 <- rcorr(as.matrix(df_merged_geo_offensegroup_nona_cor))
my_cor3

# Run linear regressions at the Census Tract-Offense Group level, adding one explanatory variable at a time

print("Crime Count = Median HH Income")
lm_ct_violent1 <- lm(df_merged_geo_offensegroup_nona$crime_count ~ df_merged_geo_offensegroup_nona$med_hh_income)
summ(lm_ct_violent1)

print("Crime Count = Median HH Income + Violent Crime Dummy")
lm_ct_violent2 <- lm(df_merged_geo_offensegroup_nona$crime_count ~ df_merged_geo_offensegroup_nona$med_hh_income
           + df_merged_geo_offensegroup_nona$violent_crime)
summ(lm_ct_violent2)

print("Crime Count = Median HH Income + Violent Crime Dummy + Population Density")
lm_ct_violent3 <- lm(df_merged_geo_offensegroup_nona$crime_count ~ df_merged_geo_offensegroup_nona$med_hh_income
           + df_merged_geo_offensegroup_nona$violent_crime
           + df_merged_geo_offensegroup_nona$pop_dens
)
summ(lm_ct_violent3)

print("Crime Count = Median HH Income + Violent Crime Dummy + Population Density + Sex Ratio")
lm_ct_violent4 <- lm(df_merged_geo_offensegroup_nona$crime_count ~ df_merged_geo_offensegroup_nona$med_hh_income
           + df_merged_geo_offensegroup_nona$violent_crime
           + df_merged_geo_offensegroup_nona$pop_dens
           + df_merged_geo_offensegroup_nona$sex_age_sex_ratio
)
summ(lm_ct_violent4)

print("Crime Count = Median HH Income + Violent Crime Dummy + Population Density + Sex Ratio + Median Age")
lm_ct_violent5 <- lm(df_merged_geo_offensegroup_nona$crime_count ~ df_merged_geo_offensegroup_nona$med_hh_income
           + df_merged_geo_offensegroup_nona$violent_crime
           + df_merged_geo_offensegroup_nona$pop_dens
           + df_merged_geo_offensegroup_nona$sex_age_sex_ratio
           + df_merged_geo_offensegroup_nona$age_median
)
summ(lm_ct_violent5)

print("Crime Count = Median HH Income + Violent Crime Dummy + Population Density + Sex Ratio + Median Age + % Asian")
lm_ct_violent6 <- lm(df_merged_geo_offensegroup_nona$crime_count ~ df_merged_geo_offensegroup_nona$med_hh_income
           + df_merged_geo_offensegroup_nona$violent_crime
           + df_merged_geo_offensegroup_nona$pop_dens
           + df_merged_geo_offensegroup_nona$sex_age_sex_ratio
           + df_merged_geo_offensegroup_nona$age_median
           + df_merged_geo_offensegroup_nona$pct_asian
)
summ(lm_ct_violent6)

print("Crime Count = Median HH Income + Violent Crime Dummy + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander")
lm_ct_violent7 <- lm(df_merged_geo_offensegroup_nona$crime_count ~ df_merged_geo_offensegroup_nona$med_hh_income
           + df_merged_geo_offensegroup_nona$violent_crime
           + df_merged_geo_offensegroup_nona$pop_dens
           + df_merged_geo_offensegroup_nona$sex_age_sex_ratio
           + df_merged_geo_offensegroup_nona$age_median
           + df_merged_geo_offensegroup_nona$pct_asian
           + df_merged_geo_offensegroup_nona$pct_hawaiian_pi
)
summ(lm_ct_violent7)

print("Crime Count = Median HH Income + Violent Crime Dummy + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander + % White")
lm_ct_violent8 <- lm(df_merged_geo_offensegroup_nona$crime_count ~ df_merged_geo_offensegroup_nona$med_hh_income
           + df_merged_geo_offensegroup_nona$violent_crime
           + df_merged_geo_offensegroup_nona$pop_dens
           + df_merged_geo_offensegroup_nona$sex_age_sex_ratio
           + df_merged_geo_offensegroup_nona$age_median
           + df_merged_geo_offensegroup_nona$pct_asian
           + df_merged_geo_offensegroup_nona$pct_hawaiian_pi
           + df_merged_geo_offensegroup_nona$pct_white
)
summ(lm_ct_violent8)

print("Crime Count = Median HH Income + Violent Crime Dummy + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander + % White + % Black")
lm_ct_violent9 <- lm(df_merged_geo_offensegroup_nona$crime_count ~ df_merged_geo_offensegroup_nona$med_hh_income
           + df_merged_geo_offensegroup_nona$violent_crime
           + df_merged_geo_offensegroup_nona$pop_dens
           + df_merged_geo_offensegroup_nona$sex_age_sex_ratio
           + df_merged_geo_offensegroup_nona$age_median
           + df_merged_geo_offensegroup_nona$pct_asian
           + df_merged_geo_offensegroup_nona$pct_hawaiian_pi
           + df_merged_geo_offensegroup_nona$pct_white
           + df_merged_geo_offensegroup_nona$pct_black
)
summ(lm_ct_violent9)

print("Crime Count = Median HH Income + Violent Crime Dummy + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander + % White + % Black + % Hispanic or Latino")
lm_ct_violent10 <- lm(df_merged_geo_offensegroup_nona$crime_count ~ df_merged_geo_offensegroup_nona$med_hh_income
           + df_merged_geo_offensegroup_nona$violent_crime
           + df_merged_geo_offensegroup_nona$pop_dens
           + df_merged_geo_offensegroup_nona$sex_age_sex_ratio
           + df_merged_geo_offensegroup_nona$age_median
           + df_merged_geo_offensegroup_nona$pct_asian
           + df_merged_geo_offensegroup_nona$pct_hawaiian_pi
           + df_merged_geo_offensegroup_nona$pct_white
           + df_merged_geo_offensegroup_nona$pct_black
           + df_merged_geo_offensegroup_nona$pct_hispanic_latino
)
summ(lm_ct_violent10)

print("Crime Count = Median HH Income + Violent Crime Dummy + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander + % White + % Black + % Hispanic or Latino + % American Indian or Alaska Native")
lm_ct_violent11 <- lm(df_merged_geo_offensegroup_nona$crime_count ~ df_merged_geo_offensegroup_nona$med_hh_income
           + df_merged_geo_offensegroup_nona$violent_crime
           + df_merged_geo_offensegroup_nona$pop_dens
           + df_merged_geo_offensegroup_nona$sex_age_sex_ratio
           + df_merged_geo_offensegroup_nona$age_median
           + df_merged_geo_offensegroup_nona$pct_asian
           + df_merged_geo_offensegroup_nona$pct_hawaiian_pi
           + df_merged_geo_offensegroup_nona$pct_white
           + df_merged_geo_offensegroup_nona$pct_black
           + df_merged_geo_offensegroup_nona$pct_hispanic_latino
           + df_merged_geo_offensegroup_nona$pct_am_ind
)
summ(lm_ct_violent11)

print("Crime Count = Median HH Income + Violent Crime Dummy + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander + % White + % Black + % Hispanic or Latino + % American Indian or Alaska Native + % Other Race")
lm_ct_violent12 <- lm(df_merged_geo_offensegroup_nona$crime_count ~ df_merged_geo_offensegroup_nona$med_hh_income
           + df_merged_geo_offensegroup_nona$violent_crime
           + df_merged_geo_offensegroup_nona$pop_dens
           + df_merged_geo_offensegroup_nona$sex_age_sex_ratio
           + df_merged_geo_offensegroup_nona$age_median
           + df_merged_geo_offensegroup_nona$pct_asian
           + df_merged_geo_offensegroup_nona$pct_hawaiian_pi
           + df_merged_geo_offensegroup_nona$pct_white
           + df_merged_geo_offensegroup_nona$pct_black
           + df_merged_geo_offensegroup_nona$pct_hispanic_latino
           + df_merged_geo_offensegroup_nona$pct_am_ind
           + df_merged_geo_offensegroup_nona$pct_other
)
summ(lm_ct_violent12)

print("Crime Count = Median HH Income + Violent Crime Dummy + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian or Pacific Islander + % White + % Black + % Hispanic or Latino + % American Indian or Alaska Native + % Other Race + % 2 or More Races")
lm_ct_violent13 <- lm(df_merged_geo_offensegroup_nona$crime_count ~ df_merged_geo_offensegroup_nona$med_hh_income
           + df_merged_geo_offensegroup_nona$violent_crime
           + df_merged_geo_offensegroup_nona$pop_dens
           + df_merged_geo_offensegroup_nona$sex_age_sex_ratio
           + df_merged_geo_offensegroup_nona$age_median
           + df_merged_geo_offensegroup_nona$pct_asian
           + df_merged_geo_offensegroup_nona$pct_hawaiian_pi
           + df_merged_geo_offensegroup_nona$pct_white
           + df_merged_geo_offensegroup_nona$pct_black
           + df_merged_geo_offensegroup_nona$pct_hispanic_latino
           + df_merged_geo_offensegroup_nona$pct_am_ind
           + df_merged_geo_offensegroup_nona$pct_other
           + df_merged_geo_offensegroup_nona$pct_2_races
)
summ(lm_ct_violent13)



coefs_names4 <- c("Constant" = "(Intercept)"
                  , "Violent Crime Dummy Var" = "df_merged_geo_offensegroup_nona$violent_crime"
                  , "Median HH Income" = "df_merged_geo_offensegroup_nona$med_hh_income"
                  , "Population Density" = "df_merged_geo_offensegroup_nona$pop_dens"
                  , "Sex Ratio" = "df_merged_geo_offensegroup_nona$sex_age_sex_ratio"
                  , "Median Age" = "df_merged_geo_offensegroup_nona$age_median"
                  , "% Asian" = "df_merged_geo_offensegroup_nona$pct_asian"
                  , "% Hawaiian/Pacific Islander" = "df_merged_geo_offensegroup_nona$pct_hawaiian_pi"
                  , "% White" = "df_merged_geo_offensegroup_nona$pct_white"
                  , "% Black/African American" = "df_merged_geo_offensegroup_nona$pct_black"
                  , "% Hispanic/Latino" = "df_merged_geo_offensegroup_nona$pct_hispanic_latino"
                  , "% American Indian/Alaska Native" = "df_merged_geo_offensegroup_nona$pct_am_ind"
                  , "% Other Race" = "df_merged_geo_offensegroup_nona$pct_other"
                  , "% 2 or More Races"= "df_merged_geo_offensegroup_nona$pct_2_races")

export_summs(lm_ct_violent1, lm_ct_violent2, lm_ct_violent3, lm_ct_violent4, lm_ct_violent5, lm_ct_violent6, lm_ct_violent7, lm_ct_violent8,
             lm_ct_violent9, lm_ct_violent10, lm_ct_violent11, lm_ct_violent12, lm_ct_violent13
             , coefs = coefs_names4
             , error_format = "p = {p.value}"
             , statistics = c(N = "nobs", AdjR2 = "adj.r.squared")
             , to.file = "xlsx"
             , file.name = "D:/ROB/MICA/Spring 2021/MVIS 5301.01/Final Project/Submitted Files/output/Regression Outputs_Census Tract-Violent Crimes Level.xlsx")

func <- function(df_merged_geo_offensegroup_nona)
{
  return(data.frame(COR = cor(df_merged_geo_offensegroup_nona$crime_count, df_merged_geo_offensegroup_nona$med_hh_income)))
}

ddply(df_merged_geo_offensegroup_nona, .(violent_crime), func)

# Look at significance of these relationship by running the follow multivariate linear regression for each offense: Crime Count = Median HH Income + Population Density + Sex Ratio + Median Age + % Asian + % Hawaiian/Pacific Islander + % White + % Black + % Hispanic/Latino + % American Indian/Alaskan Native + % Other Race + % 2 or More Races
df_merged_geo_offensegroup_nonasp <-df_merged_geo_offensegroup_nona %>% 
  filter(violent_crime == 1) 
print("Violent Crimes: ASSAULT W/DANGEROUS WEAPON, BURGLARY, HOMICIDE, ROBBERY, SEX ABUSE, ARSON")

lm_ct_violentsp1 <- lm(df_merged_geo_offensegroup_nonasp$crime_count ~ df_merged_geo_offensegroup_nonasp$med_hh_income 
           + df_merged_geo_offensegroup_nonasp$pop_dens 
           + df_merged_geo_offensegroup_nonasp$sex_age_sex_ratio 
           + df_merged_geo_offensegroup_nonasp$age_median 
           + df_merged_geo_offensegroup_nonasp$pct_asian 
           + df_merged_geo_offensegroup_nonasp$pct_hawaiian_pi
           + df_merged_geo_offensegroup_nonasp$pct_white 
           + df_merged_geo_offensegroup_nonasp$pct_black
           + df_merged_geo_offensegroup_nonasp$pct_hispanic_latino
           + df_merged_geo_offensegroup_nonasp$pct_am_ind
           + df_merged_geo_offensegroup_nonasp$pct_other
           + df_merged_geo_offensegroup_nonasp$pct_2_races)

summ(lm_ct_violentsp1)

df_merged_geo_offensegroup_nonasp <-df_merged_geo_offensegroup_nona %>% 
  filter(violent_crime == 0) 
print("Non-Violent Crimes: MOTOR VEHICLE THEFT, THEFT F/AUTO, THEFT/OTHER")

lm_ct_violentsp2 <- lm(df_merged_geo_offensegroup_nonasp$crime_count ~ df_merged_geo_offensegroup_nonasp$med_hh_income 
           + df_merged_geo_offensegroup_nonasp$pop_dens 
           + df_merged_geo_offensegroup_nonasp$sex_age_sex_ratio 
           + df_merged_geo_offensegroup_nonasp$age_median 
           + df_merged_geo_offensegroup_nonasp$pct_asian 
           + df_merged_geo_offensegroup_nonasp$pct_hawaiian_pi
           + df_merged_geo_offensegroup_nonasp$pct_white 
           + df_merged_geo_offensegroup_nonasp$pct_black
           + df_merged_geo_offensegroup_nonasp$pct_hispanic_latino
           + df_merged_geo_offensegroup_nonasp$pct_am_ind
           + df_merged_geo_offensegroup_nonasp$pct_other
           + df_merged_geo_offensegroup_nonasp$pct_2_races)

summ(lm_ct_violentsp2)

coefs_names5 <- c("Constant" = "(Intercept)"
                  , "Median HH Income" = "df_merged_geo_offensegroup_nonasp$med_hh_income"
                  , "Population Density" = "df_merged_geo_offensegroup_nonasp$pop_dens"
                  , "Sex Ratio" = "df_merged_geo_offensegroup_nonasp$sex_age_sex_ratio"
                  , "Median Age" = "df_merged_geo_offensegroup_nonasp$age_median"
                  , "% Asian" = "df_merged_geo_offensegroup_nonasp$pct_asian"
                  , "% Hawaiian/Pacific Islander" = "df_merged_geo_offensegroup_nonasp$pct_hawaiian_pi"
                  , "% White" = "df_merged_geo_offensegroup_nonasp$pct_white"
                  , "% Black/African American" = "df_merged_geo_offensegroup_nonasp$pct_black"
                  , "% Hispanic/Latino" = "df_merged_geo_offensegroup_nonasp$pct_hispanic_latino"
                  , "% American Indian/Alaska Native" = "df_merged_geo_offensegroup_nonasp$pct_am_ind"
                  , "% Other Race" = "df_merged_geo_offensegroup_nonasp$pct_other"
                  , "% 2 or More Races"= "df_merged_geo_offensegroup_nonasp$pct_2_races")

export_summs(lm_ct_violentsp1, lm_ct_violentsp2
             , coefs = coefs_names5
             , error_format = "p = {p.value}"
             , statistics = c(N = "nobs", AdjR2 = "adj.r.squared")
             , to.file = "xlsx"
             , file.name = "D:/ROB/MICA/Spring 2021/MVIS 5301.01/Final Project/Submitted Files/output/Regression Outputs_Census Tract-Violent Crimes Level2.xlsx")