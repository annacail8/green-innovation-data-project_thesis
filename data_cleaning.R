library(tidyverse)
library(dplyr)
library(readxl)

setwd("C:\\Users\\Utente\\Documents\\GitHub\\green-innovation-data-project\\raw_data")

eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
  "Czechia", "Denmark", "Estonia", "Finland", "France",
  "Germany", "Greece", "Hungary", "Ireland", "Italy",
  "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
  "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
  "Spain", "Sweden"
)

# Load the dataset

## GDP PER CAPITA ##
gdp_pc <- read.csv("gdp_pc.csv", sep = ",")

gdp_pc <- gdp_pc |> 
  filter( unit %in% c("Current prices, euro per capita") &
          na_item %in% c("Gross domestic product at market prices"))

gdp_pc <- gdp_pc |>
  rename(
    country = `geo`,
    year = `TIME_PERIOD`,
    gdp_pc = `OBS_VALUE`
  )

gdp_pc <- gdp_pc |>
  select(country, unit, na_item, year, gdp_pc)

gdp_pc_eu <- gdp_pc |> 
  filter(country %in% c("European Union - 27 countries (from 2020)"))

gdp_pc_all <- gdp_pc |>
  filter(country %in% eu_countries)

## GDP ##
gdp <- read_excel("gdp.xlsx", sheet = "Sheet 2")

gdp <- gdp[-c(1:7), ]
colnames(gdp) <- as.character(gdp[1, ])
gdp <- gdp[-1, ]

# remove columns with all NAs

gdp <- gdp[, colSums(!is.na(gdp)) > 0]
gdp <- gdp[-1, ]
gdp <- gdp[-c(46:52),]

gdp <- gdp[, !(is.na(names(gdp)) | startsWith(names(gdp), "NA"))]

gdp <- rename(gdp, country = `TIME`)

gdp <- gdp |> 
  mutate(
    country = case_when(
      country == "Türkiye" ~ "Turkiye",
      TRUE ~ country
    )
  )
# pivot longer

gdp_long <- gdp |> 
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "gdp"
  )

gdp_long_eu <- gdp_long |>
  filter(
    country %in% c('European Union - 27 countries (from 2020)')
  )

gdp_long_all <- gdp_long |>
  filter(
    country %in% eu_countries,
    !is.na(gdp)
  )

## Energy price 

## GASOLINE PRICE TRANSPORTS

gasoline_IEA <- read.csv("gasoline_IEA.csv", header = TRUE, sep = ",")

gasoline_IEA <- gasoline_IEA |> 
  select(Region.Country.Subdivision, TIME_PERIOD, OBS_VALUE)

gasoline_IEA <- gasoline_IEA |>
  rename(
    country = `Region.Country.Subdivision`,
    year = `TIME_PERIOD`,
    gasoline_price_IEA = `OBS_VALUE`
  )

gasoline_IEA <- gasoline_IEA |>
  mutate(
    country = case_when(
      country == "Slovak Republic" ~ "Slovakia",
      TRUE ~ country  # Keep other values unchanged
    ))

gasoline_IEA <- gasoline_IEA |>
  filter(country %in% eu_countries)

## Natural gas price IEA

gas_IEA <- read.csv("gas_price_IEA.csv", header = TRUE, sep = ",")


gas_IEA <- gas_IEA |> 
  select(Region.Country.Subdivision, TIME_PERIOD, OBS_VALUE)

gas_IEA <- gas_IEA |>
  rename(
    country = `Region.Country.Subdivision`,
    year = `TIME_PERIOD`,
    gas_price_IEA = `OBS_VALUE`
  )

gas_IEA <- gas_IEA |>
  mutate(
    country = case_when(
      country == "Slovak Republic" ~ "Slovakia",
      TRUE ~ country  # Keep other values unchanged
    ))

gas_IEA <- gas_IEA |>
  filter(country %in% eu_countries)

## GAS PRICE ###

gas_price_until2007 <- read.csv("gas_price_until2007.csv", header = TRUE, sep = ",")

gas_price_from2007 <- read.csv("gas_price_from2007.csv", header = TRUE, sep = ",")

gas_price1 <- bind_rows(gas_price_until2007, gas_price_from2007) 

gas_price1 <- gas_price1 |>
  select(c(TIME_PERIOD, geo, OBS_VALUE)) |>
  rename(
    year = `TIME_PERIOD`,
    country = `geo`,
    gas_price = `OBS_VALUE`
  )

gas_price_all <- gas_price1 |>
  filter(country %in% eu_countries)

gas_price_annual_all <- gas_price_all %>%
  mutate(
    year = str_extract(year, "^\\d{4}")  # Extract "2005" from "2005H1", etc.
  ) %>%
  group_by(country, year) %>%
  summarise(
    gas_price = mean(gas_price, na.rm = TRUE),  # or sum(value, na.rm = TRUE)
    .groups = "drop"
  )

gas_price_eu <- gas_price1 |>
  filter(country %in% c("European Union - 27 countries (from 2020)", "European Union - 15 countries (1995-2004)")) |> 
  mutate(
    country = "European Union"
  )

gas_price_eu_annual <- gas_price_eu %>%
  mutate(
    year = str_extract(year, "^\\d{4}")  # Extract "2005" from "2005H1", etc.
  ) %>%
  group_by(country, year) %>%
  summarise(
    gas_price = mean(gas_price, na.rm = TRUE),  # or sum(value, na.rm = TRUE)
    .groups = "drop"
  )



# REAL PRICES COMMODITIES
energy_price <- read_excel("CMO-Historical-Data-Annual.xlsx", 
                     sheet = "Annual Prices (Real)",
                     skip = 5, 
                     col_names = FALSE)

colnames(energy_price) <- paste(energy_price[1, ], energy_price[2, ], sep = "\n")

# Remove the first two rows from the data
energy_price <- energy_price[-c(1,2), ] %>%
  rename(
    year = `NA\nNA`  
  )

energy_price1 <- energy_price |>
  select(c(
    year,
    "Natural gas, Europe\n($/mmbtu)",
    "Crude oil, average\n($/bbl)"
  ))

energy_price1 <- energy_price1 |>
  mutate(year = as.character(year))

## FDI ##

fdi <- read.csv("fdi.csv", header = TRUE, sep = ",")

fdi <- fdi |> 
  mutate(
    Country.Name = case_when(
      Country.Name == "Slovak Republic" ~ "Slovakia",
      TRUE ~ Country.Name  # Keep other values unchanged
    ) )

colnames(fdi) <-  sub(".*(\\d{4}).*", "\\1", colnames(fdi))

fdi_long <- fdi |>
  pivot_longer(
    cols = -c(Country.Name, Series.Name, Series.Code, Country.Code),
    names_to = "year",
    values_to = "fdi"
  )

fdi_long_eu <- fdi_long |>
  filter(Country.Name %in% c("European Union"))

fdi_long_eu <- fdi_long_eu |> 
  rename(
    country = `Country.Name`,
    serie = `Series.Name`
  )

fdi_long_eu <- fdi_long_eu |>
  select(-c(Series.Code, `Country.Code`))


fdi_long_all <- fdi_long |>
  rename(
    country = `Country.Name`,
    serie = `Series.Name`
  )

fdi_long_all <- fdi_long_all |>
  select(-c(Series.Code, `Country.Code`))

fdi_long_all <- fdi_long_all |>
  filter(country %in% eu_countries)


## industry per gdp

industry_gdp <- read.csv("industry_gdp.csv", header = TRUE, sep = ",") |> 
  mutate(
  Country.Name = case_when(
    Country.Name == "Slovak Republic" ~ "Slovakia",
    TRUE ~ Country.Name  # Keep other values unchanged
  ) )

colnames(industry_gdp) <-  sub(".*(\\d{4}).*", "\\1", colnames(industry_gdp))

industry_gdp <- industry_gdp |> 
  rename(
    country = `Country.Name`,
    serie = `Series.Name`,
    country_code = `Country.Code`
  )

industry_gdp_all <- industry_gdp |> 
  filter(country %in% eu_countries)

# Step 1: Make sure column names are all characters
names(industry_gdp_all) <- as.character(names(industry_gdp_all))


# Step 2: Identify year columns (those that look like 4-digit numbers)
year_cols <- grep("^\\d{4}$", names(industry_gdp_all), value = TRUE)

# Step 3: Convert all year columns to numeric (or all to character if preferred)
industry_gdp_all[year_cols] <- lapply(industry_gdp_all[year_cols], as.numeric)

# Step 4: Pivot
industry_gdp_all_long <- industry_gdp_all |>
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "industry_gdp"
  )

# Step 5 (optional): Convert `year` column to integer
industry_gdp_all_long$year <- as.integer(industry_gdp_all_long$year)


## europe missing ###

industry_gdp_eu <- industry_gdp |>
  filter(country %in% c("European Union"))

names(industry_gdp_eu) <- as.character(names(industry_gdp_eu))
year_cols_eu <- grep("^\\d{4}$", names(industry_gdp_eu), value = TRUE)
industry_gdp_eu[year_cols_eu] <- lapply(industry_gdp_eu[year_cols_eu], as.numeric)

industry_gdp_eu_long <- industry_gdp_eu |>
  pivot_longer(
    cols = all_of(year_cols_eu),
    names_to = "year",
    values_to = "industry_gdp"
  )


## GVA ##

gva <- read.csv("gva_us$.csv", header = TRUE, sep = ",")

colnames(gva) <-  sub(".*(\\d{4}).*", "\\1", colnames(gva))
year_cols_gva <- grep("^\\d{4}$", names(gva), value = TRUE)
gva[year_cols_gva] <- lapply(gva[year_cols_gva], as.numeric)

gva_long <- gva |>
  pivot_longer(
    cols = all_of(year_cols_gva),
    names_to = "year",
    values_to = "gva"
  )

gva_long <- gva_long |> 
  mutate(
    Country.Name = case_when(
      Country.Name == "Slovak Republic" ~ "Slovakia",
      TRUE ~ Country.Name  # Keep other values unchanged
    ) )

gva_long <- gva_long |> 
  rename(
    country = `Country.Name` ,
    serie = `Series.Name`,
  )

gva_long <- gva_long |> 
  select(-c(Series.Code, `Country.Code`))

names(gva_long) <- as.character(names(gva_long))

gva_eu_long <- gva_long |>
  filter(country %in% c("European Union"))

gva_long_all <- gva_long |> 
  filter(country %in% eu_countries)


## CO2 Emissions ##

co2_owid <- read.csv("co2_owid.csv", header = TRUE, sep = ",")

co2_owid_all <- co2_owid |>
  filter(country %in% eu_countries & 
           year %in% c(1995:2020))

co2_owid_all <- co2_owid |>
  select(country, year, co2, iso_code, population)

co2_owid_eu <- co2_owid |>
  filter(country %in% c("Europe") &
           year %in% c(1995:2020)) |> 
  select(country, year, co2, iso_code, population) |> 
  mutate(iso_code = "EU")


## Carbon Pricing

## VARIABLE OF INTEREST ##

ent <- read.csv("en_tx.csv", header = TRUE, sep = ",")

ent <- ent |> 
  mutate(
    REF_AREA_NAME = case_when(
      REF_AREA_NAME == "Slovak Republic" ~ "Slovakia",
      TRUE ~ REF_AREA_NAME
    )
  )

ent <- ent |> 
  filter(REF_AREA_NAME %in% eu_countries)

ent <- ent |> 
  rename(
    country = `REF_AREA_NAME`,
    year = `TIME_PERIOD`,
    en_taxes = `OBS_VALUE`
  )

ent <- ent |> 
  filter(UNIT_MEASURE_NAME %in% c("Percentage of GDP"))

ent1 <- ent |> 
  select(country, year, en_taxes)





##################### ANOTHER DATASET FOR EN_TAXES OECD ########################
## everything is in Million

entax2 <- read.csv("en_tax.csv", header = TRUE, sep = ",")

entax2 <- entax2 |> 
  filter(
    tax == "Total environmental taxes" &
    unit == "Million euro"
  )

entax2 <- entax2 |> 
  select(geo, TIME_PERIOD, OBS_VALUE)

entax2 <- entax2 |> 
  rename(
    country = `geo`,
    year = `TIME_PERIOD`,
    en_taxes2 = `OBS_VALUE`
  )

entax2 <- entax2 |> 
  filter(country %in% eu_countries)


### EN_TAX FOR EUROPE

en_tax2_eu <- entax2 %>%
  group_by(year) %>%
  summarise(
    en_taxes2 = mean(en_taxes2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(country = "European Union")



################################################################################
################################################################################


## R&D subsidies ##

rd1 <- read.csv("rd_GDERD.csv", sep = ",") |> 
  rename(
    country = `geo`,
    year = `TIME_PERIOD`,
    rd = `OBS_VALUE`
  ) |> 
  filter(
    unit %in% c("Million euro") &
    sectperf %in% c("All sectors")
  )

## in million euro

rd1 <- rd1 |> 
  select(country, year, unit,  rd)


rd_eu <- rd1 |> 
  filter(country %in% c("European Union - 27 countries (from 2020)"))

rd_eu <- rd_eu |> 
  mutate(
    country = "Europe"
  )

rd_all <- rd1 |> 
  filter(country %in% eu_countries)

## how many data points between 2000 to 2019 I am missing




#### rd_gdp #####

rd_gdp <- read.csv("rd_gdp.csv", sep = ",") |> 
  rename(
    country = `geo`,
    year = `TIME_PERIOD`,
    rd_gdp = `OBS_VALUE`
  ) |> 
  filter(
    unit %in% c("Percentage of gross domestic product (GDP)") &
    sectperf %in% c("All sectors")
  )

rd_gdp <- rd_gdp |>
  select(
    country, year, rd_gdp
  ) |> 
  filter(
    country %in% eu_countries
  )

rd_final <- rd_gdp |>
  left_join(rd_all, by = c("country", "year")) |>
  select(country, year, rd_gdp, rd)


## CONTROL VARIABLES ##

## left join gdp_long, gva_long, co2_long MISSING ENERGY PRICE

gdp_pc_all$year <- as.character(gdp_pc_all$year)
gas_price_annual_all$year <- as.character(gas_price_annual_all$year)
gas_IEA$year <- as.character(gas_IEA$year)
gasoline_IEA$year <- as.character(gasoline_IEA$year)
co2_owid_all$year <- as.character(co2_owid_all$year)
industry_gdp_all_long$year <- as.character(industry_gdp_all_long$year)

control_vars_all <- gdp_long_all |>
  left_join(gva_long_all, by = c("country", "year")) |>
  left_join(co2_owid_all, by = c("country", "year")) |> 
  left_join(fdi_long_all, by = c("country", "year")) |> 
  left_join(gdp_pc_all, by = c("country", "year")) |>
  left_join(gas_IEA, by = c("country", "year")) |>
  left_join(gasoline_IEA, by = c("country", "year")) |>
  left_join(gas_price_annual_all, by = c("country", "year")) |> 
  left_join(industry_gdp_all_long, by = c("country", "year"))
  #left_join(energy_price1, by = "year")  ## not really intresting

class(co2_owid_all$year)
class(fdi_long$year)
class(gva_long$year)
class(energy_price1$year)
class(gdp_pc$year)

control_vars_all <- control_vars_all |>
  select( -c(serie.x, serie.y, serie, Series.Code, unit, na_item) )

## controls variables eu

## transform country in Europe

gdp_long_eu <- gdp_long_eu |>
  mutate(country = "European Union")

gva_eu_long <- gva_eu_long |>
  mutate(country = "European Union")

co2_owid_eu <- co2_owid_eu |>
  mutate(country = "European Union")

energy_price1 <- energy_price1 |>
  mutate(country = "European Union")

gdp_pc_eu <- gdp_pc_eu |>
  mutate(country = "European Union")

gdp_pc_eu$year <- as.character(gdp_pc_eu$year)
industry_gdp_eu_long$year <- as.character(industry_gdp_eu_long$year)
co2_owid_eu$year <- as.character(co2_owid_eu$year)

control_vars_eu <- gdp_long_eu |>
  left_join(gva_eu_long, by = c("country", "year")) |>
  left_join(co2_owid_eu, by = c("country", "year")) |> 
  left_join(fdi_long_eu, by = c("country", "year")) |> 
  left_join(gdp_pc_eu, by = c("country", "year")) |> 
  left_join(gas_price_eu_annual, by = c("country", "year")) |>
  left_join(industry_gdp_eu_long, by = c("country", "year"))
  #left_join(energy_price1, by = "year")

control_vars_eu <- control_vars_eu |>
  select( -c(serie.x, serie.y, serie, na_item, Series.Code, unit) )

control_vars_eu <- control_vars_eu |>
  rename(
    country = country.x
  )

## VARIABLE OF INTEREST ##

## left join eps_long, rd_long

rd_final$year <- as.integer(rd_final$year)
entax2$year <- as.integer(entax2$year)
ent1$year <- as.integer(ent1$year)

interest_vars_all <- rd_final |>
  left_join(entax2, by = c("country", "year")) |> 
  left_join(ent1, by = c("country", "year"))

interest_vars_all <- interest_vars_all |>
  mutate(year = as.character(year))

## change country in EU

rd_eu <- rd_eu |>
  mutate(country = "European Union")

class(eps_eu$year)
class(rd_eu$year)

rd_eu$year <- as.character(rd_eu$year)
en_tax2_eu$year <- as.character(en_tax2_eu$year)

interst_vars_eu <- en_tax2_eu |>
  left_join(rd_eu, by = c("country", "year"))

interst_vars_eu <- interst_vars_eu |>
  select(country, year, en_taxes2, rd)

final_df_eu <- control_vars_eu |>
  left_join(interst_vars_eu, by = c("country", "year")) |> 
  rename(
    rd_eu = rd
  )

################################################################################

class(control_vars_all$year)

rd_final$year <- as.character(rd_final$year)

final_df_all <- control_vars_all |>
  left_join(interest_vars_all, by = c("country", "year"))

final_df_all <- final_df_all |>
  mutate(
    fdi = case_when(
      fdi == ".." ~ NA_character_,
      TRUE ~ fdi
    ),
    fdi = as.numeric(fdi)
  )


rd_eu <- final_df_eu %>%
  filter(!is.na(rd_eu))

## Malta out because there are no data of green_patents

df1 <- final_df_all |> 
  filter(country != "Malta")

## check NAs 

 missi_rd <- df1 |> 
  filter(year %in% c(1995:2019))
  
sum(is.na(missi_rd$rd))


sum(is.na(missi_rd$en_taxes2))

## check for more observations
#df2 <- df1 |> 
  #filter(year %in% c(2000:2022))

rd_eu <- rd_eu |> 
  select( year, rd_eu)

df2 <- df1 |> 
  left_join(rd_eu, by = "year")

pop_missing_by_year <- final_df_all %>%
  filter(is.na(rd)) %>%
  group_by(year) %>%
  summarise(pop_missing = sum(population, na.rm = TRUE))

df3 <- df2 |> 
  left_join(pop_missing_by_year, by = "year")

df4 <- df3 %>%
  mutate(
    rd = case_when(
      is.na(rd) ~ rd_eu * population / pop_missing,
      TRUE ~ rd
    )
  )

df5 <- df4 |>
 # rename(
#    country = country.x
 # ) |> 
  select(-c(pop_missing, country_code, iso_code, rd_eu))


### FIND IF THE DATASET IS BALANCED ##

final_df_all |>
  group_by(country) |>
  summarise(n_years = n_distinct(year)) |>
  arrange(n_years)

## all have the same number of years, OK

final_df_all |>
  group_by(country, year) |>
  summarise(
    missing_gdp = sum(is.na(industry_gdp)),
    missing_fdi = sum(is.na(fdi)),
    missing_population = sum(is.na(population)),
    missing_eps = sum(is.na(eps)),
    missing_rd = sum(is.na(rd)),
    missing_gas_price = sum(is.na(gas_price)),
    missing_co2 = sum(is.na(co2)),
    missing_gva = sum(is.na(gva)),
    missing_gdp_pc = sum(is.na(gdp_pc)),
    .groups = "drop"
  ) |>
  filter(missing_gdp > 0 | missing_fdi > 0 | missing_population > 0)



## green patents

green_patents1 <- read.csv("green_patents.csv", sep = ";") |> 
  rename(
    country = filing_country,
    year = appln_filing_year,
    green_patents = NumberOfY02Patents
  )

green_patents_eu <- green_patents1 |>
  filter(country %in% "EP")

green_patents_eu <- green_patents_eu |>
  mutate(
    country = "European Union"
  )

green_patents_eu$year <- as.character(green_patents_eu$year)

final_df_eu <- final_df_eu |>
  left_join(green_patents_eu, by = c("country", "year"))

green_patents_all <- green_patents1 |> 
  mutate(
    country = case_when(
      country == "AT" ~ "Austria",
      country == "BE" ~ "Belgium",
      country == "BG" ~ "Bulgaria",
      country == "HR" ~ "Croatia",
      country == "CY" ~ "Cyprus",
      country == "CZ" ~ "Czechia",
      country == "DK" ~ "Denmark",
      country == "EE" ~ "Estonia",
      country == "FI" ~ "Finland",
      country == "FR" ~ "France",
      country == "DE" ~ "Germany",
      country == "GR" ~ "Greece",
      country == "HU" ~ "Hungary",
      country == "IE" ~ "Ireland",
      country == "IT" ~ "Italy",
      country == "LV" ~ "Latvia",
      country == "LT" ~ "Lithuania",
      country == "LU" ~ "Luxembourg",
      country == "MT" ~ "Malta",
      country == "NL" ~ "Netherlands",
      country == "PL" ~ "Poland",
      country == "PT" ~ "Portugal",
      country == "RO" ~ "Romania",
      country == "SK" ~ "Slovakia",
      country == "SI" ~ "Slovenia",
      country == "ES" ~ "Spain",
      country == "SE" ~ "Sweden",
      TRUE ~ NA_character_
    )
  )

tot_gp <- green_patents_all |>
  filter(country %in% eu_countries)

tot_gp$year <- as.character(tot_gp$year)

tot_pat <- read.csv("tot_pat.csv", sep = ";") |> 
  rename(
    country = country_code,
    year = appln_filing_year,
    total_patents = number_of_applications
  )

tot_pat <- tot_pat |>
  mutate(
    country = case_when(
      country == "AT" ~ "Austria",
      country == "BE" ~ "Belgium",
      country == "BG" ~ "Bulgaria",
      country == "HR" ~ "Croatia",
      country == "CY" ~ "Cyprus",
      country == "CZ" ~ "Czechia",
      country == "DK" ~ "Denmark",
      country == "EE" ~ "Estonia",
      country == "FI" ~ "Finland",
      country == "FR" ~ "France",
      country == "DE" ~ "Germany",
      country == "GR" ~ "Greece",
      country == "HU" ~ "Hungary",
      country == "IE" ~ "Ireland",
      country == "IT" ~ "Italy",
      country == "LV" ~ "Latvia",
      country == "LT" ~ "Lithuania",
      country == "LU" ~ "Luxembourg",
      country == "MT" ~ "Malta",
      country == "NL" ~ "Netherlands",
      country == "PL" ~ "Poland",
      country == "PT" ~ "Portugal",
      country == "RO" ~ "Romania",
      country == "SK" ~ "Slovakia",
      country == "SI" ~ "Slovenia",
      country == "ES" ~ "Spain",
      country == "SE" ~ "Sweden"
    ))

tot_pat <- tot_pat |>
  filter(country %in% eu_countries)


tot_pat$year <- as.character(tot_pat$year)

df_all <- df1 |> 
  left_join(tot_gp, by = c("country", "year")) |> 
  left_join(tot_pat, by = c("country", "year"))


## drop out Malta, just one observation for green patents


write.csv(
  df_all,
  "C:/Users/Utente/Documents/GitHub/green-innovation-data-project/processed_data/final_df_all.csv",
  row.names = FALSE
)

write.csv(final_df_eu,
          "C:/Users/Utente/Documents/GitHub/green-innovation-data-project/processed_data/final_df_eu.csv", 
          row.names = FALSE)

# CHECK THE VARIABLE OF INTEREST: 
# 1) FIRST: CHECK WHICH CURRENCY IS BEST FOR R&D -> there is no SLOVENIA
# 2) SECOND: CHECK THE MEASURE OF EPS COINCIDE WITH EVERYTHING ELS -> currency is different
