library(tidyverse)
library(dplyr)
library(readxl)
library(car)
library(ggplot2)
library(corrplot)
library(sandwich)
library(lmtest)
library(margins)
library(plm)
library(stargazer)
library(fixest)

final_df <- read.csv("C:\\Users\\Utente\\Desktop\\Data Thesis\\final_df_all.csv", sep = ",")

eu <- final_df |> 
  filter(year %in% 1995:2019)

eu$year <- as.numeric(eu$year)

eu <- eu |> 
  mutate(paris = ifelse(year >= 2015, 1, 0), 
         eco_crisis = ifelse(year >= 2008, 1, 0))

pdata_all <- pdata.frame(eu, index = c("country", "year"))

## Paris Agreement in 2015 

after_paris <-  eu |> 
  filter(year >= 2015)

before_paris <- eu |>
  filter(year < 2015)

model_before <- feols(log(green_patents) ~ log(rd_gdp) + log(en_taxes) + 
                     log(gas_price) + log(fdi) +
                     log(co2) + log(population) + log(gdp_pc) | 
                     country + year,
                     data = before_paris)

summary(model_before)

model_after <- feols(log(green_patents) ~ log(rd_gdp) + log(en_taxes) + 
                     log(gas_price) + log(fdi) +
                     log(co2) + log(population) + log(gdp_pc) | 
                     country + year,
                     data = after_paris)

summary(model_after)

model_paris <- feols(log(green_patents) ~ log(rd) * paris + log(en_taxes2) * paris + 
                     log(gas_price) + log(fdi) + log(industry_gdp) +
                     log(co2) + log(population) + log(gdp_pc) | 
                     country + year,
                     data = pdata_all)

summary(model_paris)

model_paris2 <- feols(log(green_patents) ~ log(rd) * paris +
                        log(gas_price) + log(fdi) + log(industry_gdp) +
                        log(co2) + log(population) + log(gdp_pc) | 
                        country + year,
                      data = pdata_all)

summary(model_paris2)

model_paris3 <- feols(log(green_patents) ~ log(en_taxes2) * paris +
                        log(gas_price) + log(fdi) + log(industry_gdp) +
                        log(co2) + log(population) + log(gdp_pc) | 
                        country + year,
                      data = pdata_all)

summary(model_paris3)

etable(model_paris,model_paris2,model_paris3,
       cluster = "country",
       se = "cluster",
       fitstat = ~ n + r2 + f,
       tex = TRUE)

## Economic crisis in 2008

model_crisis <- feols(log(green_patents) ~ log(rd) * eco_crisis + log(en_taxes2) * eco_crisis + 
                     log(gas_price) + log(fdi) + log(industry_gdp) +
                     log(co2) + log(population) + log(gdp_pc) | 
                     country + year,
                     data = pdata_all)

summary(model_crisis)

model_crisis2 <- feols(log(green_patents) ~ log(rd) * eco_crisis +
                        log(gas_price) + log(fdi) + log(industry_gdp) +
                        log(co2) + log(population) + log(gdp_pc) | 
                        country + year,
                      data = pdata_all)

summary(model_crisis2)

model_crisis3 <- feols(log(green_patents) ~ log(en_taxes2) * eco_crisis +
                        log(gas_price) + log(fdi) + log(industry_gdp) +
                        log(co2) + log(population) + log(gdp_pc) | 
                        country + year,
                      data = pdata_all)

summary(model_crisis3)

etable(model_crisis, model_crisis2, model_crisis3,
       cluster = "country",
       se = "cluster",
       tex = TRUE,
       fitstat = ~ n + r2 + f
       )



## 10% rich vs 10% poor countries

median_gdp <- median(pdata_all$gdp_pc, na.rm = TRUE)
pdata_all$rich <- ifelse(pdata_all$gdp_pc > median_gdp, 1, 0)

model_rich_het <- feols(
  log(green_patents) ~ log(rd) * rich + log(en_taxes2) * rich +
    log(gas_price) + log(fdi) + log(industry_gdp) +
    log(co2) + log(population) + log(gdp_pc) | 
     year + country,
  cluster = ~country,
  data = pdata_all
)

summary(model_rich_het)

model_rich2 <- feols(
  log(green_patents) ~ log(rd) * rich +
    log(gas_price) + log(fdi) + log(industry_gdp) +
    log(co2) + log(population) + log(gdp_pc) | 
    year + country,
  cluster = ~country,
  data = pdata_all
)

summary(model_rich2)

model_rich3 <- feols(
  log(green_patents) ~ log(en_taxes2) * rich +
    log(gas_price) + log(fdi) + log(industry_gdp) +
    log(co2) + log(population) + log(gdp_pc) | 
    year + country,
  cluster = ~country,
  data = pdata_all
)

summary(model_rich3)

etable(model_rich_het, model_rich2, model_rich3,
       tex = TRUE,
       title = "Heterogeneity in Green Innovation: Rich vs Poor Countries",
       cluster = "country",
       digits = 3)

etable(model_rich_het,
       se = "cluster",
       fitstat = ~ n + r2 + f,
       tex = TRUE
       )


## innovative vs. not that innovative countries

gii_2024 <- c(
  Austria = 50.3,
  Belgium = 47.7,
  Bulgaria = 35.2,
  Croatia = 37.3,
  Cyprus = 41.6,
  Czechia = 43.2,
  Denmark = 56.3,
  Estonia = 52.3,
  Finland = 59.4,
  France = 55.4,
  Germany = 58.1,
  Greece = 36.2,
  Hungary = 39.1,
  Ireland = 50.0,
  Italy = 44.9,
  Latvia = 36.4,
  Lithuania = 40.1,
  Luxembourg = 49.1,
  Malta = 43.7,
  Netherlands = 58.8,
  Poland = 39.0,
  Portugal = 44.2,
  Romania = 34.9,
  Slovakia = 34.3,
  Slovenia = 40.2,
  Spain = 45.6,
  Sweden = 61.6
)

pdata_all$gii <- gii_2024[as.character(pdata_all$country)]

pdata_all$innovative <- ifelse(pdata_all$gii > 45, 1, 0)

model_innovative_het <- feols(
  log(green_patents) ~ log(rd) * innovative + log(en_taxes2) * innovative +
    log(gas_price) + log(fdi) + log(industry_gdp) +
    log(co2) + log(population) + log(gdp_pc) | 
    year + country,
  cluster = ~country,
  data = pdata_all
)
summary(model_innovative_het)

model_inn2 <- feols(
  log(green_patents) ~ log(rd) * innovative +
    log(gas_price) + log(fdi) + log(industry_gdp) +
    log(co2) + log(population) + log(gdp_pc) | 
    year + country,
  cluster = ~country,
  data = pdata_all
)

summary(model_inn2)

model_inn3 <- feols(
  log(green_patents) ~ log(en_taxes2) * innovative +
    log(gas_price) + log(fdi) + log(industry_gdp) +
    log(co2) + log(population) + log(gdp_pc) | 
    year + country,
  cluster = ~country,
  data = pdata_all
)

summary(model_inn3)

etable(model_innovative_het,model_inn2,model_inn3,
       tex = TRUE,
       fitstat = ~ n + r2 + f)
       

## 10% polluted vs. 10% less polluted countries

median_co2 <- median(pdata_all$co2, na.rm = TRUE)

pdata_all$polluted <- ifelse(pdata_all$co2 > median_co2, 1, 0)

model_polluted_het <- feols(
  log(green_patents) ~ log(rd) * polluted + log(en_taxes2) * polluted +
    log(gas_price) + log(fdi) + log(industry_gdp) +
    log(co2) + log(population) + log(gdp_pc) | 
    year + country,
  data = pdata_all
)

summary(model_polluted_het)

model_poll2 <- feols(
  log(green_patents) ~ log(rd) * polluted +
    log(gas_price) + log(fdi) + log(industry_gdp) +
    log(co2) + log(population) + log(gdp_pc) | 
    year + country,
  data = pdata_all
)

summary(model_poll2)

model_poll3 <- feols(
  log(green_patents) ~ log(en_taxes2) * polluted +
    log(gas_price) + log(fdi) + log(industry_gdp) +
    log(co2) + log(population) + log(gdp_pc) | 
    year + country,
  data = pdata_all
)

summary(model_poll3)

etable(model_polluted_het, model_poll2, model_poll3,
       se = "cluster",
       fitstat = ~ n + r2 + f,
       tex = TRUE)


pdata_all <- pdata_all %>%
  mutate(group = case_when(
    rich == 1 & polluted == 1 ~ "Rich & Polluted",
    rich == 1 & polluted == 0 ~ "Rich & Clean",
    rich == 0 & polluted == 1 ~ "Poor & Polluted",
    rich == 0 & polluted == 0 ~ "Poor & Clean"
  )) %>%
  mutate(group = factor(group, levels = c("Rich & Polluted", "Rich & Less Polluted",
                                          "Poor & Polluted", "Poor & Less Polluted")))

pdata_all <- pdata_all %>%
  mutate(year = as.numeric(as.character(year)))

ggplot(pdata_all, aes(x = year, y = log(green_patents + 1), color = group)) +
  stat_summary(fun = "mean", geom = "line", size = 1.2) +
  labs(
    title = "Average Green Patents by Country Group",
    subtitle = "Rich/Poor × Polluted/Clean classification",
    x = "Year", y = "Log(Average Green Patents)"
  ) +
  scale_color_manual(values = c(
    "Rich & Polluted" = "#E64B35",
    "Rich & Clean" = "#4DBBD5",
    "Poor & Polluted" = "#00A087",
    "Poor & Clean" = "#3C5488"
  )) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", legend.title = element_blank())

# Save the results to a LaTeX file

library(modelsummary)

models <- list(
  "Paris" = model_paris,
  "Crisis" = model_crisis,
  "Rich Heterogeneity" = model_rich_het,
  "Innovative Heterogeneity" = model_innovative_het,
  "Polluted Heterogeneity" = model_polluted_het
)

modelsummary(models,
             output = "latex",
             title = "Regression Models on Green Innovation",
             stars = TRUE)


