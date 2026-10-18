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
library(kableExtra)
library(fixest)

final_df <- read.csv("C:\\Users\\Utente\\Desktop\\Data Thesis\\final_df_all.csv", sep = ",")


pdata_final <- final_df |>
  arrange(country, year) |>
  group_by(country) |>
  mutate(
    rd_lag1 = dplyr::lag(rd, 1),
    rd_lag2 = dplyr::lag(rd, 2),
    rd_lag3 = dplyr::lag(rd, 3),
    rd_lag4 = dplyr::lag(rd, 4),
    entax_lag1 = dplyr::lag(en_taxes2, 1),
    entax_lag2 = dplyr::lag(en_taxes2, 2),
    entax_lag3 = dplyr::lag(en_taxes2, 3),
    entax_lag4 = dplyr::lag(en_taxes2, 4),
    co2_lag1 = dplyr::lag(co2, 1),
    co2_lag2 = dplyr::lag(co2, 2),
    co2_lag3 = dplyr::lag(co2, 3),
    industry_lag1 = dplyr::lag(industry_gdp, 1),
    industry_lag2 = dplyr::lag(industry_gdp, 2),
    industry_lag3 = dplyr::lag(industry_gdp, 3),
    fdi_lag1 = dplyr::lag(fdi, 1),
    fdi_lag2 = dplyr::lag(fdi, 2),
    fdi_lag3 = dplyr::lag(fdi, 3),
    gas_price_lag1 = dplyr::lag(gas_price, 1),
    gas_price_lag2 = dplyr::lag(gas_price, 2),
    gas_price_lag3 = dplyr::lag(gas_price, 3),
    #gva_lag1 = dplyr::lag(gva, 1),
    #gva_lag2 = dplyr::lag(gva, 2),
    #gva_lag3 = dplyr::lag(gva, 3),
    gdp_pc_lag1 = dplyr::lag(gdp_pc, 1),
    gdp_pc_lag2 = dplyr::lag(gdp_pc, 2),
    gdp_pc_lag3 = dplyr::lag(gdp_pc, 3),
    population_lag1 = dplyr::lag(population, 1),
    population_lag2 = dplyr::lag(population, 2),
    population_lag3 = dplyr::lag(population, 3)
  ) |>
  ungroup()

pdata_final <- pdata_final |>
  filter(year %in% 1995:2019)

pdata_final <- pdata.frame(pdata_final, index = c("country", "year"))

### simple regression model with lagged variables ###

model_fixest <- feglm(log(green_patents) ~  log(rd) + log(en_taxes2) + log(gas_price) + log(fdi) +
                        log(industry_gdp) + log(co2) + log(population) + log(gdp_pc)
                      | country + year,
                      data = pdata_final)

summary(model_fixest, robust = TRUE)

model1 <- feols(log(green_patents) ~ log(rd_lag1) + log(entax_lag1) + log(gas_price)
             + log(co2) + log(fdi) + log(industry_gdp) + 
             + log(population) + log(gdp_pc) 
             | country + year,
             cluster = ~country + year,
              data = pdata_final)


summary(model1)

###################

stargazer(model1, type = "text", 
           title = "Simple Regression Model with Lagged Variables",
           dep.var.labels = "Green Patents",
           covariate.labels = c("R&D Lag 1", "Energy Taxes Lag 1", "Industry GDP", "FDI", "CO2 Emissions", "Gas Price"),
           model.numbers = FALSE,
           omit.stat = c("f", "ser"),
           digits = 3,
           add.lines = list(c("Robust Standard Errors", "Yes")),
           notes = "Robust standard errors are calculated using the HC1 method.",
           notes.append = FALSE,
           star.char = c("*", "**", "***"))

model2 <- feols(log(green_patents) ~ log(rd_lag2) + log(entax_lag2) + log(gas_price)
             + log(fdi) + log(industry_gdp) +
             + log(co2)
            
             + log(population) + log(gdp_pc) 
             | country + year,
             cluster = ~country + year,
              data = pdata_final)
summary(model2)


model3 <- feols(log(green_patents) ~ log(rd_lag3) + log(entax_lag3) + log(gas_price)
             + log(fdi) + log(industry_gdp) +
             + log(co2)
             + log(population) + log(gdp_pc) 
             | country + year,
             cluster = ~country,
              data = pdata_final)

summary(model3)

model4 <- feols(log(green_patents) ~ log(rd_lag4) + log(entax_lag4) + log(gas_price)
             + log(fdi) + log(industry_gdp) +
             + log(co2)
             + log(population) + log(gdp_pc) 
             | country + year,
              data = pdata_final)

summary(model4)

etable(model1, model2, model3,
       title = "Regression Results with Lagged Variables",
       cluster = ~country,
       digits = 2,
       tex = TRUE)

etable(model1, model2, model3,
       se = "cluster",
       cluster = "country",
       drop = "Intercept",           # optional, removes the constant
       fitstat = ~ n + r2  + f,
       headers = c("Lag 1", "Lag 2", "Lag 3"),
       tex = TRUE)

etable(model1, model2, model3,
       se = "cluster",
       cluster = "country",
       dict = c("log(rd_lag1)" = "log(rd)",
                "log(rd_lag2)" = "log(rd)",
                "log(rd_lag3)" = "log(rd)",
                "log(entax_lag1)" = "log(en_taxes2)",
                "log(entax_lag2)" = "log(en_taxes2)",
                "log(entax_lag3)" = "log(en_taxes2)",
                "log(gas_price_lag1)" = "Gas Price",
                "log(gas_price_lag2)" = "Gas Price",
                "log(gas_price_lag3)" = "Gas Price",
                "log(fdi_lag1)" = "FDI",
                "log(fdi_lag2)" = "FDI",
                "log(fdi_lag3)" = "FDI",
                "log(industry_lag1)" = "Industry GDP",
                "log(industry_lag2)" = "Industry GDP",
                "log(industry_lag3)" = "Industry GDP",
                "log(co2_lag1)" = "CO2 Emissions",
                "log(co2_lag2)" = "CO2 Emissions",
                "log(co2_lag3)" = "CO2 Emissions",
                "log(population_lag1)" = "Population",
                "log(population_lag2)" = "Population",
                "log(population_lag3)" = "Population",
                "log(gdp_pc_lag1)" = "GDP per Capita",
                "log(gdp_pc_lag2)" = "GDP per Capita",
                "log(gdp_pc_lag3)" = "GDP per Capita"
                
       ),
       fitstat = ~ n + r2 + f,
       headers = c("Lag 1", "Lag 2", "Lag 3"),
       tex = TRUE)


etable(model1, model2, model3,
       se = "cluster",
       cluster = "country",
       keep = c("%log\\(rd_lag1\\)", "%log\\(rd_lag2\\)", "%log\\(rd_lag3\\)",
                "%log\\(entax_lag1\\)", "%log\\(entax_lag2\\)", "%log\\(entax_lag3\\)"),
       dict = c(
         "log(rd_lag1)" = "R&D",
         "log(rd_lag2)" = "R&D",
         "log(rd_lag3)" = "R&D",
         "log(entax_lag1)" = "Energy Taxes",
         "log(entax_lag2)" = "Energy Taxes",
         "log(entax_lag3)" = "Energy Taxes"
       ),
       fitstat = ~ n + r2 + f,
       headers = c("Lag 1", "Lag 2", "Lag 3"),
       extralines = list(
         "Covariates" = c("Full", "Full", "Full")
       ),
       tex = TRUE)
