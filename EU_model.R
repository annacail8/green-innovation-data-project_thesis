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

final_df <- read.csv("C:\\Users\\Utente\\Documents\\GitHub\\green-innovation-data-project\\processed_data\\final_df_all.csv", sep = ",")

eu <- final_df |> 
  filter(year %in% 1995:2019)

## check if it is balanced ##

## first of all, keep just the variable we care about

pdata_final <- eu |> 
  select(country, year, green_patents, en_taxes2, rd, industry_gdp, co2, gas_price, fdi, gdp_pc, population, total_patents)


pdata_final <- pdata.frame(eu, index = c("country", "year"))


library(naniar)
vis_miss(pdata_final)

# check NAs

sum(is.na(pdata_final))

sapply(pdata_final, anyNA)
sapply(pdata_final, function(x) sum(is.na(x)))

## how many rows I loose because of NAs?
pdata_NAs <- pdata_final[complete.cases(pdata_final),]

colSums(is.na(pdata_final))


library(summarytools)

table1 <- dfSummary(pdata_final)

stargazer(
  pdata_final,
  type = "latex",
  summary = TRUE,
  title = "Summary Statistics",
  label = "tab:sumstats",
  out = "summary_stats.tex"  # optional
)

## summary statistics

stargazer(pdata_final, type = "latex", summary.stat = c("n", "mean", "sd", "min", "max"))

## Prelimanary analysis

pdata_final <- pdata_final |> 
  filter(
    gasoline_price_IEA > 0
  )

## simple regression

model1 <- lm(log(green_patents) ~ log(rd) + log(en_taxes2) + log(industry_gdp) + log(co2)
            + log(gas_price)
            + log(fdi) 
            + log(gdp_pc) + log(population),
             data = pdata_final)

data_model1 <- model.frame(model1)
summary(model1)

## print in latex

stargazer(model1,
          type = "latex",
          title = "Regression Results",
          dep.var.labels = "Green Patents",
          covariate.labels = c("R&D Expenditure", 
                               "Environmental Taxes", 
                               "Industry GDP", 
                               "CO2", 
                               "Gas Price", 
                               "GDP per Capita", 
                               "Population"),
          omit.stat = "ser",
          digits = 2,
          align = TRUE,
          no.space = TRUE,
          add.lines = list(
            c("Observations", nobs(model1))
          )
)

## without log #################################################################

model1_nolog <- lm(green_patents ~ rd + en_taxes2 + industry_gdp + co2
                 + gas_price 
                 + gasoline_price_IEA
                 + fdi 
                 + gdp_pc + population,
               data = pdata_final)

summary(model1_nolog)

###############################################################################

## print the model with covariates FULL

stargazer(model1,
          type = "text",
          title = "Regression Results",
          dep.var.labels = "Green Patents",
          covariate.labels = c("R&D Expenditure", 
                               "Environmental Taxes", 
                               "Industry GDP", 
                               "CO2", 
                               "Gas Price", 
                               "GDP per Capita", 
                               "Population"),
          omit.stat = "ser",
          digits = 2,
          align = TRUE,
          no.space = TRUE,
          add.lines = list(
            c("Observations", nobs(model1))
          )
)

stargazer(model1,
          type = "text",
          title = "Regression Results",
          dep.var.labels = "Green Patents",
          covariate.labels = c("R&D Expenditure", "Environmental Taxes"),
          keep = c("R&D_Expenditure", "Environmental_Taxes"), # match variable names in model
          omit.stat = "ser",
          digits = 2,
          align = TRUE,
          no.space = TRUE,
          add.lines = list(
            c("Covariates", "FULL"),
            c("Observations", nobs(model1))
          )
)

###############################################################################

stargazer(model1, type = "text", 
          title = "Regression Results",
          dep.var.labels = "Green Patents",
          covariate.labels = c("R&D Expenditure", "Environmental Taxes", "FDI", "Industry GDP", "CO2", "Gas Price"),
          omit.stat =  "ser",
          digits = 2,
          align = TRUE,
          no.space = TRUE,
          add.lines = list(c("Observations", nrow(pdata_final))))

## corr plot

corr_matrix <- cor(pdata_final[, c("green_patents","en_taxes2", "rd", "fdi", "industry_gdp", "co2", "gas_price", "gdp_pc", "population")], use = "complete.obs")
corr_matrix
corrplot(corr_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7,
         diag = FALSE)

model2 <- lm(log(green_patents) ~ log(rd) + log(en_taxes2) + log(industry_gdp) + log(co2) + log(gas_price) + log(fdi)
             + log(gdp_pc) + log(population), data = pdata_final)

summary(model2)
vif2 <- vif(model2)

model3 <- lm(log(green_patents) ~ log(en_taxes2)  + log(industry_gdp) + log(co2) + log(gas_price) + log(fdi)
             + log(gdp_pc) + log(population), data = pdata_final)
summary(model3)
vif3 <- vif(model3)

model4 <- lm(log(green_patents) ~ log(rd) + log(industry_gdp) + log(gas_price) + log(co2) + log(fdi)
             + log(gdp_pc) + log(population), data = pdata_final)
summary(model4)
vif4 <- vif(model4)

model5 <- lm(log(green_patents) ~ log(en_taxes2) + log(rd) + log(industry_gdp) + log(co2) + log(fdi)
             + log(gas_price) + log(gdp_pc) + log(population) + log(en_taxes2)*log(rd), data = pdata_final)

summary(model5)

vif5 <- vif(model5, type = "predictor")

stargazer(model2, model3, model4, model5, 
          title = "Regression Results",
          dep.var.labels = "Green Patents",
          #covariate.labels = c("R&D Expenditure", "Environmental Taxes", "FDI", "Industry GDP", "Gas Price", "CO2", "GDP per Capita", "Population"),
          type = "latex",
          digits = 2,
          add.lines = list(c("Observations", nrow(pdata_final))),
          out = "regression_results.txt")

stargazer(model1, model2, model3, model4,
          type = "latex",
          keep = c("log\\(rd\\)", "log\\(en_taxes2\\)", "log\\(en_taxes2\\):log\\(rd\\)"),
          dep.var.labels = "Green Patents",
          covariate.labels = c("log(rd)", "log(en_taxes2)", "log(en_taxes2):log(rd)"),
          omit.stat = c("ll", "aic", "bic"),
          no.space = TRUE
         )

stargazer(model2, model3, model4, model5,
          type = "latex",
          keep = c("log\\(rd\\)", "log\\(en_taxes2\\)", "log\\(en_taxes2\\):log\\(rd\\)"),
          dep.var.labels = "Green Patents",
          covariate.labels = c("log(rd)", "log(en_taxes2)", "log(en_taxes2):log(rd)"),
          omit.stat = c("ll", "aic", "bic"),
          no.space = TRUE)


############################### TRY WITH FE ####################################

model_fixest <- feols(log(green_patents) ~  log(rd) + log(en_taxes2) + log(gas_price) + log(fdi) +
                      log(industry_gdp) + log(co2) + log(population) + log(gdp_pc)
                      | country + year,
                      data = pdata_final)

summary(model_fixest, robust = TRUE)

## check variable with NA or 0

sapply(pdata_final, function(x) sum(is.na(x)))
sapply(pdata_final, function(x) sum(x == 0, na.rm = TRUE))


model_fixest2 <- feols(log(green_patents) ~ log(rd) + log(en_taxes2) + log(gas_price) + log(co2) + log(fdi) +
                       + log(industry_gdp) + log(population) + log(gdp_pc) + log(en_taxes2)*log(rd)
                       | country + year,
                       cluster = ~country,
                       data = pdata_final)

summary(model_fixest2, robust = TRUE)


#model_fixest3 <- feols(green_patents ~ en_taxes2 + rd  + industry_gdp + co2 + gas_price
                      #| #country + year,
                      #data = pdata_all)

#summary(model_fixest3, robust = TRUE, cluster = ~country)

model_fixest4 <- feols(log(green_patents) ~ log(rd) + log(co2) + 
                         log(fdi) +
                         log(industry_gdp) +
                       + log(gas_price) + log(population) + log(gdp_pc)
                      | country + year,
                      cluster = ~country,
                      data = pdata_final)

summary(model_fixest4, robust = TRUE, cluster = ~country)


model_fixest5 <- feols(log(green_patents) ~ log(en_taxes2) + log(industry_gdp) + log(co2) + log(fdi) 
                       + log(gas_price) + log(population) + log(gdp_pc)
                      | country + year,
                      cluster = ~country,
                      data = pdata_final)

summary(model_fixest5, robust = TRUE, cluster = ~country)

library(etable)

etable(model_fixest, model_fixest4, model_fixest5, model_fixest2,
       #digits       = 2,
       tex          = TRUE,
       title        = "Fixed Effects Regression Model Results",
       fitstat      = ~ n + r2 + f,
       signif.code  = c("***" = .01, "**" = .05, "*" = .1))

etable(model_fixest, model_fixest4, model_fixest5, model_fixest2,
       se = "cluster",
       cluster = "country",
       dict = c("log(rd)" = "log(rd)",
                "log(en_taxes2)" = "log(en_taxes2)",
                "log(rd):log(en_taxes2)" = "log(rd) × log(en_taxes2)"),
       drop = "Intercept",
       fitstat = ~ n + r2  + f,
       headers = c("(1)", "(2)", "(3)", "(4)"),
       tex = TRUE,
       extralines = list(
         "Covariates" = c("Full", "Full", "Full", "Full")
       ))



