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

## check if it is balanced ##

## first of all, keep just the variable we care about


pdata_all <- pdata.frame(eu, index = c("country", "year"))

is.pbalanced(pdata_all)

pdata_final <- pdata.frame(pdata_all, index = c("country", "year"))


############### Negative binomial regression model #############
## useful because it includes 0s and 1s, and good for proportions derived from counts

library(MASS)

## TOO much overdispersion

model_pois <- fepois(green_patents ~ log(rd) + log(en_taxes2) + 
                      log(gas_price) + log(industry_gdp) + log(fdi) +
                      log(co2) + log(population) + log(gdp_pc) | 
                      country + year,
                    cluster = ~country,
                    data = pdata_all)

summary(model_pois)

model_pois2 <- fepois(green_patents ~ log(rd) + 
                      log(gas_price) + log(industry_gdp) + log(fdi) +
                      log(co2) + log(population) + log(gdp_pc) | 
                      country + year,
                    cluster = ~country,
                    data = pdata_all)

summary(model_pois2)

model_pois3 <- fepois(green_patents ~ log(en_taxes2) + 
                       log(gas_price) + log(industry_gdp) + log(fdi) +
                       log(co2) + log(population) + log(gdp_pc) | 
                     country +
                       year,
                     cluster = ~country,
                     data = pdata_all)

summary(model_pois3)

model_pois4 <- fepois(green_patents ~ log(rd) + log(en_taxes2) + 
                       log(gas_price) + log(industry_gdp) + log(fdi) +
                       log(co2) + log(population) + log(gdp_pc) + log(rd)*log(en_taxes2) | 
                        country + year,
                     cluster = ~country,
                     data = pdata_all)

summary(model_pois4)

etable(model_pois, model_pois2, model_pois3, model_pois4,
       tex = TRUE
)

etable(model_pois, model_pois2, model_pois3, model_pois4,
       tex = TRUE,
       extralines = list(
         "Covariates" = c("Full", "Full", "Full", "Full")
       )
)

## Try FE with total patents 

model1 <- feols(log(total_patents) ~ log(rd) + log(en_taxes2) + 
                    log(gas_price) + log(industry_gdp) + 
                    log(co2) + log(fdi) + log(population) + log(gdp_pc) | 
                    country + year,
                  cluster = ~country,
                  data = pdata_all)

summary(model1)

model2 <- feols(log(total_patents) ~ log(rd) + 
                    log(gas_price) + log(industry_gdp) + 
                    log(co2) + log(fdi) + log(population) + log(gdp_pc) 
                    
                  |year + country,
                  cluster = ~country,
                  data = pdata_all)

summary(model2)

model3 <- feols(log(total_patents) ~  log(en_taxes2) + 
                    log(gas_price) + log(industry_gdp) + 
                    log(co2) + log(fdi) + log(population) + log(gdp_pc) 
                    
                  |country+ year,
                  cluster = ~country,
                  data = pdata_all)

summary(model3)

model4 <- feols(log(total_patents) ~ log(rd) + log(en_taxes2) + 
                    log(gas_price) + log(industry_gdp) + 
                    log(co2) + log(fdi) + log(population) + log(gdp_pc) +
                    log(en_taxes2)*log(rd)
                  | country + year,
                  cluster = ~country,
                  data = pdata_all)

summary(model4)


etable(model1, model2, model3, model4,
       tex = TRUE
)


## 

model <- feols(log(green_patents) ~  log(rd) + log(en_taxes2) + log(fdi) + log(industry_gdp) + 
              log(co2) + log(population)
      | country + year,
      data = pdata_final)

summary(model)

eta

############## model with share of green patents ###############################

pdata_all$green_patents_share <- pdata_all$green_patents / pdata_all$total_patents

model_share <- feols(green_patents_share ~ log(rd) + log(en_taxes2) + 
                             log(gas_price) + log(industry_gdp) + 
                             log(co2) + log(fdi) + log(population) + log(gdp_pc) | 
                             country + year,
                           cluster = ~country,
                           data = pdata_all)
summary(model_share)

modelshare2 <- feols(green_patents_share ~ log(rd) + 
                              log(gas_price) + log(industry_gdp) + 
                              log(co2) + log(fdi) + log(population) + log(gdp_pc) | 
                              country + year,
                            cluster = ~country,
                            data = pdata_all)

summary(modelshare2)

modelshare3 <- feols(green_patents_share ~ log(en_taxes2) + 
                              log(gas_price) + log(industry_gdp) + 
                              log(co2) + log(fdi) + log(population) + log(gdp_pc) | 
                              country + year,
                            cluster = ~country,
                            data = pdata_all)

summary(modelshare3)

etable(model_share, modelshare2, modelshare3,
       tex = TRUE
)


model_nb_share_int <- fenegbin(green_patents_share ~ log(rd) + log(en_taxes2) + 
                                 log(gas_price) + log(industry_gdp) + 
                                 log(co2) + log(fdi) + log(population) + log(gdp_pc) +
                                 log(en_taxes2) * log(rd)
                               | country + year,
                               cluster = ~country,
                               data = pdata_all)
summary(model_nb_share_int)

#################################################################################

update.packages(ask = FALSE)
