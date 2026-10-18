library(tidyverse)
library(dplyr)
library(readxl)

library(car)
library(ggplot2)
library(corrplot)


################# European Union Trends #####################

# Load the dataset

df <- read.csv("C:/Users/Utente/Documents/GitHub/green-innovation-data-project/processed_data/final_df_eu.csv", sep = ",")

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


df_pat <- tot_pat

df_pat$total_patents <- as.numeric(df_pat$total_patents)
df_pat$year <- as.numeric(df_pat$year)

tot_pat_eu <- df_pat |>
  group_by(year) |>
  mutate(
    tot_eu = sum(total_patents, na.rm = TRUE)
  )

df_eu <- df |>
  left_join(tot_pat_eu, by = c("year"))

df_eu <- df_eu |> 
  dplyr::select(-country.y) |> 
  rename(
    country = country.x
  )


df_eu |> 
  filter(year %in% c(1995:2019)) |>
  ggplot(aes(y = green_patents, x = en_taxes2)) +
  geom_point(aes(y = green_patents, x = en_taxes2), color = "red") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation between Environmental Taxes and Total Green Patents",
       x = "Environmental Taxes",
       y = "Total Green Patents") +
  theme_minimal()

df_eu_scaled <- df_eu |>
  mutate(
    green_patents_scaled = (green_patents - min(green_patents, na.rm=TRUE)) / (max(green_patents, na.rm=TRUE) - min(green_patents, na.rm=TRUE)),
    en_taxes2_scaled = (en_taxes2 - min(en_taxes2, na.rm=TRUE)) / (max(en_taxes2, na.rm=TRUE) - min(en_taxes2, na.rm=TRUE)),
    rd_eu_scaled = (rd_eu - min(rd_eu, na.rm=TRUE)) / (max(rd_eu, na.rm=TRUE) - min(rd_eu, na.rm=TRUE)),
    tot_eu_scaled = (tot_eu - min(tot_eu, na.rm=TRUE)) / (max(tot_eu, na.rm=TRUE) - min(tot_eu, na.rm=TRUE)
  ))

df_eu_scaled |> 
  filter(year %in% c(1995:2019)) |> 
ggplot() +
  geom_line(aes(x = year, y = green_patents_scaled, color = "Green Patents")) +
  geom_line(aes(x = year, y = en_taxes2_scaled, color = "Environmental Taxes"), linetype = "dashed") +
  geom_line(aes(x = year, y = rd_eu_scaled, color = "R&D Expenditure"), linetype = "dotted") +
  #geom_line(aes(x = year, y = tot_eu_scaled, color = "Total EU Patents"), linetype = "longdash") +
  scale_x_continuous(breaks = seq(1995, 2019, by = 1)) +
  labs(
    #title = "Normalized Trends: Green Patents, Environmental Taxes, and R&D",
    x = "Year",
    y = "Normalized Value (0-1)",
    color = "Legend"
  ) +
  theme_minimal()

ggsave(
  "C:/Users/Utente/Documents/GitHub/green-innovation-data-project/graphs/eu_trends.png",
  width = 10,
  height = 6,
  bg = "white"
)

 ## time series to investigate the relationship between variables

df_eu_scaled <- df_eu |> 
  group_by(year) |> 
  slice(1) |>  # keep one row per year for EU-level data
  ungroup() |>
  mutate(
    green_patents_z = scale(green_patents)[,1],
    en_taxes2_z = scale(en_taxes2)[,1],
    rd_eu_z = scale(rd_eu)[,1],
    gas_price_z = scale(gas_price)[,1],
    tot_eu_z = scale(tot_eu)[,1],
    fdi_z = scale(fdi)[,1],
    co2_z = scale(co2)[,1],
    industry_gdp_z = scale(industry_gdp)[,1]
  )

df_eu_scaled <- df_eu_scaled |>
  filter(year %in% c(1995:2019))

ggplot() +
  geom_line(data = df_eu_scaled, 
            aes(x = year, y = green_patents_z, color = country), alpha = 0.5) +
  
  # Add EU-level lines (standardized)
  #geom_line(data = df_eu_scaled, aes(x = year, y = en_taxes2_z, color = "Environmental Taxes"), linetype = "dashed", size = 1) +
  #geom_line(data = df_eu_scaled, aes(x = year, y = rd_eu_z, color = "R&D Expenditure"), linetype = "dotted", size = 1) +
  geom_line(data = df_eu_scaled, aes(x = year, y = gas_price_z, color = "Gas Price"), linetype = "twodash", size = 1) +
  #geom_line(data = df_eu_scaled, aes(x = year, y = crude_oil_z, color = "Crude Oil Price"), linetype = "dotdash", size = 1) +
  #geom_line(data = df_eu_scaled, aes(x = year, y = tot_eu_z, color = "Total EU Patents"), linetype = "longdash", size = 1) +
  geom_line(data = df_eu_scaled, aes(x = year, y = fdi_z, color = "FDI"), linetype = "dotted", size = 1) +
  geom_line(data = df_eu_scaled, aes(x = year, y = co2_z, color = "CO2"), linetype = "dashed", size = 1) +
  geom_line(data = df_eu_scaled, aes(x = year, y = industry_gdp_z, color = "Industry GDP"), linetype = "twodash", size = 1) +
  
  labs(title = "Standardized Trends: Green Patents vs. EU-Level Indicators",
       x = "Year",
       y = "Standardized Values (z-scores)",
       color = "Legend") +
  theme_minimal() +
  theme(legend.position = "bottom")

#### for all countries

stand_trends <- ggsave(
  "C:/Users/Utente/Documents/GitHub/green-innovation-data-project/graphs/stand_trends.png",
  width = 10,
  height = 6,
  bg = "white"
)


df1 <- read.csv("C:/Users/Utente/Documents/GitHub/green-innovation-data-project/processed_data/final_df_all.csv", sep = ",") |>
  filter(year == 2019)

library(ggrepel)
library(countrycode)
install.packages("ggflags", repos = c(
  "https://jimjam-slam.r-universe.dev",
  "https://cloud.r-project.org"))
library(ggflags)
library(htmlwidgets)

library(countrycode)

df1$iso2 <- countrycode(df1$country, origin = "country.name", destination = "iso2c") 

ggplot(df1, aes(x = rd, y = green_patents)) +
  geom_vline(xintercept = median(log(df1$rd), na.rm = TRUE) %>% log10(), 
             linetype = "dashed", color = "red") +
  geom_hline(yintercept = median(log(df1$green_patents), na.rm = TRUE) %>% log10(), 
             linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) + 
  geom_point() +
  #geom_flag(aes(country = iso2), size = 10) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal() + 
  labs(title = "Green Patent vs GDP per capita", 
       x = "R&D Expenditure",
       y = "Number of patents",
       caption = "Source: Eurostat, APO Figure: Cailotto Anna") +
  theme(
    plot.background = element_rect(fill = "lightcyan2"),
    plot.title = element_text(size = 15, face = "plain", hjust = 0.5),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

library(ggplot2)
library(ggrepel)
library(scales)

df <- df %>%
  mutate(
    rd = as.numeric(rd),
    population = as.numeric(population),
    green_patents = as.numeric(green_patents)
  )
class(df$rd)
class(df$population)
class(df$green_patents)

str(df$rd)

df1_pc <- df1 %>%
  mutate(
    rd_pc = rd / (population / 1e6),         # R&D per million people
    green_pc = green_patents / (population / 1e6),
    rd_million = rd_pc * 1e6,               # back to absolute R&D
    green_million = green_pc * 1e6          # back to absolute green patents
  )

rd <- ggplot(df1_pc, aes(x = rd_pc, y = green_pc)) +
  # reference lines at median per-million values
  geom_vline(xintercept = median(df1$rd_pc, na.rm = TRUE),
             linetype = "dashed", color = "red", linewidth = 0.7) +
  geom_hline(yintercept = median(df1$green_pc, na.rm = TRUE),
             linetype = "dashed", color = "red", linewidth = 0.7) +
  
  # regression line
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x,
              color = "blue", linewidth = 1) +
  
  # points sized by population
  geom_point(aes(size = population), color = "yellow4", alpha = 0.6) +
  
  # ISO2 labels
  geom_text_repel(aes(label = iso2),
                  size = 3.5,
                  box.padding = 0.5,
                  point.padding = 0.8,
                  max.overlaps = 20) +
  
  # log scales for readability
  scale_x_log10(labels = comma_format()) +
  scale_y_log10(labels = comma_format()) +
  
  # bubble size scale
  scale_size(
    range = c(2, 12),
    name = "Population (millions)",
    labels = function(x) paste0(round(x / 1e6, 1), "M")
  ) +
  
  # labels
  labs(
    x = "R&D Expenditure per Capita (per million, log scale)",
    y = "Green Patents per Capita (per million, log scale)"
  ) +
  
  # theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(colour = "gray80", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white", color = NA)
  )

rd


ggsave("C:/Users/Utente/Documents/GitHub/green-innovation-data-project/graphs/country_trends_rd.png", width = 10, height = 6, bg = "white")

df1 <- df1 %>%
  mutate(
    en_taxes_pc       = en_taxes2 / (population / 1e6),  # per million people
    green_pc          = green_patents / (population / 1e6),
    en_taxes_million  = en_taxes_pc * 1e6,               # back to "millions" for plot readability
    green_million     = green_pc * 1e6
  )


env <- ggplot(df1, aes(x = en_taxes_pc, y = green_pc)) +
  # reference lines at medians (computed in the same units as axes)
  geom_vline(xintercept = median(df1$en_taxes_pc, na.rm = TRUE),
             linetype = "dashed", color = "red", linewidth = 0.7) +
  geom_hline(yintercept = median(df1$green_pc, na.rm = TRUE),
             linetype = "dashed", color = "red", linewidth = 0.7) +
  
  # regression line
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x,
              color = "blue", linewidth = 1) +
  
  # points sized by population
  geom_point(aes(size = population), color = "yellow4", alpha = 0.6) +
  
  # ISO codes
  geom_text_repel(aes(label = iso2),
                  size = 3,
                  point.padding = 0.6,
                  max.overlaps = 15) +
  
  # log scales with comma formatting
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  
  # bubble size scale
  scale_size(range = c(2, 10), 
             labels = function(x) paste0(round(x / 1e6, 1), "M"),
             name = "Population (millions)") +
  
  # axes labels
  labs(
    x = "Environmental Taxes per million people (log scale)",
    y = "Green Patents per million people (log scale)"
  ) +
  
  # theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(colour = "gray80", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white", color = NA)
  )

env

ggsave("C:/Users/Utente/Documents/GitHub/green-innovation-data-project/graphs/country_trends_envtax.png", width = 10, height = 6, bg = "white")

library(patchwork)

combined_plot <- rd + env +
  plot_layout(ncol = 1) +
  plot_annotation(
    #title = "Green Patents vs R&D Expenditure and Environmental Taxes",
    #caption = "Source: Eurostat, APO • Visualization: Cailotto Anna"
  )

combined_plot

ggsave("C:/Users/Utente/Documents/GitHub/green-innovation-data-project/graphs/combined_patents.png", width = 10, height = 12, bg = "white")

