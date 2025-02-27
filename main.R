setwd("C:/Users/Clément/Desktop/Work/sovier_union_life_exp")

# install libraries
# install.packages("wbstats")    

#libraries
library(tidyverse)
library(wbstats)



# Download data with API
le <- wb(country = c("RU", "AM", "AZ", "BY", "EE", "GE", "KZ", "KG", "LV", "LT", "MD", "UZ", "UA", "TJ", "TM", "CZ", "PL", "HU", "SK", "RO", "BG"),
         indicator = "SP.DYN.LE00.IN",
         startdate = 1960, enddate = 2015)

pc <- wb(country = c("RU", "AM", "AZ", "BY", "EE", "GE", "KZ", "KG", "LV", "LT", "MD", "UZ", "UA", "TJ", "TM", "CZ", "PL", "HU", "SK", "RO", "BG"),
         indicator = "SP.POP.TOTL",
         startdate = 1960, enddate = 2015)

# Data transformation
le = le %>%
  select(c("iso3c","date","value")) %>%
  rename(life_exp = value)

pc = pc %>%
  select(c("iso3c","date","value")) %>%
  rename(population = value)

df = le %>%
  left_join(pc, by = c("iso3c","date")) %>%
  group_by(date) %>%
  mutate(sum_pop = sum(population),
         weigth_pop = population/sum_pop,
         date = as.numeric(date)) %>%
  summarise(mean_le = weighted.mean(life_exp, weigth_pop, na.rm = TRUE))

plot_le = ggplot(df, aes(x = date, y = mean_le)) +
  geom_line(color = "#00b029", size = 0.9) + 
  geom_vline(xintercept = 1991, color = "black", linetype = "solid") +  
  scale_x_continuous(breaks = seq(min(df$date), max(df$date), by = 5)) +  
  scale_y_continuous(breaks = seq(round(min(df$mean_le)), max(df$mean_le), by = 1)) +
  labs(title = "Évolution de l'espérance de vie moyenne des pays du bloc de l'Est", 
       x = "Année", 
       y = "Espérance de vie moyenne") +
  theme_minimal()


# Discussion
df_all = le %>%
  left_join(pc, by = c("iso3c", "date")) %>%
  group_by(date) %>%
  mutate(sum_pop = sum(population),
         weigth_pop = population / sum_pop,
         date = as.numeric(date)) %>%
  summarise(mean_le = weighted.mean(life_exp, weigth_pop, na.rm = TRUE)) %>%
  mutate(group = "Tous les pays") 

df_sans_russia = le %>%
  left_join(pc, by = c("iso3c", "date")) %>%
  group_by(date) %>%
  filter(iso3c != "RUS") %>%
  mutate(sum_pop = sum(population),
         weigth_pop = population / sum_pop,
         date = as.numeric(date)) %>%
  summarise(mean_le = weighted.mean(life_exp, weigth_pop, na.rm = TRUE)) %>%
  mutate(group = "Sans la Russie")  # Ajouter une variable pour la légende

df_combined = bind_rows(df_all, df_sans_russia)

# Tracer les deux lignes
plot_le_sans_russie = ggplot(df_combined, aes(x = date, y = mean_le, color = group)) +
  geom_line(size = 0.9) +  # Lignes pour les deux séries
  geom_vline(xintercept = 1991, color = "black", linetype = "solid") +  
  scale_x_continuous(breaks = seq(min(df_combined$date), max(df_combined$date), by = 5)) +  
  scale_y_continuous(breaks = seq(round(min(df_combined$mean_le)), max(df_combined$mean_le), by = 1)) +
  labs(title = "Évolution de l'espérance de vie moyenne des pays du bloc de l'Est", 
       x = "Année", 
       y = "Espérance de vie moyenne") +
  theme_minimal() +
  scale_color_manual(values = c("#00b029", "#47008f")) +
  guides(color = guide_legend(title = "Groupes de pays"))  


# file .RData
save(plot_le, plot_le_sans_russie, file = "plots.RData")

