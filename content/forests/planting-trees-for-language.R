## ----message=FALSE, warning=FALSE---------------------------------------------
library(brms)
library(ggplot2)
library(dplyr)
library(tidybayes)
library(ggridges)
library(glue)

#setwd('Downloads/forests')

#loading CSVs and models to visualize
source('to_thomas.R')


## ----message=FALSE, warning=FALSE---------------------------------------------
model <- nested

#### Forest plot: data #############################################################

draws.indiv.studies <- tidybayes::spread_draws(model, r_articleID[articleID,], `r_articleID:sampleID`[articleID_sampleID,], b_Intercept) %>% 
  mutate(b_Intercept = r_articleID + b_Intercept + `r_articleID:sampleID`) 

draws.indiv.studies$experiment <- df_estimates_mostReduced$experiment[match(draws.indiv.studies$articleID_sampleID, paste(df_estimates_mostReduced$articleID, df_estimates_mostReduced$sampleID, sep="_"))]


draws.pooled <-spread_draws(model, b_Intercept) %>% 
  mutate(experiment = "Overall Effect")

forest.data <- bind_rows(draws.indiv.studies, 
                         draws.pooled) %>% 
  ungroup() %>%
  mutate(experiment = reorder(experiment, b_Intercept))


forest.data$experiment <- forcats::fct_relevel(forest.data$experiment, 'Overall Effect', after= Inf)

forest.data.summary <- group_by(forest.data, experiment) %>% 
  mean_qi(b_Intercept) %>%
  arrange(b_Intercept) %>%
  mutate(b_Intercept = exp(b_Intercept),
         .lower      = exp(.lower),
         .upper      = exp(.upper))


forest.data <- forest.data %>%
  mutate(b_Intercept = exp(b_Intercept),
         experiment = factor(experiment))


forest.data <- forest.data %>%
  mutate(fill_color = if_else(experiment == "Overall Effect", "black", "indianred"))



## ----message=FALSE, warning=FALSE---------------------------------------------
### ggplot ###


ggplot(aes(b_Intercept, 
           y =experiment), 
       data = forest.data) +
  
  # Add densities
  geom_density_ridges(aes(fill = fill_color), 
                      show.legend = FALSE, 
                      rel_min_height = 0.01, 
                      col = NA, scale = 1,
                      alpha = 0.8) +
  geom_pointinterval(data = forest.data.summary,  
                      size = 1, 
                      aes(xmin = .lower,
                      xmax = .upper)
                      ) +

  # Add vertical lines for pooled effect and CI
  geom_vline(xintercept = exp(fixef(model)[1, 1]), 
             color = "grey", size = 1, linetype = 2) +
  geom_vline(xintercept = exp(fixef(model)[1, 3:4]), 
             color = "grey", linetype = 2) +
  geom_vline(xintercept = 1, color = "black", 
             size = 1) +
  
  # Add text and labels
  geom_text(data = mutate_if(forest.data.summary, 
                             is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), 
                x = Inf), hjust = "inward") +
  labs(x = "Difference in pronoun use (measured in odds ratio)", # summary measure
       y = element_blank()) +
  
  xlim(0,13) +
  theme_minimal() 

ggsave('forest-nested.png')


## ----message=FALSE, warning=FALSE---------------------------------------------
model <- nested_all_int2c

#### Forest plot: data #############################################################

draws.indiv.studies <- tidybayes::spread_draws(model, r_articleID[articleID,], `r_articleID:sampleID`[articleID_sampleID,], b_Intercept) %>% 
  mutate(b_Intercept = r_articleID + b_Intercept + `r_articleID:sampleID`) 

draws.indiv.studies$experiment <- df_estimates_mostReduced$experiment[match(draws.indiv.studies$articleID_sampleID, paste(df_estimates_mostReduced$articleID, df_estimates_mostReduced$sampleID, sep="_"))]


draws.pooled <-spread_draws(model, b_Intercept) %>% 
  mutate(experiment = "Overall Effect")

forest.data <- bind_rows(draws.indiv.studies, 
                         draws.pooled) %>% 
  ungroup() %>%
  mutate(experiment = reorder(experiment, b_Intercept))


forest.data$experiment <- forcats::fct_relevel(forest.data$experiment, 'Overall Effect', after= Inf)

forest.data.summary <- group_by(forest.data, experiment) %>% 
  mean_qi(b_Intercept) %>%
  arrange(b_Intercept) %>%
  mutate(b_Intercept = exp(b_Intercept),
         .lower      = exp(.lower),
         .upper      = exp(.upper))


forest.data <- forest.data %>%
  mutate(b_Intercept = exp(b_Intercept),
         experiment = factor(experiment))


forest.data <- forest.data %>%
  mutate(fill_color = if_else(experiment == "Overall Effect", "black", "indianred"))

ggsave('forest-nested_all_int2c.png')



## ----message=FALSE, warning=FALSE---------------------------------------------
### ggplot ###


ggplot(aes(b_Intercept, 
           y =experiment), 
       data = forest.data) +
  
  # Add densities
  geom_density_ridges(aes(fill = fill_color), 
                      show.legend = FALSE, 
                      rel_min_height = 0.01, 
                      col = NA, scale = 1,
                      alpha = 0.8) +
  geom_pointinterval(data = forest.data.summary,  
                      size = 1, 
                      aes(xmin = .lower,
                      xmax = .upper)
                      ) +

  # Add vertical lines for pooled effect and CI
  geom_vline(xintercept = exp(fixef(model)[1, 1]), 
             color = "grey", size = 1, linetype = 2) +
  geom_vline(xintercept = exp(fixef(model)[1, 3:4]), 
             color = "grey", linetype = 2) +
  geom_vline(xintercept = 1, color = "black", 
             size = 1) +
  
  # Add text and labels
  geom_text(data = mutate_if(forest.data.summary, 
                             is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), 
                x = Inf), hjust = "inward") +
  labs(x = "Difference in pronoun use (measured in odds ratio)", # summary measure
       y = element_blank()) +
  
  xlim(0,11) +
  theme_minimal() 


## ----message=FALSE, warning=FALSE---------------------------------------------
model <- pro_drop

#### Forest plot: data #############################################################

draws.indiv.studies <- tidybayes::spread_draws(model, r_articleID[articleID,], `r_articleID:sampleID`[articleID_sampleID,], b_Intercept) %>% 
  mutate(b_Intercept = r_articleID + b_Intercept + `r_articleID:sampleID`) 

draws.indiv.studies$experiment <- df_estimates_proDrop$experiment[match(draws.indiv.studies$articleID_sampleID, paste(df_estimates_proDrop$articleID, df_estimates_proDrop$sampleID, sep="_"))]


draws.pooled <-spread_draws(model, b_Intercept) %>% 
  mutate(experiment = "Overall Effect")

forest.data <- bind_rows(draws.indiv.studies, 
                         draws.pooled) %>% 
  ungroup() %>%
  mutate(experiment = reorder(experiment, b_Intercept))


forest.data$experiment <- forcats::fct_relevel(forest.data$experiment, 'Overall Effect', after= Inf)

forest.data.summary <- group_by(forest.data, experiment) %>% 
  mean_qi(b_Intercept) %>%
  arrange(b_Intercept) %>%
  mutate(b_Intercept = exp(b_Intercept),
         .lower      = exp(.lower),
         .upper      = exp(.upper))


forest.data <- forest.data %>%
  mutate(b_Intercept = exp(b_Intercept),
         experiment = factor(experiment))


forest.data <- forest.data %>%
  mutate(fill_color = if_else(experiment == "Overall Effect", "black", "indianred"))


## ----message=FALSE, warning=FALSE---------------------------------------------
### ggplot ###

ggplot(aes(b_Intercept, 
           y =experiment), 
       data = forest.data) +
  
  # Add densities
  geom_density_ridges(aes(fill = fill_color), 
                      show.legend = FALSE, 
                      rel_min_height = 0.01, 
                      col = NA, scale = 1,
                      alpha = 0.8) +
  #@X: This is an updated version. The geom_pointintervalh() function we had before is now depracated
  geom_pointinterval(data = forest.data.summary,  
                      size = 1, 
                      aes(xmin = .lower,
                      xmax = .upper)
                      ) +

  # Add vertical lines for pooled effect and CI
  geom_vline(xintercept = exp(fixef(model)[1, 1]), 
             color = "grey", size = 1, linetype = 2) +
  geom_vline(xintercept = exp(fixef(model)[1, 3:4]), 
             color = "grey", linetype = 2) +
  geom_vline(xintercept = 1, color = "black", 
             size = 1) +
  
  # Add text and labels
  geom_text(data = mutate_if(forest.data.summary, 
                             is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), 
                x = Inf), hjust = "inward") +
  labs(x = "Difference in pronoun use (measured in odds ratio)", # summary measure
       y = element_blank()) +
  
  xlim(0,22) +
  theme_minimal() 

ggsave('forest-pro-drop.png')



## ----message=FALSE, warning=FALSE---------------------------------------------
knitr::purl('planting-trees-for-language.qmd')

