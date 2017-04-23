# Pre-analysis setup

# Clear all data from previous runs
rm(list=ls())

# Load required packages
library(dplyr)
library(tidyr)
library(lubridate) 
library(zoo)

# Set working directory
setwd("C:/Users/Michael/Desktop/R_code/2oProd_code/Spider_analyses_20Mar2017")

# Import Biomass table from "data" folder in working directory.
Biomass = read.csv("data/Spider_v1.csv", header = TRUE)
#==============================================================================================

# Step 1

# Resample_function_biomass
Resample_function_biomass = function(Biomass)
{group_by(Biomass, SITE, DATE, HABITAT) %>%
   slice(sample(n(), replace = TRUE))%>%
   ungroup()
}
#==============================================================================================

# Step 2

# Define number of resampling events
N_resample = 1000

# Loop 1
for (n in 1:N_resample)
  {Resample_biomass = Resample_function_biomass(Biomass)

# Writes CSV file to "check" folder in working directory to make sure the protion of Step 1 worked correctly
# Delete # to run this line of code
  # write.csv(Resample_biomass, file = paste("check/Resample_biomass.csv"), row.names = FALSE) 
#==============================================================================================
  
# Step 3

Hab_avg_biomass = select(Resample_biomass, -DATE) %>%
  group_by(SITE, HABITAT) %>%
    summarise_each(funs(mean))

# Writes CSV file to "check" folder in working directory to make sure the protion of Step 3 worked correctly
# Delete # to run this line of code
  # write.csv(Hab_avg_biomass, file = paste("check/Hab_avg_biomass.csv"), row.names = FALSE) 
#==============================================================================================

# Step 4   

   if (n==1) 
   {Hab_biomass = Hab_avg_biomass}
      else 
        {Hab_biomass = bind_rows(Hab_biomass, Hab_avg_biomass)}
  }

# Writes CSV file to "results" folder in working directory
 write.csv(Hab_biomass, file = paste("results/Hab_biomass.csv"), row.names = FALSE)
#==============================================================================================

# Step 5 
 
# By SITE and HABITAT
Site_hab_quantile = group_by(Hab_biomass, SITE, HABITAT) %>%
  summarise("Mean_M_HABITAT" = mean(M_HABITAT),
            "M_HABITAT 95% UP" = quantile(M_HABITAT, probs = 0.975),
            "M_HABITAT 95% DW" = quantile(M_HABITAT, probs = 0.025),
            "Mean_M_VALLEY" = mean(M_VALLEY),
            "M_VALLEY 95% UP" = quantile(M_VALLEY, probs = 0.975),
            "M_VALLEY 95% DW" = quantile(M_VALLEY, probs = 0.025))

# By SITE
Site_quantile = group_by(Hab_biomass, SITE, HABITAT) %>%
   mutate(ID = sequence(n())) %>%
   ungroup() %>%
   group_by(SITE, ID) %>%
   summarise(Sum_M_VALLEY = sum(M_VALLEY), Sum_M_HABITAT = sum(M_HABITAT)) %>%
   summarise("Mean_M_HABITAT" = mean(Sum_M_HABITAT),
             "M_HABITAT 95% UP" = quantile(Sum_M_HABITAT, probs = 0.975),
             "M_HABITAT 95% DW" = quantile(Sum_M_HABITAT, probs = 0.025),
             "Mean_M_VALLEY" = mean(Sum_M_VALLEY),
             "M_VALLEY 95% UP" = quantile(Sum_M_VALLEY, probs = 0.975),
             "M_VALLEY 95% DW" = quantile(Sum_M_VALLEY, probs = 0.025))

# Writes CSV file to "results" folder in working directory
write.csv(Site_hab_quantile, file = paste("results/Site_hab_quantile.csv"), row.names = FALSE)
write.csv(Site_quantile, file = paste("results/Site_quantile.csv"), row.names = FALSE)
  



