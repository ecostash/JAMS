# Pre-analysis setup

# Clear all data from previous runs
rm(list=ls())

# Load required packages
library(dplyr)
library(tidyr)
library(lubridate) 
library(zoo)

# Set working directory
setwd("C:/Users/Michael/Desktop/R_code/2oProd_code/2oProd_no_cohort_21Mar2017/PB/draft_1")

# Import Biomass table from "data" folder in working directory.
Biomass = read.csv("data/PB_no_cohort_v1.csv", header = TRUE)
Biomass[is.na(Biomass)] = 0

# Import PB ratio from "data" folder in working directory.
PB = read.csv("data/PB_v1.csv", header = TRUE)
#===============================================================================================

# Step 1

# Resample_function_biomass
Resample_function_biomass = function(Biomass)
{group_by(Biomass, ID_IGR, GENUS, DATE, HABITAT) %>%
   slice(sample(n(), replace = TRUE))%>%
    ungroup()%>%
      group_by(GENUS, HABITAT) %>%
        arrange (mdy(DATE)) %>%
          ungroup()
}   

# Resample_function_PB
Resample_function_PB = function(PB)
{group_by(PB, ID_IGR, ID_TAXA) %>%
  sample_n(.,1, replace = TRUE) %>%
   ungroup()
}

#==============================================================================================

# Step 2

# Define number of resampling events
N_resample= 1000

# Loop 1
for (n in 1:N_resample)
  {Resample_biomass = Resample_function_biomass(Biomass)
   Resample_PB = Resample_function_PB(PB)
   
# Writes CSV file to "check" folder in working directory to make sure the protion of Step 1 worked correctly
# Delete # to run this line of code
  # write.csv(Resample_biomass, file = paste("check/Resample_biomass.csv"), row.names = FALSE) 
  # write.csv(Resample_PB, file = paste("check/Resample_PB.csv"), row.names = FALSE) 
#============================================================================================

# Step 3
   
# Habitat specific secondary production
# Calculations in this step are HABITAT-SPECIFIC
   
# Step 3.1
Hab_avg_biomass = select(Resample_biomass, -DATE)%>%
  group_by(ID_IGR, GENUS, HABITAT) %>%
   summarise_each(funs(mean))%>%
    ungroup()
   
# Writes CSV file to "check" folder in working directory to make sure the protion of Step 3.1 worked correctly
# Delete # to run this line of code
  # write.csv(Hab_avg_biomass, file = paste("check/Hab_avg_biomass.csv"), row.names = FALSE) 

# Step 3.2
Hab_total_prod = left_join(Hab_avg_biomass, Resample_PB, by = "ID_IGR")%>%
  mutate(P = TOTBIO * AVG_PB, B = TOTBIO)%>%
   select (-TOTBIO)

# Writes CSV file to "check" folder in working directory to make sure the protion of Step 3.2 worked correctly
# Delete # to run this line of code
  # write.csv(Hab_total_prod, file = paste("check/Hab_total_prod.csv"), row.names = FALSE) 

#================================================================================================

# Step 4

  if (n==1) 
    {Hab_prod = Hab_total_prod
    }
      else {Hab_prod = bind_rows(Hab_prod,Hab_total_prod)}

}
# This is the end of Loop 1

# Writes CSV file to "results" folder in working directory
write.csv(Hab_prod, file = paste("results/Raw_Hab_prod.csv"), row.names = FALSE)

#==============================================================================================

# Step 5

# Calculate 95% quantiles for habitat specific P and B
Hab_quantile = group_by(Hab_prod, GENUS, HABITAT)%>%
  summarise("MeanP" = mean(P),
            "P_95%_UP" = quantile(P, probs = 0.975),
            "P_95%_DW" = quantile(P, probs = 0.025),
            "MeanB" = mean(B), 
            "B_95%_UP" = quantile(B, probs = 0.975),
            "B_95%_DW" = quantile(B, probs = 0.025))

# Writes CSV file to "results" folder in working directory
write.csv(Hab_quantile, file = paste("results/Hab_quantile.csv"), row.names = FALSE)

