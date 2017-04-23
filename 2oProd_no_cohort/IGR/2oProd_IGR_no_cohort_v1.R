# Pre-analysis setup

# Clear all data from previous runs
rm(list=ls())

# Load required packages
library(dplyr)
library(tidyr)
library(lubridate) 
library(zoo)

# Set working directory
setwd("C:/Users/Michael/Desktop/Github/2oProd_no_cohort/IGR/draft_1")

# Import Biomass table from "data" folder in working directory.
Biomass = read.csv("data/IGR_no_cohort_v1.csv", header = TRUE)
Biomass[is.na(Biomass)] = 0

# Import Instantaneous Growth rates from "data" folder in working directory.
Growth = read.csv("data/IGR_v1.csv", header = TRUE)
#===============================================================================================

# Step 1

# Resample_function_biomass
Resample_function_biomass = function(Biomass)
{group_by(Biomass, GENUS, DATE, HABITAT, ID_IGR) %>%
   slice(sample(n(), replace = TRUE))%>%
    ungroup()%>%
      group_by(GENUS, HABITAT) %>%
        arrange (mdy(DATE)) %>%
          ungroup()
}

# Resample_function_growth
Resample_function_growth = function(Growth)
  {sample_n(Growth, 1, replace = TRUE)}
#==============================================================================================

# Step 2

# Define number of resampling events
N_resample = 1000

# Loop 1 is a very long loop.
for (n in 1:N_resample)
  {Resample_biomass = Resample_function_biomass(Biomass)
   Resample_growth = Resample_function_growth(Growth)
    Size_class = select(Resample_biomass, -ID_IGR, -DATE, -HABITAT, -GENUS) %>%
      summarise_each(funs(sum)) 
      # Loop 2
        for (j in length(Size_class):1)
          {if (Size_class[j]>0)
            {Number_size_class = j
              break
            }
          }
    
    Resample_biomass = Resample_biomass[,1:(Number_size_class+4)]   

# Writes CSV file to "check" folder in working directory to make sure Step 1 and Step 2 have worked correctly
# Delete # to run this line of code
  # write.csv(Resample_biomass, file = paste("check/Resample_biomass.csv"), row.names = FALSE)
  # write.csv(Resample_growth, file = paste("check/Resample_growth.csv"), row.names = FALSE)
#=============================================================================================

# Step 3
    
# Habitat specific secondary production
# Calculations in this step are HABITAT-SPECIFIC
    
# Step 3.1 
Hab_avg_biomass_size = group_by(Resample_biomass, ID_IGR, GENUS, DATE, HABITAT) %>%
  summarise_each(funs(mean))%>%
    ungroup()%>%
        arrange(mdy(DATE))
    
# Writes CSV file to "check" folder in working directory to make sure the protion of Step 3.1 worked correctly
# Delete # to run this line of code
  # write.csv(Hab_avg_biomass_size, file = paste("check/Hab_avg_biomass_size.csv"), row.names = FALSE) 

# Step 3.2
Hab_total_biomass = gather(Hab_avg_biomass_size, SIZECLASS, BIOMASS, -DATE, -HABITAT, -GENUS, -ID_IGR) %>%
  select(-SIZECLASS) %>%
    group_by(ID_IGR, GENUS, HABITAT, DATE) %>%
      summarise_each(funs(sum)) %>%
        arrange (mdy(DATE)) %>%
          mutate(POO = as.numeric(difftime(mdy(DATE),lag(mdy(DATE),1))), 
            DAYSB=ifelse(is.na(POO),0,POO))%>%
              select(-POO) 

# Writes CSV file to "check" folder in working directory to make sure the protion of Step 3.2 worked correctly
# Delete # to run this line of code
  # write.csv(Hab_total_biomass, file = paste("check/Hab_total_biomass.csv"), row.names = FALSE) 

# Step 3.3
Hab_interval_prod = left_join(Hab_total_biomass, Resample_growth, by = "ID_IGR")%>%
  mutate(PINT= rollmean (BIOMASS, 2, fill=0, align="right") * DAYSB * IGR)

# Writes CSV file to "check" folder in working directory to make sure the protion of Step 3.3 worked correctly
# Delete # to run this line of code
  # write.csv(Hab_interval_prod, file = paste("check/Hab_interval_prod.csv"), row.names = FALSE) 

# Step 3.4
Hab_total_prod =group_by(Hab_interval_prod, GENUS, HABITAT)%>%
  summarise(P = sum(PINT), B = mean(BIOMASS, na.rm = TRUE))

# Writes CSV file to "check" folder in working directory to make sure the protion of Step 3.4 worked correctly
# Delete # to run this line of code
  # write.csv(Hab_total_prod, file = paste("check/Hab_total_prod.csv"), row.names = FALSE) 
#================================================================================================

# Step 4

  if (n==1) 
    {Hab_prod = Hab_total_prod
    }
      else {Hab_prod = bind_rows(Hab_prod,Hab_total_prod)
            }

}
# This is the end of Loop 1

# Writes CSV file to "results" folder in working directory
write.csv(Hab_prod, file = paste("results/Raw_Hab_prod.csv"), row.names = FALSE)
#============================================================================================

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
