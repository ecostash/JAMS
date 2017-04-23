# Pre-analysis setup

# Clear all data from previous runs
  rm(list=ls())

# Load required packages
  library(dplyr)
  library(tidyr)
  library(lubridate) 
  library(zoo)

# Set working directory
  setwd("C:/Users/Michael/Desktop/R_code/2oProd_code/2oProd_cohort_21Mar2017/draft_2")

# Import Abundance table from "data" folder in working directory.
  Abundance = read.csv("data/Cohort_v1.csv", header = TRUE)
  Abundance[is.na(Abundance)] = 0

# Import Length_mass table from "data" folder in working directory.
  Length_mass = read.csv("data/Length_mass_v1.csv", header = TRUE) 

# Import Remove_date table from "data" folder in working directory
  Remove_date = read.csv("data/Remove_date_v1.csv", header = TRUE) 
  Remove_date[is.na(Remove_date)] = 0
#===============================================================================================

# Step 1
  
# Resample_function
  Resample_function = function(Abundance)
  {group_by(Abundance, TAXA, GENUS, DATE, HABITAT) %>%
    slice(sample(n(), replace = TRUE)) %>%
     ungroup() %>%
       group_by(TAXA, GENUS, HABITAT) %>%
          arrange (mdy(DATE)) %>%
           ungroup()
  }  
#==============================================================================================

# Step 2

# Define number of resampling events
  N_resample = 1000  

# Loop 1 is a very long loop.
  for (n in 1:N_resample)
      {Resample_abundance = Resample_function(Abundance)
          Size_class = select(Resample_abundance, -DATE, -HABITAT, -TAXA, -GENUS) %>%
            summarise_each(funs(sum))
                # Loop 2
                  for (j in length(Size_class):1)
                    {if (Size_class[j]>0)
                      {Number_size_class = j
                         break
                      }
                    }
          
        Resample_abundance = Resample_abundance[,1:(Number_size_class+4)]   

# Writes CSV file to "check" folder in working directory to make sure Step 1 and Step 2 have worked correctly
# Delete # to run this line of code
  # write.csv(Resample_abundance, file = paste("check/Resample_abundance.csv"), row.names = FALSE)
#=============================================================================================

# Step 3

# Daily instantaneous growth rate calculations. 
# Calculations in this step are NON-HABITAT-SPECIFIC. 

# step 3.1 
  Avg_abundance_size = select(Resample_abundance, -HABITAT)%>%
    group_by(TAXA, GENUS, DATE) %>%
      summarise_each(funs(mean)) %>%
        ungroup()%>%
          group_by(TAXA, GENUS)%>%
            arrange(mdy(DATE)) %>%
              ungroup()

# Writes CSV file to "check" folder in working directory to make sure the protion of Step 3.1 worked correctly
# Delete # to run this line of code
  # write.csv(Avg_abundance_size, file = paste("check/Avg_abundance_size.csv"), row.names = FALSE)
  
# Step 3.2
  Avg_biomass_size = merge(gather(Avg_abundance_size, SIZECLASS, NUMBER, -DATE, -TAXA, -GENUS), 
    gather(Length_mass, SIZECLASS, BIOMASS,-TAXA)) %>%
      mutate(TOTBIO = NUMBER * BIOMASS)%>% 
        select(-SIZECLASS, -BIOMASS) %>%
          group_by(TAXA, GENUS, DATE) %>%
            summarise_each(funs(sum)) %>%
              mutate(AVGMASS = TOTBIO/NUMBER)%>%
                arrange (mdy(DATE))%>%
                  mutate(POO=as.numeric(difftime(mdy(DATE),lag(mdy(DATE),1))), 
                    DAYSB=ifelse(is.na(POO),0,POO),
                      DAYS_ELAPSED=cumsum(as.numeric(DAYSB))) %>%                                                                   
                        select(-POO) %>%
                          mutate (LN.AVGMASS = log(AVGMASS))

# Writes CSV file to "check" folder in working directory to make sure the protion of Step 3.2 worked correctly
# Delete # to run this line of code
  # write.csv(Avg_biomass_size, file = paste("check/Avg_biomass_size.csv"), row.names = FALSE) 
  
# Step 3.3
  Growth_rate = left_join(Avg_biomass_size, Remove_date, by = c("TAXA", "GENUS", "DATE"))%>%
    filter(CROP == 0)%>%
      select(-CROP)%>%
        group_by(TAXA, GENUS)%>%
          do(GROWTH = lm(LN.AVGMASS ~ DAYS_ELAPSED, data = ., na.action =na.omit))%>%
            mutate(IGR = as.numeric(coefficients(GROWTH)[2]), INTERCEPT = coefficients(GROWTH)[1]) %>%
              select(-GROWTH) %>%
                ungroup()
  
# Writes CSV file to "check" folder in working directory to make sure the protion of Step 3.3 worked correctly
# Delete # to run this line of code
  # write.csv(Growth_rate, file = paste("check/Growth_rate.csv"), row.names = FALSE) 
#==============================================================================================

# Step 4

# Calculations in this step are NON-HABITAT-SPECIFIC. 

# Step 4.1 - units are mg dry mass per square meter per day
  Interval_prod = left_join(Avg_biomass_size, Growth_rate, by = c("TAXA", "GENUS"))%>%  
    mutate(PINT = rollmean(TOTBIO,2,fill=0,align="right") * DAYSB * IGR)

# Writes CSV file to "check" folder in working directory to make sure the protion of Step 4.1 worked correctly
# Delete # to run this line of code
  # write.csv(Interval_prod, file = paste("check/Interval_prod.csv"), row.names = FALSE) 
  
  
# Step 4.2 
  Total_prod = group_by(Interval_prod, TAXA, GENUS)%>%
    summarise(P = sum(PINT), B = mean(TOTBIO, na.rm = TRUE), 
              PB = sum(PINT) / mean(TOTBIO, na.rm = TRUE))%>%
              left_join(.,Growth_rate, by = c("TAXA", "GENUS"))
  
# Writes CSV file to "check" folder in working directory to make sure the protion of Step 4.2 worked correctly
# Delete # to run this line of code
  # write.csv(Total_prod, file = paste("check/Total_prod.csv"), row.names = FALSE) 
#============================================================================================

# Step 5

# Habitat specific secondary production. 
# Calculations in this step are HABITAT-SPECIFIC.

# Step 5.1
  Hab_avg_abundance_size = group_by(Resample_abundance, TAXA, GENUS, DATE, HABITAT) %>%
    summarise_each(funs(mean))%>%
      ungroup()%>%
        group_by(TAXA, GENUS)%>%
          arrange(mdy(DATE)) %>%
            ungroup()

# Writes CSV file to "check" folder in working directory to make sure the protion of Step 5.1 worked correctly
# Delete # to run this line of code
  # write.csv(Hab_avg_abundance_size, file = paste("check/Hab_avg_abundance_size.csv"), row.names = FALSE) 
  
# Step 5.2
  Hab_avg_biomass_size = merge(gather(Hab_avg_abundance_size, SIZECLASS, NUMBER, -DATE, -HABITAT, -TAXA, -GENUS), 
    gather(Length_mass, SIZECLASS, BIOMASS, -TAXA)) %>%
      mutate(TOTBIO = NUMBER * BIOMASS) %>%
        select(-SIZECLASS, -BIOMASS, -NUMBER) %>%  
          group_by(TAXA, GENUS,HABITAT, DATE) %>%
            summarise_each(funs(sum)) %>%
              arrange (mdy(DATE)) %>%
                mutate(POO = as.numeric(difftime(mdy(DATE),lag(mdy(DATE),1))), 
                  DAYSB=ifelse(is.na(POO),0,POO))%>%
                    select(-POO)

# Writes CSV file to "check" folder in working directory to make sure the protion of Step 5.2 worked correctly
# Delete # to run this line of code
  # write.csv(Hab_avg_biomass_size, file = paste("check/Hab_avg_biomass_size.csv"), row.names = FALSE) 
  
# Step 5.3 - units are mg dry mass per square meter per day
  Hab_interval_prod = left_join(Hab_avg_biomass_size, Growth_rate, by = c("TAXA", "GENUS"))%>%  
    mutate(PINT= rollmean (TOTBIO, 2, fill=0, align="right") * DAYSB * IGR)
  
# Writes CSV file to "check" folder in working directory to make sure the protion of Step 5.3 worked correctly
# Delete # to run this line of code
  # write.csv(Hab_interval_prod, file = paste("check/Hab_interval_prod.csv"), row.names = FALSE) 

# Step 5.4
  Hab_total_prod = group_by(Hab_interval_prod, TAXA, GENUS, HABITAT)%>%
    summarise(P = sum(PINT), B = mean(TOTBIO, na.rm = TRUE)) %>%
      left_join(.,select(Total_prod, -P, -B), by = c("TAXA", "GENUS"))
  
# Writes CSV file to "check" folder in working directory to make sure the protion of Step 5.4 worked correctly
# Delete # to run this line of code
  # write.csv(Hab_total_prod, file = paste("check/Hab_total_prod.csv"), row.names = FALSE) 
#================================================================================================

# Step 6 
  
# Step 6.1 
  if (n==1) 
    {Site_prod = Total_prod
     Hab_prod = Hab_total_prod
    }
      else {Site_prod = bind_rows(Site_prod,Total_prod)
            Hab_prod = bind_rows(Hab_prod,Hab_total_prod)
            }
}
  
# This is the end of Loop 1

# Writes CSV file to "results" folder in working directory
  write.csv(Site_prod, file = paste("results/Raw_site_prod.csv"), row.names = FALSE)
  write.csv(Hab_prod, file = paste("results/Raw_hab_prod.csv"), row.names = FALSE)
#============================================================================================

# Step 7

# Step 7.1
  Site_remove_neg = group_by(Site_prod, TAXA, GENUS)%>%
    filter(P >= 0) 

  Hab_remove_neg = group_by(Hab_prod, TAXA, GENUS, HABITAT)%>%
    filter(P >= 0) 

# Step 7.2
  Site_final = group_by(Site_remove_neg, TAXA, GENUS) %>%
    mutate(RESAMPLE = N_resample - n())%>%
      do(bind_rows(., sample_n(., size = .$RESAMPLE[1], replace=TRUE ))) %>%
        ungroup()

  Hab_final = group_by(Hab_remove_neg, TAXA, GENUS, HABITAT) %>%
    mutate(RESAMPLE = N_resample - n())%>%
      do(bind_rows(., sample_n(., size = .$RESAMPLE[1], replace=TRUE ))) %>%
        ungroup()

# Writes CSV file to "results" folder in working directory
  write.csv(Site_final, file = paste("results/Site_final.csv"), row.names = FALSE)
  write.csv(Hab_final, file = paste("results/Hab_final.csv"), row.names = FALSE)
#==========================================================================================================================

# Step 8

# Calculate 95% quantiles for HABITAT-SPECIFIC P and B
  Hab_quantile = select(Hab_final, -PB, -IGR, -INTERCEPT, -RESAMPLE)%>%
    group_by(TAXA, GENUS, HABITAT)%>%
      summarise("MeanP" = mean(P),
                "P_95%_UP" = quantile(P, probs = 0.975),
                "P_95%_DW" = quantile(P, probs = 0.025),
                "MeanB" = mean(B), 
                "B_95%_UP" = quantile(B, probs = 0.975),
                "B_95%_DW" = quantile(B, probs = 0.025))

# Calculate 95% quantiles for NON-HABITAT-SPECIFIC PB, IGR, and INTERCEPT
  Site_quantile = select(Site_final, -P, -B, -RESAMPLE)%>%
    group_by(TAXA, GENUS)%>%
      summarise("MeanPB" = mean(PB),
                "PB_95%_UP" = quantile(PB, probs = 0.975),
                "PB_95%_DW" = quantile(PB, probs = 0.025),
                "MeanIGR" = mean(IGR), 
                "IGR_95%_UP" = quantile(IGR, probs = 0.975), 
                "IGR_95%_DW" = quantile(IGR, probs = 0.025),
                "MeanINTERCEPT" = mean(INTERCEPT),
                "INTERCEPT_95%_UP" = quantile(INTERCEPT, probs = 0.975), 
                "INTERCEPT_95%_DW" = quantile(INTERCEPT, probs = 0.025))

# Writes CSV file to "results" folder in working directory
  write.csv(Hab_quantile, file = paste("results/Hab_quantile.csv"), row.names = FALSE)
  write.csv(Site_quantile, file = paste("results/Site_quantile.csv"), row.names = FALSE)
  