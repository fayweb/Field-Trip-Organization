library(tidyverse)
library(data.table)

Samples <- read.csv("https://raw.githubusercontent.com/fayweb/Field-Trip-Organization/main/Tissue_Samples_2021.csv")

Samples <- Samples %>%
  mutate(Tube = case_when(Cryo_Tube == T ~ "Cryo",
                          Tube_2mL  == T ~ "2mL",
                          Tube_15mL == T ~ "15mL"),
         Storage_Medium = case_when(Storage_4_K2Cr2O7 == T ~ "Potassium Dichromate",
                             Storage_minus20_Alcohol  == T ~ "Alcohol",
                             Storage_minus80          == T ~ "None",
                             RT_Formalin              == T ~ "Formalin"),
         Storage_Temp = case_when(Storage_4_K2Cr2O7       == T ~ "+4 degrees",
                                  Storage_minus20_Alcohol == T ~ "-20 degrees",
                                  Storage_minus80         == T ~ "-80 degrees",
                                  RT_Formalin             == T ~ "RT"),
         Tube_Need = case_when(need_Cryo_tubes > 0 ~ need_Cryo_tubes,
                               need_2mL_tubes  > 0 ~ need_2mL_tubes,
                               need_15mL_tubes > 0 ~ need_15mL_tubes),
         need_to_print = case_when(label_need_reality >= 0 ~ label_need_reality),
         need_to_rewrite = case_when( expired_labeled_Cryo > 0 & expired_Labels > 0 ~ (expired_labeled_Cryo + expired_Labels),
                                      expired_Labels > 0 ~ expired_Labels,
                                      expired_labeled_Cryo > 0 ~ expired_labeled_Cryo)) %>%
        select(Sample_Name,
         Label,
         Person,
         Storage_Temp,
         Storage_Medium,
         Tube,
         Tube_Need,
         Non_mus_Prep,
         last_Sample_Label,
         already_labeled_Cryo_tube,
         new_Labels,
         need_to_rewrite,
         need_to_print,
         c(11:21),
         -need_Cryo_tubes, -need_2mL_tubes, -need_15mL_tubes,
         -label_need_theoretical, -label_need_reality, 
         -expired_labeled_Cryo, -expired_Labels)

#write.csv(Samples, "Tissue_Samples_2021_adj.csv")

#Samples_min <- Samples %>% select(c(1:6))
#write.csv(Samples_min, "Tissue_Samples.csv")

  
  
  