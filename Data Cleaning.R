library(here)
library(foreign)
library(tidyverse)
library(missForest)

raw_data <- read.spss("H:\\My Drive\\Research\\Projects\\Depression Beliefs\\BADS\\BADS Data\\BADS_UM_2022_2023_FullDataSet_Scored_&MSU_Take2_wAmherst_Take2_wFollowUP.sav",
                      to.data.frame = T)

clean_data <- raw_data %>%
  # Filter to respondents who passed attention checks
  filter(Attn_FinalSum >= 4) %>%
  # Create new/clean variables
  transmute(
    attention_checks = Attn_FinalSum,
    # Demographics
    age = Demo_Age %>%
      gsub("2o", "20", .) %>% # Replace mis-typed response with number
      trimws() %>% # Remove leading and trailing spaces
      as.numeric() %>% # Switch to numeric variable
      if_else(. == 10, NA_real_, .), # Set likely invalid outlier to NA
    gender = case_when(
      Demo_GenID == "Male" & (is.na(Dem_Trans) | Dem_Trans  == "No") ~ "Cis Male",
      Demo_GenID == "Female" & (is.na(Dem_Trans) | Dem_Trans == "No") ~ "Cis Female",
      Demo_GenID != "Prefer not to answer" ~ "TGD",
      T ~ NA_character_
    ),
    race_ethnicity = case_when(
      Demo_Hisp == "Yes" ~ "Hispanic",
      Demo_Race == "A. American Indian or Alaskan Native" ~ "AI/AN non-Hispanic",
      Demo_Race == "\tB. Asian or Asian American" ~ "Asian non-Hispanic",
      Demo_Race == "C. Black or African American" ~ "Black non-Hispanic",
      Demo_Race == "\tD. Native Hawaiian or Other Pacific Islander" ~ "NH/PI non-Hispanic",
      Demo_Race == "\tE. Middle Eastern or North African" ~ "ME/NA non-Hispanic",
      Demo_Race == "\tF. White or European American" ~ "White non-Hispanic",
      Demo_Race == "G. Other (fill in the blank)" ~ "Other or Multiracial non-Hispanic",
      T ~ NA_character_
    ),
    family_income = case_when(
      Demo_Income %in% c(" $0-$20,000", "$20,000 - $50,000", "$50,000-$70,000") ~ "Below U.S. Median",
      Demo_Income %in% c("$70,000 - $100,000", "$100,000 - $200,000", "Over $200,000") ~ "Above U.S. Median",
      T ~ NA_character_
    ),
    
    # Depression self-identification (self-labeling)
    depression_self_id = PPBS_depYN == "Yes",
    
    # Depression diagnosis (professional labeling)
    depression_dx = Treatment_Exper_1_9 == "Yes",
    
    # Depression (PHQ8)
    PHQ8 = PHQ8_Tot,
    PHQ8_cutoff = PHQ8_Tot >= 10,
    
    # Cognitive emotion regulation (CERQ)
    CERQ_self_blame = CERQ_SelfBlame,
    CERQ_other_blame = CERQ_Blame,
    CERQ_rumination = CERQ_Rumin,
    CERQ_catastrophizing = CERQ_Catastroph,
    CERQ_perspective = CERQ_Perspective,
    CERQ_refocusing = CERQ_PosRefocus,
    CERQ_reappraising = CERQ_Reappraisal,
    CERQ_acceptance = CERQ_Accept,
    CERQ_planning = CERQ_Planning,
    
    # Attitudes towards treatment (MHSAS)
    MHSAS_therapy = Therapy_Seeking_tot,
    MHSAS_medication = Med_Seeking_tot,
    
    # Causal beliefs (RFD)
    RFD_character = RFD_Character_woItem4_Avg,
    RFD_achievement = RFD_Achievement_Avg,
    RFD_interpersonal = RFD_Interper_Avg,
    RFD_intimacy = RFD_Intimacy_Avg,
    RFD_existential = RFD_Existential_Avg,
    RFD_childhood = RFD_Childhood_Avg,
    RFD_physical = RFD_Physical_Avg,
    RFD_relationship = RFD_Relationship_Avg,
    RFD_cognitive = RFD_Cognitive_Avg,
    RFD_biological = RFD_BiologicalwChar4_Avg,
    
    # Prognostic pessimism (PPBS)
    prognostic_pessimism = (as.numeric(PPBS_7) + as.numeric(PPBS_8) - 2) / 2,
    
    # (Self-)Blame (PPBS)
    blame = (as.numeric(PPBS_1) + as.numeric(PPBS_3) - 2) / 2,
    
    # Agency (PPBS)
    agency = (as.numeric(PPBS_5) + as.numeric(PPBS_6) - 2) / 2,
    
    # Agency (IQT)
    agency_itq = (as.numeric(ITQ_16) + as.numeric(ITQ_17) + as.numeric(ITQ_18) + as.numeric(ITQ_19) - 4) / 4,
    
    # Stigma (ISMI)
    stigma = ISMI_Tot,
    
    # Function beliefs (ad-hoc)
    function_beliefs = Funct_Depress_Avg
    
  ) %>%
  # Reclass as numeric and factor to play nice with missForest
  mutate(across(where(is.character), as.factor),
         across(where(is.logical), as.factor))

set.seed(5269846)
imputed_data <- missForest(clean_data, verbose = T) # Could be doing this with the full dataset for more accuracy, but there are so few missing values it shouldn't ultimately matter

# Change factor variables back to logical, where applicable
imputed_data$ximp <- imputed_data$ximp %>%
  mutate(across(all_of(c("depression_self_id", "depression_dx", "PHQ8_cutoff")), as.logical))

saveRDS(imputed_data$ximp, file = here("Clean Data.rds"))
saveRDS(imputed_data$OOBerror, file = here("Imputation Results.rds"))
