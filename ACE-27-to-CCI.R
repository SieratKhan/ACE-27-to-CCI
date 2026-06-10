# Map ACE-27 Comorbidities to the Charlson Comorbidity Index (CCI)
# 
# This function takes a dataset containing standard ACE-27 item grades (0-3) 
# and synthesizes a harmonized Charlson Comorbidity Index (CCI) profile and total score.
# 
# @param data A dataframe containing ACE-27 columns (e.g., ACE_MI, ACE_DM, etc.)
# @return A dataframe with appended binary 'Mapped_CCI_' items and a 'Mapped_CCI_Total'.
# 
# @import dplyr
map_ace27_to_cci <- function(data) {
  
  # Ensure dplyr is available
  require(dplyr)
  
  data %>%
    mutate(
      # --- STEP 1: ESTABLISH BINARY PRESENCE (1 = Yes, 0 = No) ---
      
      # 1-to-1 Direct Matches
      Mapped_CCI_MI = ifelse(ACE_MI > 0, 1, 0),
      Mapped_CCI_CHF = ifelse(ACE_CHF > 0, 1, 0),
      Mapped_CCI_PVD = ifelse(ACE_PAD > 0, 1, 0),
      Mapped_CCI_COPD = ifelse(ACE_Respiratory > 0, 1, 0),
      Mapped_CCI_DEMENTIA = ifelse(ACE_Dementia > 0, 1, 0),
      Mapped_CCI_RHEUMATIC = ifelse(ACE_Rheumatologic > 0, 1, 0),
      Mapped_CCI_HBP = ifelse(ACE_Hypertension > 0, 1, 0),
      Mapped_CCI_HIV = ifelse(ACE_Immunological > 0, 1, 0),
      Mapped_CCI_RENAL = ifelse(ACE_Renal > 0, 1, 0),
      
      # Stroke & Hemiplegia Logic
      # Rule: If hemiplegia is present, do not count CVA separately
      Mapped_CCI_PLEGIA = ifelse(ACE_Paralysis > 0, 1, 0),
      Mapped_CCI_CVA = ifelse(ACE_Stroke > 0 & Mapped_CCI_PLEGIA == 0, 1, 0),
      
      # Diabetes Severity Split
      # ACE Grade 3 captures end-organ failure (retinopathy, neuropathy, nephropathy)
      Mapped_CCI_DMENDORGAN = ifelse(ACE_DM == 3, 1, 0),
      Mapped_CCI_DM = ifelse(ACE_DM %in% c(1, 2), 1, 0),
      
      # Liver Severity Split
      # ACE Grade 1 is mild (no portal hypertension). Grades 2 & 3 include portal HTN/ascites.
      Mapped_CCI_MILDLIVER = ifelse(ACE_Hepatic == 1, 1, 0),
      Mapped_CCI_SEVERELIVER = ifelse(ACE_Hepatic %in% c(2, 3), 1, 0),
      
      # Cancer & Metastasis Split
      # ACE Solid Tumor Grade 3 explicitly denotes documented metastases.
      Mapped_CCI_METASTASES = ifelse(ACE_SolidTumor == 3, 1, 0),
      Mapped_CCI_CANCER = ifelse(
        (ACE_SolidTumor %in% c(1, 2) | ACE_LeukemiaMyeloma > 0 | ACE_Lymphoma > 0) & 
          Mapped_CCI_METASTASES == 0, 1, 0
      ),
      
      # Proxies for Imperfect Matches
      # Venous >= 2 (Coumarin treatment) or Arrhythmias = 2 (A-Fib) serves as Warfarin proxy
      Mapped_CCI_WARFARIN = ifelse(ACE_Venous >= 2 | ACE_Arrhythmias = 2, 1, 0),
      Mapped_CCI_ULCER = ifelse(ACE_StomachIntestine > 0, 1, 0),
      Mapped_CCI_DEPRESSION = ifelse(ACE_Psychiatric > 0, 1, 0),
      
      # Unmappable CCI Variable
      Mapped_CCI_SKINULCER = 0,
      
      # --- STEP 2: APPLY OFFICIAL CCI WEIGHTS ---
      # Multiply the binary presence (0 or 1) by the standard CCI point value
      
      Mapped_CCI_Total = 
        (Mapped_CCI_MI * 1) +            
        (Mapped_CCI_CHF * 1) +           
        (Mapped_CCI_PVD * 1) +           
        (Mapped_CCI_CVA * 1) +           
        (Mapped_CCI_PLEGIA * 2) +        
        (Mapped_CCI_COPD * 1) +          
        (Mapped_CCI_DM * 1) +            
        (Mapped_CCI_DMENDORGAN * 2) +    
        (Mapped_CCI_RENAL * 2) +         
        (Mapped_CCI_MILDLIVER * 2) +     
        (Mapped_CCI_SEVERELIVER * 3) +   
        (Mapped_CCI_ULCER * 1) +         
        (Mapped_CCI_CANCER * 2) +        
        (Mapped_CCI_METASTASES * 6) +    
        (Mapped_CCI_DEMENTIA * 1) +      
        (Mapped_CCI_RHEUMATIC * 1) +     
        (Mapped_CCI_HIV * 6) +           
        (Mapped_CCI_HBP * 1) +           
        (Mapped_CCI_SKINULCER * 2) +     
        (Mapped_CCI_DEPRESSION * 1) +    
        (Mapped_CCI_WARFARIN * 1)        
    )
}