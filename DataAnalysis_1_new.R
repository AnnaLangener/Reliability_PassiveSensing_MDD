
##############################################
######## Reliability Project Try Out #########
##############################################
setwd("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project/Code")

library(lme4)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(misty)
library(patchwork)
library(reshape2)
library(hrbrthemes)
library(gridExtra)

source("Helper_Functions.R")         


length(unique(data_daily$uid))
# ---- Correlation Analyses ----
scaler = "Standard"
data_daily <- read.csv(paste0("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Daily_Cleaned_Dataset_",scaler,"_Complete.csv"))
data_weekly <- read.csv(paste0("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Weekly_Cleaned_Dataset_",scaler,"_Complete.csv"))
data_monthly <- read.csv(paste0("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Monthly_Cleaned_Dataset_",scaler,"_Complete.csv"))

Sleep <- c('gm_sleep_duration', 'gm_sleep_duration_awake','gm_sleep_duration_deep', 'gm_sleep_duration_rem',"sleep_duration")
Activity <- c('gm_dailies_step', 'step_count2',  'garmin_steps', 'act_still_ep_0', 'gm_dailies_active_kcal', 'gm_dailies_active_sec', 'gm_dailies_distance', 'gm_dailies_moderate_sec')
AffectiveDysregulation <- c ('garmin_hrv_mean_ep_0', 'gm_dailies_activity_stress_duration','gm_dailies_average_stress', 'garmin_stress_mean_ep_0','gm_dailies_high_stress_duration', 'gm_dailies_low_stress_duration','gm_dailies_max_stress', 'gm_dailies_medium_stress_duration')
BehavioralInactivation <- c('unlock_duration_ep_0', 'unlock_num_ep_0', 'home_ep_0', 'loc_visit_num_ep_0', 'loc_dist_ep_0')
Social <- c('audio_convo_duration_ep_0', 'audio_convo_num_ep_0','call_in_duration_ep_0', 'call_in_num_ep_0', 'call_out_duration_ep_0', 'call_out_num_ep_0', 'sms_in_num_ep_0', 'sms_out_num_ep_0')


# ---- Plotting ----
melt_corr <- function(corr_matrix) melt(corr_matrix, na.rm = TRUE)

plot_heatmap <- function(df, title) {
  ggplot(df, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "#d95f02",
      mid = "white",
      high = "#1b9e77",
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    geom_text(aes(label = round(value, 2)), size = 3) +
    theme_ipsum() +
    labs(title = title, x = "", y = "") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(face = "bold", size = 12),
      legend.position = "none"
    )
}

# ---- Datasets and domains ----
datasets <- list(
  Daily = data_daily,
  Weekly = data_weekly,
  Monthly = data_monthly
)

domains <- list(
  Sleep = Sleep,
  Activity = Activity,
  AffectiveDysregulation = AffectiveDysregulation,
  BehavioralInactivation = BehavioralInactivation,
  Social = Social
)

# ---- Compute correlations and store plots ----
plots_by_domain <- list()

for (i in 1:5) {
  subplots <- list()
  
  for (time_scale in names(datasets)) {
    data <- datasets[[time_scale]]
    corr_matrix <- cor(data[, domains[[i]]], use = "complete.obs")
    melt_df <- melt_corr(corr_matrix)
    print(names(domains)[i])
    print(time_scale)
    print(paste(mean(melt_df$value), min(melt_df$value),max(melt_df$value[melt_df$value != 1]) ))
    plot_title <- paste0(time_scale)
    subplots[[time_scale]] <- plot_heatmap(melt_df, plot_title)

  }
  
  # Combine subplots horizontally for this domain
  if (length(subplots) > 0) {
    combined <- wrap_plots(subplots, ncol = 3,guides = "collect", axes = "collect") +
    plot_annotation(title = names(domains)[i]) &
    theme(plot.title = element_text(face = "bold", size = 14))
    plots_by_domain[[names(domains)[i]]] <- combined
  }
}





#--------- Overview -------------

# 1) Internal consistency
# 2) Test - Retest
# 3) Interrater (Ceyhun)

#--------------------------------------
######## Internal consistency #########
#--------------------------------------

#------- 2) Generalizability Theory [Cranford]----------                                                                    
##### Generalizability Theory [Cranford] #######
# 4 variance components: items, measurement occasions, persons, measurement error
# Reliability of a construct when the scores of a scale are averaged

#r1f: reliability across people on one occasion (Reliability (Between Persons) of Measures Taken on the Same Fixed Occasion): occasion considered to be meaningful
#rkf: Consistency across persons averaged over k (e.g., all) occasions (Reliability (Between Persons) of Average of Measures Taken Over K Fixed Occasions): occasion also meaningful
#r1r: Generalizability of a score from one randomly chosen time point (Reliability (Between Persons) of Measures When Persons Are Measured on Different Occasions, how well does one randomly selected time point reflects indiviudal standing)
#rkr: Reliability of Person Mean (between): Generalizability of a person’s average score across randomly chosen occasions (average of measures taken over the planned random occasions): occasions not meaningful
#rc: Reliability of Change (within): Reliability of individual changes over time (within-person variation) (Reliability of Change (Within Person))


#r1: refers to one single occasion
#rk: refers to k occasions (average over the items per measurement occasions)
#r: random, f: fixed > we are intrested in random (most esm designs are)
# How a single measurement can provide a reliable estimate of between person differences vs. how repeated measurements provides a reliable estimate of between person differences

# Difference r1r & rkr, error components are divided my number of planned time points
# Between: Between person variation/sum(between+occasion+occasion by person + error)
# Within: Person by occasion/(person by occasion + measurement error): unique score systematically varies across occasions, with this variation applying to all items of a given scale
# (https://www.dipfdocs.de/volltexte/2020/20619/pdf/Emotion_2020_4_Brose_et_al_Measurement_of_within-person_affect_variation_A.pdf)

#Warn-D: Between: 1, Within 0.61-0.74
#Cranford: Between: 0.97-0.99, within: 0.62-0.88

####
library(future.apply)

# Define the combinations of scaler and suffix
scalers <- c("Standard", "Log_Standard", "MinMax", "Log_MinMax")
suffixes <- c("main", "imp")
combos <- expand.grid(scaler = scalers, suffix = suffixes, stringsAsFactors = FALSE)
scalers <- c("Standard")
suffixes <- c("main")

# Plan for parallel execution
plan(multisession, workers = 12)  # adjust workers to your CPU cores

# Function to run for each combination
process_combo <- function(scaler, suffix) {
  # Read data
  data_daily <- read.csv(paste0("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Daily_Cleaned_Dataset_", scaler, "_Complete.csv"))
  
  # Define variable groups
  if(suffix == "main"){
    Sleep <- c('gm_sleep_duration', 'gm_sleep_duration_awake','gm_sleep_duration_deep', 'gm_sleep_duration_rem',"sleep_duration")
    Activity <- c('gm_dailies_step', 'step_count2',  'garmin_steps', 'act_still_ep_0', 'gm_dailies_active_kcal', 'gm_dailies_active_sec', 'gm_dailies_distance', 'gm_dailies_moderate_sec')
    AffectiveDysregulation <- c ('garmin_hrv_mean_ep_0', 'gm_dailies_activity_stress_duration','gm_dailies_average_stress', 'garmin_stress_mean_ep_0','gm_dailies_high_stress_duration', 'gm_dailies_low_stress_duration','gm_dailies_max_stress', 'gm_dailies_medium_stress_duration')
    BehavioralInactivation <- c('unlock_duration_ep_0', 'unlock_num_ep_0', 'home_ep_0', 'loc_visit_num_ep_0', 'loc_dist_ep_0')
    Social <- c('audio_convo_duration_ep_0', 'audio_convo_num_ep_0','call_in_duration_ep_0', 'call_in_num_ep_0', 'call_out_duration_ep_0', 'call_out_num_ep_0', 'sms_in_num_ep_0', 'sms_out_num_ep_0')
  }
  
  if(suffix == "imp"){
    Sleep <- c('gm_sleep_duration', 'gm_sleep_duration_rem',"sleep_duration") 
    Activity <- c('gm_dailies_step', 'step_count2',  'garmin_steps', 'act_still_ep_0', 'gm_dailies_active_kcal', 'gm_dailies_active_sec', 'gm_dailies_distance', 'gm_dailies_moderate_sec')
    AffectiveDysregulation <- c ('garmin_hrv_mean_ep_0','gm_dailies_average_stress', 'garmin_stress_mean_ep_0','gm_dailies_high_stress_duration','gm_dailies_max_stress', 'gm_dailies_medium_stress_duration') 
    BehavioralInactivation <- c('unlock_duration_ep_0', 'home_ep_0', 'loc_visit_num_ep_0', 'loc_dist_ep_0') 
    Social <- c('audio_convo_duration_ep_0', 'audio_convo_num_ep_0','call_in_duration_ep_0', 'call_in_num_ep_0', 'call_out_duration_ep_0', 'call_out_num_ep_0', 'sms_in_num_ep_0', 'sms_out_num_ep_0')
  }
  
  daily_vars <- list(
    Sleep = Sleep
   # Activity = Activity, 
    #AffectiveDysregulation = AffectiveDysregulation, 
    #BehavioralInactivation = BehavioralInactivation, 
    #Social = Social
  )
  
  # Run your model
  daily_out <- run_gen_models(
    data = data_daily,
    vars = daily_vars,
    timescale = "Daily",
    time_var = "day",
    optimizers = c("nloptwrap", "Nelder_Mead"),
    REML_list = c(TRUE, FALSE),
    scale_name = scaler,
    suffix = suffix
  )
  
  return(daily_out)
}

results <- future_mapply(
  FUN = process_combo,
  scaler = combos$scaler,
  suffix = combos$suffix,
  SIMPLIFY = FALSE
)

# ---- Step 1: Load Data ----
for(scaler in c("Standard", "Log_Standard", "MinMax", "Log_MinMax")){
  for(suffix in c("main", "imp")){
  data_daily <- read.csv(paste0("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Daily_Cleaned_Dataset_",scaler,"_Complete.csv"))
  
  if(suffix == "main"){
    Sleep <- c('gm_sleep_duration', 'gm_sleep_duration_awake','gm_sleep_duration_deep', 'gm_sleep_duration_rem',"sleep_duration")
    Activity <- c('gm_dailies_step', 'step_count2',  'garmin_steps', 'act_still_ep_0', 'gm_dailies_active_kcal', 'gm_dailies_active_sec', 'gm_dailies_distance', 'gm_dailies_moderate_sec')
    AffectiveDysregulation <- c ('garmin_hrv_mean_ep_0', 'gm_dailies_activity_stress_duration','gm_dailies_average_stress', 'garmin_stress_mean_ep_0','gm_dailies_high_stress_duration', 'gm_dailies_low_stress_duration','gm_dailies_max_stress', 'gm_dailies_medium_stress_duration')
    BehavioralInactivation <- c('unlock_duration_ep_0', 'unlock_num_ep_0', 'home_ep_0', 'loc_visit_num_ep_0', 'loc_dist_ep_0')
    Social <- c('audio_convo_duration_ep_0', 'audio_convo_num_ep_0','call_in_duration_ep_0', 'call_in_num_ep_0', 'call_out_duration_ep_0', 'call_out_num_ep_0', 'sms_in_num_ep_0', 'sms_out_num_ep_0')
  }
  
  if(suffix == "imp"){
    # Remove negative items
    Sleep <- c('gm_sleep_duration', 'gm_sleep_duration_rem',"sleep_duration") # 2 removed
    Activity <- c('gm_dailies_step', 'step_count2',  'garmin_steps', 'act_still_ep_0', 'gm_dailies_active_kcal', 'gm_dailies_active_sec', 'gm_dailies_distance', 'gm_dailies_moderate_sec')
    AffectiveDysregulation <- c ('garmin_hrv_mean_ep_0','gm_dailies_average_stress', 'garmin_stress_mean_ep_0','gm_dailies_high_stress_duration','gm_dailies_max_stress', 'gm_dailies_medium_stress_duration') # 2 removed
    BehavioralInactivation <- c('unlock_duration_ep_0', 'home_ep_0', 'loc_visit_num_ep_0', 'loc_dist_ep_0') #unlock numeric removed
    Social <- c('audio_convo_duration_ep_0', 'audio_convo_num_ep_0','call_in_duration_ep_0', 'call_in_num_ep_0', 'call_out_duration_ep_0', 'call_out_num_ep_0', 'sms_in_num_ep_0', 'sms_out_num_ep_0')
  }
  
  daily_vars = list(Sleep = Sleep, Activity = Activity, AffectiveDysregulation = AffectiveDysregulation, BehavioralInactivation = BehavioralInactivation, Social = Social)
  
  
  daily_out <- run_gen_models(
    data = data_daily,
    vars = daily_vars,
    timescale = "Daily",
    time_var = "day",
    optimizers =  c("nloptwrap", "Nelder_Mead"),
    REML_list =  c(TRUE, FALSE),
    scale_name = scaler,
    suffix = suffix,
  )
  
  }
}

####
for(scaler in c("Standard", "Log_Standard", "MinMax", "Log_MinMax")){
  for(suffix in c("main", "imp")){
    
    print(paste(scaler, suffix))
    data_weekly <- read.csv(paste0("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Weekly_Cleaned_Dataset_",scaler,"_Complete.csv"))
    
    if(suffix == "main"){
      Sleep <- c('gm_sleep_duration', 'gm_sleep_duration_awake','gm_sleep_duration_deep', 'gm_sleep_duration_rem',"sleep_duration")
      Activity <- c('gm_dailies_step', 'step_count2',  'garmin_steps', 'act_still_ep_0', 'gm_dailies_active_kcal', 'gm_dailies_active_sec', 'gm_dailies_distance', 'gm_dailies_moderate_sec')
      AffectiveDysregulation <- c ('garmin_hrv_mean_ep_0', 'gm_dailies_activity_stress_duration','gm_dailies_average_stress', 'garmin_stress_mean_ep_0','gm_dailies_high_stress_duration', 'gm_dailies_low_stress_duration','gm_dailies_max_stress', 'gm_dailies_medium_stress_duration')
      BehavioralInactivation <- c('unlock_duration_ep_0', 'unlock_num_ep_0', 'home_ep_0', 'loc_visit_num_ep_0', 'loc_dist_ep_0')
      Social <- c('audio_convo_duration_ep_0', 'audio_convo_num_ep_0','call_in_duration_ep_0', 'call_in_num_ep_0', 'call_out_duration_ep_0', 'call_out_num_ep_0', 'sms_in_num_ep_0', 'sms_out_num_ep_0')
    }
    
    if(suffix == "imp"){
      # Remove negative items
      Sleep <- c('gm_sleep_duration', 'gm_sleep_duration_rem',"sleep_duration") # 2 removed
      Activity <- c('gm_dailies_step', 'step_count2',  'garmin_steps', 'act_still_ep_0', 'gm_dailies_active_kcal', 'gm_dailies_active_sec', 'gm_dailies_distance', 'gm_dailies_moderate_sec')
      AffectiveDysregulation <- c ('garmin_hrv_mean_ep_0','gm_dailies_average_stress', 'garmin_stress_mean_ep_0','gm_dailies_high_stress_duration','gm_dailies_max_stress', 'gm_dailies_medium_stress_duration') # 2 removed
      BehavioralInactivation <- c('unlock_duration_ep_0', 'home_ep_0', 'loc_visit_num_ep_0', 'loc_dist_ep_0') #unlock numeric removed
      Social <- c('audio_convo_duration_ep_0', 'audio_convo_num_ep_0','call_in_duration_ep_0', 'call_in_num_ep_0', 'call_out_duration_ep_0', 'call_out_num_ep_0', 'sms_in_num_ep_0', 'sms_out_num_ep_0')
    }
    
    weekly_vars = list(Sleep = Sleep, Activity = Activity, AffectiveDysregulation = AffectiveDysregulation, BehavioralInactivation = BehavioralInactivation, Social = Social)
    
    weekly_out <- run_gen_models(
      data = data_weekly,
      vars = weekly_vars,
      timescale = "Weekly",
      time_var = "week_num",
      optimizers =  c("nloptwrap", "Nelder_Mead"),
      REML_list =  c(TRUE, FALSE),
      scale_name = scaler,
      suffix = suffix
    )
  }
}

###
for(scaler in c("Standard", "Log_Standard", "MinMax", "Log_MinMax")){
  for(suffix in c("main", "imp")){
    
    print(paste(scaler, suffix))
    
    data_monthly <- read.csv(paste0("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Monthly_Cleaned_Dataset_",scaler,"_Complete.csv"))
    
    if(suffix == "main"){
      Sleep <- c('gm_sleep_duration', 'gm_sleep_duration_awake','gm_sleep_duration_deep', 'gm_sleep_duration_rem',"sleep_duration")
      Activity <- c('gm_dailies_step', 'step_count2',  'garmin_steps', 'act_still_ep_0', 'gm_dailies_active_kcal', 'gm_dailies_active_sec', 'gm_dailies_distance', 'gm_dailies_moderate_sec')
      AffectiveDysregulation <- c ('garmin_hrv_mean_ep_0', 'gm_dailies_activity_stress_duration','gm_dailies_average_stress', 'garmin_stress_mean_ep_0','gm_dailies_high_stress_duration', 'gm_dailies_low_stress_duration','gm_dailies_max_stress', 'gm_dailies_medium_stress_duration')
      BehavioralInactivation <- c('unlock_duration_ep_0', 'unlock_num_ep_0', 'home_ep_0', 'loc_visit_num_ep_0', 'loc_dist_ep_0')
      Social <- c('audio_convo_duration_ep_0', 'audio_convo_num_ep_0','call_in_duration_ep_0', 'call_in_num_ep_0', 'call_out_duration_ep_0', 'call_out_num_ep_0', 'sms_in_num_ep_0', 'sms_out_num_ep_0')

    }
    
    if(suffix == "imp"){
      # Remove negative items
      Sleep <- c('gm_sleep_duration', 'gm_sleep_duration_rem',"sleep_duration") # 2 removed
      Activity <- c('gm_dailies_step', 'step_count2',  'garmin_steps', 'act_still_ep_0', 'gm_dailies_active_kcal', 'gm_dailies_active_sec', 'gm_dailies_distance', 'gm_dailies_moderate_sec')
      AffectiveDysregulation <- c ('garmin_hrv_mean_ep_0','gm_dailies_average_stress', 'garmin_stress_mean_ep_0','gm_dailies_high_stress_duration','gm_dailies_max_stress', 'gm_dailies_medium_stress_duration') # 2 removed
      BehavioralInactivation <- c('unlock_duration_ep_0', 'home_ep_0', 'loc_visit_num_ep_0', 'loc_dist_ep_0') #unlock numeric removed
      Social <- c('audio_convo_duration_ep_0', 'audio_convo_num_ep_0','call_in_duration_ep_0', 'call_in_num_ep_0', 'call_out_duration_ep_0', 'call_out_num_ep_0', 'sms_in_num_ep_0', 'sms_out_num_ep_0')
    }
    
    monthly_vars = list(Sleep = Sleep, Activity = Activity, AffectiveDysregulation = AffectiveDysregulation, BehavioralInactivation = BehavioralInactivation, Social = Social)
    
    monthly_out <- run_gen_models(
      data = data_monthly,
      vars = monthly_vars,
      timescale = "Monthly",
      time_var = "month_num",
      optimizers =  c("nloptwrap", "Nelder_Mead"),
      REML_list =  c(TRUE, FALSE),
      scale_name = scaler,
      suffix = suffix
    )
  }
}




####
data <- read.csv(paste0("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Weekly_Cleaned_Dataset_",scaler,"_Complete.csv"))

mlr = psych::mlr(data,
                 grp   = "uid",
                 Time  = "week_num",
                 items = Social,
                 lmer  = TRUE,
                 lme   = FALSE,
                 alpha = FALSE,
                 aov   = FALSE)
print(mlr)


#------- 3) Generalizability Theory [Nezlek, 3x multilevel model, Leertrouwer and Sebas Code is the Same]----------   
# within: "internal consistency of a set of responses collected each measurement occasion, e.g., each day within the context of a daily diary study)""

scaler = "Standard"
suffix = "imp"


timescale = "Daily"
data_daily <- read.csv(paste0("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Daily_Cleaned_Dataset_",scaler,"_Complete.csv"))
data_weekly <- read.csv(paste0("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Weekly_Cleaned_Dataset_",scaler,"_Complete.csv"))
data_monthly <- read.csv(paste0("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Monthly_Cleaned_Dataset_",scaler,"_Complete.csv"))


if(suffix == "main"){
  Sleep <- c('gm_sleep_duration', 'gm_sleep_duration_awake','gm_sleep_duration_deep', 'gm_sleep_duration_rem',"sleep_duration")
  Activity <- c('gm_dailies_step', 'step_count2',  'garmin_steps', 'act_still_ep_0', 'gm_dailies_active_kcal', 'gm_dailies_active_sec', 'gm_dailies_distance', 'gm_dailies_moderate_sec')
  AffectiveDysregulation <- c ('garmin_hrv_mean_ep_0', 'gm_dailies_activity_stress_duration','gm_dailies_average_stress', 'garmin_stress_mean_ep_0','gm_dailies_high_stress_duration', 'gm_dailies_low_stress_duration','gm_dailies_max_stress', 'gm_dailies_medium_stress_duration')
  BehavioralInactivation <- c('unlock_duration_ep_0', 'unlock_num_ep_0', 'home_ep_0', 'loc_visit_num_ep_0', 'loc_dist_ep_0')
  Social <- c('audio_convo_duration_ep_0', 'audio_convo_num_ep_0','call_in_duration_ep_0', 'call_in_num_ep_0', 'call_out_duration_ep_0', 'call_out_num_ep_0', 'sms_in_num_ep_0', 'sms_out_num_ep_0')
  
}

if(suffix == "imp"){
  # Remove negative items
  Sleep <- c('gm_sleep_duration', 'gm_sleep_duration_rem',"sleep_duration") # 2 removed
  Activity <- c('gm_dailies_step', 'step_count2',  'garmin_steps', 'act_still_ep_0', 'gm_dailies_active_kcal', 'gm_dailies_active_sec', 'gm_dailies_distance', 'gm_dailies_moderate_sec')
  AffectiveDysregulation <- c ('garmin_hrv_mean_ep_0','gm_dailies_average_stress', 'garmin_stress_mean_ep_0','gm_dailies_high_stress_duration','gm_dailies_max_stress', 'gm_dailies_medium_stress_duration') # 2 removed
  BehavioralInactivation <- c('unlock_duration_ep_0', 'home_ep_0', 'loc_visit_num_ep_0', 'loc_dist_ep_0') #unlock numeric removed
  Social <- c('audio_convo_duration_ep_0', 'audio_convo_num_ep_0','call_in_duration_ep_0', 'call_in_num_ep_0', 'call_out_duration_ep_0', 'call_out_num_ep_0', 'sms_in_num_ep_0', 'sms_out_num_ep_0')
}

setwd("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Results")
Sleep_Nez_d <- GeneralizabilityTheory_Nezlek(data_daily,Sleep,"day","uid") #0.93, 0 # singular
Activity_Nez_d <- GeneralizabilityTheory_Nezlek(data_daily,Activity,"day","uid") #0.96, 0 # singular
AffectiveDysregulation_Nez_d <- GeneralizabilityTheory_Nezlek(data_daily,AffectiveDysregulation,"day","uid") # 0.97, 0 # singular
BehavioralInactivation_Nez_d <- GeneralizabilityTheory_Nezlek(data_daily,BehavioralInactivation,"day","uid") # singular
Social_Nez_d <- GeneralizabilityTheory_Nezlek(data_daily,Social,"day","uid")#0.99, 0.13 # no problem

saveRDS(Sleep_Nez_d, paste0(timescale, "_Nez_Sleep_", scaler, "_", suffix, ".rds"))
saveRDS(Activity_Nez_d, paste0(timescale, "_Nez_Activity_", scaler, "_", suffix, ".rds"))
saveRDS(AffectiveDysregulation_Nez_d, paste0(timescale, "_Nez_AffectiveDysregulation_", scaler, "_", suffix, ".rds"))
saveRDS(BehavioralInactivation_Nez_d, paste0(timescale, "_Nez_BehavioralInactivation_", scaler, "_", suffix, ".rds"))
saveRDS(Social_Nez_d, paste0(timescale, "_Nez_Social_", scaler, "_", suffix, ".rds"))

timescale = "Weekly"
Sleep_Nez_w <-GeneralizabilityTheory_Nezlek(data_weekly,Sleep,"week_num","uid") #0.93, 0 # singular
Activity_Nez_w <- GeneralizabilityTheory_Nezlek(data_weekly,Activity,"week_num","uid") #0.96, 0 # singular
AffectiveDysregulation_Nez_w <- GeneralizabilityTheory_Nezlek(data_weekly,AffectiveDysregulation,"week_num","uid") # 0.97, 0 # singular
BehavioralInactivation_Nez_w <-GeneralizabilityTheory_Nezlek(data_weekly,BehavioralInactivation,"week_num","uid") # singular
Social_Nez_w <- GeneralizabilityTheory_Nezlek(data_weekly,Social,"week_num","uid")#0.99, 0.13 # no problem

saveRDS(Sleep_Nez_w, paste0(timescale, "_Nez_Sleep_", scaler, "_", suffix, ".rds"))
saveRDS(Activity_Nez_w, paste0(timescale, "_Nez_Activity_", scaler, "_", suffix, ".rds"))
saveRDS(AffectiveDysregulation_Nez_w, paste0(timescale, "_Nez_AffectiveDysregulation_", scaler, "_", suffix, ".rds"))
saveRDS(BehavioralInactivation_Nez_w, paste0(timescale, "_Nez_BehavioralInactivation_", scaler, "_", suffix, ".rds"))
saveRDS(Social_Nez_w, paste0(timescale, "_Nez_Social_", scaler, "_", suffix, ".rds"))

timescale = "Monthly"
Sleep_Nez_m <-GeneralizabilityTheory_Nezlek(data_monthly,Sleep,"month_num","uid") 
Activity_Nez_m <- GeneralizabilityTheory_Nezlek(data_monthly,Activity,"month_num","uid") 
AffectiveDysregulation_Nez_m <- GeneralizabilityTheory_Nezlek(data_monthly,AffectiveDysregulation,"month_num","uid") 
BehavioralInactivation_Nez_m <-GeneralizabilityTheory_Nezlek(data_monthly,BehavioralInactivation,"month_num","uid")
Social_Nez_m <- GeneralizabilityTheory_Nezlek(data_monthly,Social,"month_num","uid")


saveRDS(Sleep_Nez_m, paste0(timescale, "_Nez_Sleep_", scaler, "_", suffix, ".rds"))
saveRDS(Activity_Nez_m, paste0(timescale, "_Nez_Activity_", scaler, "_", suffix, ".rds"))
saveRDS(AffectiveDysregulation_Nez_m, paste0(timescale, "_Nez_AffectiveDysregulation_", scaler, "_", suffix, ".rds"))
saveRDS(BehavioralInactivation_Nez_m, paste0(timescale, "_Nez_BehavioralInactivation_", scaler, "_", suffix, ".rds"))
saveRDS(Social_Nez_m, paste0(timescale, "_Nez_Social_", scaler, "_", suffix, ".rds"))


#------- 4) Multilevel Confirmatory Factor Analysis (Nicholas) ---------- 

scaler = "MinMax"
suffix = "main"

data_daily <- read.csv(paste0("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Daily_Cleaned_Dataset_",scaler,"_Complete.csv"))
data_weekly <- read.csv(paste0("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Weekly_Cleaned_Dataset_",scaler,"_Complete.csv"))
data_monthly <- read.csv(paste0("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Monthly_Cleaned_Dataset_",scaler,"_Complete.csv"))


if(suffix == "main"){
  Sleep <- c('gm_sleep_duration', 'gm_sleep_duration_awake','gm_sleep_duration_deep', 'gm_sleep_duration_rem',"sleep_duration")
  Activity <- c('gm_dailies_step', 'step_count2',  'garmin_steps', 'act_still_ep_0', 'gm_dailies_active_kcal', 'gm_dailies_active_sec', 'gm_dailies_distance', 'gm_dailies_moderate_sec')
  AffectiveDysregulation <- c ('garmin_hrv_mean_ep_0', 'gm_dailies_activity_stress_duration','gm_dailies_average_stress', 'garmin_stress_mean_ep_0','gm_dailies_high_stress_duration', 'gm_dailies_low_stress_duration','gm_dailies_max_stress', 'gm_dailies_medium_stress_duration')
  BehavioralInactivation <- c('unlock_duration_ep_0', 'unlock_num_ep_0', 'home_ep_0', 'loc_visit_num_ep_0', 'loc_dist_ep_0')
  Social <- c('audio_convo_duration_ep_0', 'audio_convo_num_ep_0','call_in_duration_ep_0', 'call_in_num_ep_0', 'call_out_duration_ep_0', 'call_out_num_ep_0', 'sms_in_num_ep_0', 'sms_out_num_ep_0')
  
}

if(suffix == "imp"){
  # Remove negative items
  Sleep <- c('gm_sleep_duration', 'gm_sleep_duration_rem',"sleep_duration") # 2 removed
  Activity <- c('gm_dailies_step', 'step_count2',  'garmin_steps', 'act_still_ep_0', 'gm_dailies_active_kcal', 'gm_dailies_active_sec', 'gm_dailies_distance', 'gm_dailies_moderate_sec')
  AffectiveDysregulation <- c ('garmin_hrv_mean_ep_0','gm_dailies_average_stress', 'garmin_stress_mean_ep_0','gm_dailies_high_stress_duration','gm_dailies_max_stress', 'gm_dailies_medium_stress_duration') # 2 removed
  BehavioralInactivation <- c('unlock_duration_ep_0', 'home_ep_0', 'loc_visit_num_ep_0', 'loc_dist_ep_0') #unlock numeric removed
  Social <- c('audio_convo_duration_ep_0', 'audio_convo_num_ep_0','call_in_duration_ep_0', 'call_in_num_ep_0', 'call_out_duration_ep_0', 'call_out_num_ep_0', 'sms_in_num_ep_0', 'sms_out_num_ep_0')
}


timescale = "Daily"
Sleep_omega_day <- multilevel.omega(data_daily[, Sleep], cluster = data_daily$uid, missing = "listwise",optim.method = c("nlminb")) # No solution (implausible values)
Sleep_omega_day = c(NA,NA) # If no solution was found we change to NA
Activity_omega_day <- multilevel.omega(data_daily[, Activity], cluster = data_daily$uid, missing = "listwise",optim.method = c("nlminb")) # 0.73, 0.68 (cautious interpretation)
AffectiveDysregulation_omega_day <- multilevel.omega(data_daily[, AffectiveDysregulation], cluster = data_daily$uid, missing = "listwise", optim.method = c("nlminb")) 
BehavioralInactivation_omega_day <- multilevel.omega(data_daily[, BehavioralInactivation], cluster = data_daily$uid, missing = "listwise")
Social_omega_day <- multilevel.omega(data_daily[, Social], cluster = data_daily$uid, missing = "listwise") # 0.48, 0.58 # no problem

saveRDS(Sleep_omega_day, paste0(timescale, "_multi_Sleep_", scaler, "_", suffix, ".rds"))
saveRDS(Activity_omega_day, paste0(timescale, "_multi_Activity_", scaler, "_", suffix, ".rds"))
saveRDS(AffectiveDysregulation_omega_day, paste0(timescale, "_multi_AffectiveDysregulation_", scaler, "_", suffix, ".rds"))
saveRDS(BehavioralInactivation_omega_day, paste0(timescale, "_multi_BehavioralInactivation_", scaler, "_", suffix, ".rds"))
saveRDS(Social_omega_day, paste0(timescale, "_multi_Social_", scaler, "_", suffix, ".rds"))

timescale = "Weekly"
Sleep_omega_week <- multilevel.omega(data_weekly[, Sleep], cluster = data_weekly$uid, missing = "listwise",nrep = 100000, optim.method = c("nlminb"))# No solution (implausible values)
Activity_omega_week <- multilevel.omega(data_weekly[, Activity], cluster = data_weekly$uid, missing = "listwise") # (cautious interpretation)
AffectiveDysregulation_omega_week <- multilevel.omega(data_weekly[, AffectiveDysregulation], cluster = data_weekly$uid, missing = "listwise") 
BehavioralInactivation_omega_week <- multilevel.omega(data_weekly[, BehavioralInactivation], cluster = data_weekly$uid, missing = "listwise",optim.method = c("nlminb")) 
Social_omega_week <- multilevel.omega(data_weekly[, Social], cluster = data_weekly$uid, missing = "listwise")

saveRDS(Sleep_omega_week, paste0(timescale, "_multi_Sleep_", scaler, "_", suffix, ".rds"))
saveRDS(Activity_omega_week, paste0(timescale, "_multi_Activity_", scaler, "_", suffix, ".rds"))
saveRDS(AffectiveDysregulation_omega_week, paste0(timescale, "_multi_AffectiveDysregulation_", scaler, "_", suffix, ".rds"))
saveRDS(BehavioralInactivation_omega_week, paste0(timescale, "_multi_BehavioralInactivation_", scaler, "_", suffix, ".rds"))
saveRDS(Social_omega_week, paste0(timescale, "_multi_Social_", scaler, "_", suffix, ".rds"))

timescale = "Monthly"

Sleep_omega_month <- multilevel.omega(data_monthly[, Sleep], cluster = data_monthly$uid, missing = "listwise",nrep = 100000, optim.method = c("nlminb")) # No solution (implausible values)
Activity_omega_month <- multilevel.omega(data_monthly[, Activity], cluster = data_monthly$uid, missing = "listwise") #(cautious interpretation)
AffectiveDysregulation_omega_month <- multilevel.omega(data_monthly[, AffectiveDysregulation], cluster = data_monthly$uid, missing = "listwise") 
BehavioralInactivation_omega_month <- multilevel.omega(data_monthly[, BehavioralInactivation], cluster = data_monthly$uid, missing = "listwise",optim.method = c("nlminb")) # cautious interpretation
Social_omega_month <- multilevel.omega(data_monthly[, Social], cluster = data_monthly$uid, missing = "listwise") 

saveRDS(Sleep_omega_month, paste0(timescale, "_multi_Sleep_", scaler, "_", suffix, ".rds"))
saveRDS(Activity_omega_month, paste0(timescale, "_multi_Activity_", scaler, "_", suffix, ".rds"))
saveRDS(AffectiveDysregulation_omega_month, paste0(timescale, "_multi_AffectiveDysregulation_", scaler, "_", suffix, ".rds"))
saveRDS(BehavioralInactivation_omega_month, paste0(timescale, "_multi_BehavioralInactivation_", scaler, "_", suffix, ".rds"))
saveRDS(Social_omega_month, paste0(timescale, "_multi_Social_", scaler, "_", suffix, ".rds"))




##### Load results:
setwd("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Results")

# log
# Standard_main, Log_Standard_main, MinMax_main, Log_MinMax_main
# Standard_imp, Log_Standard_imp, MinMax_imp, Log_MinMax_imp

scaler <- c("Standard")
suffix <- c("main")

create_table(scaler, suffix)

create_table <- function(scaler, suffix){
  setwd("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Results")
  
  suffixes   <- c("_d", "_w", "_m")
  prefixes <- c("Daily", "Weekly", "Monthly")
  
  for (i in seq_along(prefixes)) {
    for (con in c("Sleep","Activity","AffectiveDysregulation","BehavioralInactivation","Social")) {
      assign(
        paste0(con, "_Gen", suffixes[i]),
        readRDS(paste0(prefixes[i], "_", con, "_",scaler, "_", suffix, ".rds")),
        envir = .GlobalEnv
      )
    }
  }
  
  timescales <- c("Daily", "Weekly", "Monthly")
  suffixes   <- c("_d", "_w", "_m")
  constructs <- c("Sleep","Activity","AffectiveDysregulation","BehavioralInactivation","Social")
  
  for (i in seq_along(timescales)) {
    for (con in constructs) {
      assign(
        paste0(con, "_Nez", suffixes[i]),
        readRDS(paste0(timescales[i], "_Nez_", con, "_", scaler, "_", suffix, ".rds")),
        envir = .GlobalEnv
      )
    }
  }
  
  timescales <- c("Daily", "Weekly", "Monthly")
  suffixes   <- c("_day", "_week", "_month")
  constructs <- c("Sleep","Activity","AffectiveDysregulation","BehavioralInactivation","Social")
  
  for (i in seq_along(timescales)) {
    for (con in constructs) {
      assign(
        paste0(con, "_omega", suffixes[i]),
        readRDS(paste0(timescales[i], "_multi_", con, "_", scaler, "_", suffix, ".rds")),
        envir = .GlobalEnv
      )
    }
  }
  
  
  
  safe_omega <- function(obj, idx, digits = 2) {
    tryCatch({
      val <- obj$result$omega$omega[idx]
      if (is.na(val)) NA else round(val, digits)
    }, error = function(e) NA)
  }
  
  # ----- Overview -----
  
  # Create summary data.frame
  reliability_summary <- data.frame(
    "Construct" = rep(c("Sleep","Activity","AffectiveDysregulation","BehavioralInactivation","Social"), each = 2),
    "Level" = rep(c("Between","Within"), times = 5),
    "Cranford_Day" = c(
      round(Sleep_Gen_d[4],2), round(Sleep_Gen_d[5],2),
      round(Activity_Gen_d[4],2), round(Activity_Gen_d[5],2),
      round(AffectiveDysregulation_Gen_d[4],2), round(AffectiveDysregulation_Gen_d[5],2),
      round(BehavioralInactivation_Gen_d[4],2), round(BehavioralInactivation_Gen_d[5],2),
      round(Social_Gen_d[4],2), round(Social_Gen_d[5],2)
    ),
    "Cranford_Week" = c(
      round(Sleep_Gen_w[4],2), round(Sleep_Gen_w[5],2),
      round(Activity_Gen_w[4],2), round(Activity_Gen_w[5],2),
      round(AffectiveDysregulation_Gen_w[4],2), round(AffectiveDysregulation_Gen_w[5],2),
      round(BehavioralInactivation_Gen_w[4],2), round(BehavioralInactivation_Gen_w[5],2),
      round(Social_Gen_w[4],2), round(Social_Gen_w[5],2)
    ),
    "Cranford_Month" = c(
      round(Sleep_Gen_m[4],2), round(Sleep_Gen_m[5],2),
      round(Activity_Gen_m[4],2), round(Activity_Gen_m[5],2),
      round(AffectiveDysregulation_Gen_m[4],2), round(AffectiveDysregulation_Gen_m[5],2),
      round(BehavioralInactivation_Gen_m[4],2), round(BehavioralInactivation_Gen_m[5],2),
      round(Social_Gen_m[4],2), round(Social_Gen_m[5],2)
    ),
    "Nezlek_Day" = c(
      round(Sleep_Nez_d[1],2), round(Sleep_Nez_d[2],2),
      round(Activity_Nez_d[1],2), round(Activity_Nez_d[2],2),
      round(AffectiveDysregulation_Nez_d[1],2), round(AffectiveDysregulation_Nez_d[2],2),
      round(BehavioralInactivation_Nez_d[1],2), round(BehavioralInactivation_Nez_d[2],2),
      round(Social_Nez_d[1],2), round(Social_Nez_d[2],2)
    ),
    "Nezlek_Week" = c(
      round(Sleep_Nez_w[1],2), round(Sleep_Nez_w[2],2),
      round(Activity_Nez_w[1],2), round(Activity_Nez_w[2],2),
      round(AffectiveDysregulation_Nez_w[1],2), round(AffectiveDysregulation_Nez_w[2],2),
      round(BehavioralInactivation_Nez_w[1],2), round(BehavioralInactivation_Nez_w[2],2),
      round(Social_Nez_w[1],2), round(Social_Nez_w[2],2)
    ),
    "Nezlek_Month" = c(
      round(Sleep_Nez_m[1],2), round(Sleep_Nez_m[2],2),
      round(Activity_Nez_m[1],2), round(Activity_Nez_m[2],2),
      round(AffectiveDysregulation_Nez_m[1],2), round(AffectiveDysregulation_Nez_m[2],2),
      round(BehavioralInactivation_Nez_m[1],2), round(BehavioralInactivation_Nez_m[2],2),
      round(Social_Nez_m[1],2), round(Social_Nez_m[2],2)
    ),
    "Omega_Day" = c(
      safe_omega(Sleep_omega_day, 2), safe_omega(Sleep_omega_day, 1),
      safe_omega(Activity_omega_day, 2), safe_omega(Activity_omega_day, 1),
      safe_omega(AffectiveDysregulation_omega_day, 2), safe_omega(AffectiveDysregulation_omega_day, 1),
      safe_omega(BehavioralInactivation_omega_day, 2), safe_omega(BehavioralInactivation_omega_day, 1),
      safe_omega(Social_omega_day, 2), safe_omega(Social_omega_day, 1)
    ),
    "Omega_Week" = c(
      safe_omega(Sleep_omega_week, 2), safe_omega(Sleep_omega_week, 1),
      safe_omega(Activity_omega_week, 2), safe_omega(Activity_omega_week, 1),
      safe_omega(AffectiveDysregulation_omega_week, 2), safe_omega(AffectiveDysregulation_omega_week, 1),
      safe_omega(BehavioralInactivation_omega_week, 2), safe_omega(BehavioralInactivation_omega_week, 1),
      safe_omega(Social_omega_week, 2), safe_omega(Social_omega_week, 1)
    ),
    "Omega_Month" = c(
      safe_omega(Sleep_omega_month, 2), safe_omega(Sleep_omega_month, 1),
      safe_omega(Activity_omega_month, 2), safe_omega(Activity_omega_month, 1),
      safe_omega(AffectiveDysregulation_omega_month, 2), safe_omega(AffectiveDysregulation_omega_month, 1),
      safe_omega(BehavioralInactivation_omega_month, 2), safe_omega(BehavioralInactivation_omega_month, 1),
      safe_omega(Social_omega_month, 2), safe_omega(Social_omega_month, 1)
    )
  )
  
  library(flextable)
  
  ft <- flextable(reliability_summary) %>% 
    delete_part(part = "header") |>
    delete_part(part = "footer") |>
    merge_at(i = 1:2, j = 1, part = "body")  %>%
    merge_at(i = 3:4, j = 1, part = "body")  %>%
    merge_at(i = 5:6, j = 1, part = "body")  %>%
    merge_at(i = 7:8, j = 1, part = "body")  %>%
    merge_at(i = 9:10, j = 1, part = "body")  %>%
    add_header_row(
      values = c("", "", "Day", "Week", "Month", "Day", "Week", "Month", "Day", "Week", "Month"),
      colwidths = rep(1, 11)
    ) |>
    add_header_row(
      values = c("", "", "Cranford","Nezlek", "Omega"),
      colwidths = c(1, 1,3, 3, 3)
    ) |>
    merge_h(part = "header") |>
    # bold(part = "header") |>
    align(align = "center", part = "header") 
  
  # Identify numeric columns for heatmap
  num_cols <- 3:11
  
  # Heatmap palette
  heat_palette <- colorRampPalette(c( "#d95f02", "moccasin", "#1b9e77"))
  
  
  get_heatmap_colors <- function(x, midpoint = 0.5, na_col = "white") {
    
    pal <- heat_palette(100)
    
    # Handle NA
    out <- rep(na_col, length(x))
    
    ok <- !is.na(x)
    
    # Rescale so that midpoint maps to 0.5
    x_rescaled <- scales::rescale_mid(x[ok], mid = midpoint, to = c(0, 1))
    
    idx <- pmax(1, pmin(100, round(x_rescaled * 99) + 1))
    
    out[ok] <- pal[idx]
    out
  }
  
  
  for (col in num_cols) {
    ft <- bg(
      ft,
      j = col,
      i = seq_len(nrow(reliability_summary)),
      bg = get_heatmap_colors(reliability_summary[[col]], midpoint = 0.5)
    )
  }
  
  
  ft <- autofit(ft)
  return(ft)
}


## Abstract
sapply(reliability_summary[reliability_summary$Level == "Between",], function(x) c(min = min(x, na.rm = TRUE),
                         max = max(x, na.rm = TRUE)))

reliability_summary <- reliability_summary %>%
  rowwise() %>%
  mutate(
    min_row = min(c_across(where(is.numeric)), na.rm = TRUE),
    max_row = max(c_across(where(is.numeric)), na.rm = TRUE)
  ) %>%
  ungroup()

reliability_summary[reliability_summary$Level == "Between",]

min(reliability_summary[reliability_summary$Level == "Between",c(3:11)], na.rm = TRUE)
max(reliability_summary[reliability_summary$Level == "Between",c(3:11)], na.rm = TRUE)
mean(
  unlist(reliability_summary[reliability_summary$Level == "Between", 3:11]),
  na.rm = TRUE
)

mean(
  unlist(reliability_summary[reliability_summary$Level == "Within" & !reliability_summary$Construct == "Activity", 3:11]),
  na.rm = TRUE
)

min(
  unlist(reliability_summary[reliability_summary$Level == "Within" & !reliability_summary$Construct == "Activity", 3:11]),
  na.rm = TRUE
)

max(
  unlist(reliability_summary[reliability_summary$Level == "Within" & !reliability_summary$Construct == "Activity", 3:11]),
  na.rm = TRUE
)
#--------------------------------------
######## Test - Retest #########
#--------------------------------------

# Define a helper function for test-retest reliability
run_test_retest <- function(data_path, constructs, time_var, id_var = "uid", label = NULL) {
  # Load data
  data <- read.csv(data_path)
  
  # Initialize list to store results
  results <- vector("list", length = length(constructs))
  
  # Loop over all constructs
  for (i in seq_along(constructs)) {
    results[[i]] <- Test_Retest(data, constructs[i], time_var, id_var)
  }
  
  # Name the results for clarity
  names(results) <- constructs
  
  # Run the total score test if provided
  if (!is.null(label)) {
    overall_result <- Test_Retest(data, label, time_var, id_var)
  } else {
    overall_result <- NULL
  }
  
  # Return all results in one list
  return(list(results = results, overall = overall_result))
}



#### Daily ####
daily_path <- "/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Daily_Cleaned_Dataset_Standard_Complete.csv"

sleep_day <- run_test_retest(daily_path, Sleep, "day", "uid", "Sleep_Score")
activity_day <- run_test_retest(daily_path, Activity, "day", "uid", "Activity_Score")
affectivedysregulation_day <- run_test_retest(daily_path, AffectiveDysregulation, "day", "uid", "AffectiveDysregulation_Score")
behavioralinactivation_day <- run_test_retest(daily_path, BehavioralInactivation, "day", "uid", "BehavioralInactivation_Score")
social_day <- run_test_retest(daily_path, Social, "day", "uid", "SocialWithdrawal_Score")

###### Weekly #####
weekly_path <- "/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Weekly_Cleaned_Dataset_Standard_Complete.csv"

sleep_week <- run_test_retest(weekly_path, Sleep, "week_num", "uid", "Sleep_Score")
activity_week <- run_test_retest(weekly_path, Activity, "week_num", "uid", "Activity_Score")
affectivedysregulation_week <- run_test_retest(weekly_path, AffectiveDysregulation, "week_num", "uid", "AffectiveDysregulation_Score")
behavioralinactivation_week <- run_test_retest(weekly_path, BehavioralInactivation, "week_num", "uid", "BehavioralInactivation_Score")
social_week <- run_test_retest(weekly_path, Social, "week_num", "uid", "SocialWithdrawal_Score")

### Monthly #####
monthly_path <- "/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Monthly_Cleaned_Dataset_Standard_Complete.csv"

sleep_month <- run_test_retest(monthly_path, Sleep, "month_num", "uid", "Sleep_Score")
activity_month <- run_test_retest(monthly_path, Activity, "month_num", "uid", "Activity_Score")
affectivedysregulation_month <- run_test_retest(monthly_path, AffectiveDysregulation, "month_num", "uid", "AffectiveDysregulation_Score")
behavioralinactivation_month <- run_test_retest(monthly_path, BehavioralInactivation, "month_num", "uid", "BehavioralInactivation_Score")
social_month <- run_test_retest(monthly_path, Social, "month_num", "uid", "SocialWithdrawal_Score")


##### Viz ####

library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

# Function to flatten one run_test_retest() output into a tidy df
extract_construct_df <- function(result_obj, construct_name, timeframe) {
  res_list <- result_obj$results
  overall <- result_obj$overall
  
  df <- map_dfr(names(res_list), function(varname) {
    tmp <- as.data.frame(t(unlist(res_list[[varname]])))
    tmp$Variable <- varname
    tmp
  })
  
  # Add construct + timeframe info
  df <- df %>%
    mutate(
      Construct = construct_name,
      Timeframe = timeframe
    )
  
  # Add overall score
  if (!is.null(overall)) {
    overall_df <- as.data.frame(t(unlist(overall)))
    overall_df$Variable <- paste0(construct_name, "_Score")
    overall_df$Construct <- construct_name
    overall_df$Timeframe <- timeframe
    df <- bind_rows(df, overall_df)
  }
  
  return(df)
}

# Combine all constructs and timeframes
results_df <- bind_rows(
  extract_construct_df(sleep_day, "Sleep", "Daily"),
  extract_construct_df(activity_day, "Activity", "Daily"),
  extract_construct_df(affectivedysregulation_day, "Affective Dysregulation", "Daily"),
  extract_construct_df(behavioralinactivation_day, "Behavioral Inactivation", "Daily"),
  extract_construct_df(social_day, "Social", "Daily"),
  
  extract_construct_df(sleep_week, "Sleep", "Weekly"),
  extract_construct_df(activity_week, "Activity", "Weekly"),
  extract_construct_df(affectivedysregulation_week, "Affective Dysregulation", "Weekly"),
  extract_construct_df(behavioralinactivation_week, "Behavioral Inactivation", "Weekly"),
  extract_construct_df(social_week, "Social", "Weekly"),
  
  extract_construct_df(sleep_month, "Sleep", "Monthly"),
  extract_construct_df(activity_month, "Activity", "Monthly"),
  extract_construct_df(affectivedysregulation_month, "Affective Dysregulation", "Monthly"),
  extract_construct_df(behavioralinactivation_month, "Behavioral Inactivation", "Monthly"),
  extract_construct_df(social_month, "Social", "Monthly")
)

#Daily estimates generally showed lower stability, with many measures falling in the low-to-moderate ICC range (mean ICC = 0.37). In contrast, weekly and monthly aggregates tended to yield higher temporal stability across most domains (mean ICC = 0.64 and 0.78).
mean(results_df$V1[results_df$Timeframe == "Weekly"])


construct = "Activity"
mean(results_df$V1[results_df$Timeframe == "Daily" & results_df$Construct == construct & results_df$IsOverall == "Overall Score"])
mean(results_df$V1[results_df$Timeframe == "Weekly" & results_df$Construct == construct& results_df$IsOverall == "Overall Score"])
mean(results_df$V1[results_df$Timeframe == "Monthly" & results_df$Construct == construct & results_df$IsOverall == "Overall Score"])

max(results_df$V1[results_df$Variable == "garmin_hrv_mean_ep_0"])

mean(results_df$V1[results_df$Variable == "loc_dist_ep_0"])
min(results_df$V1[results_df$Variable == "loc_dist_ep_0"])
max(results_df$V1[results_df$Variable == "loc_dist_ep_0"])



mean(results_df$V1[results_df$Variable == "gm_sleep_duration_awake"])


library(dplyr)
library(ggplot2)

# Ensure you have the IsOverall column
results_df <- results_df %>%
  mutate(
    IsOverall = ifelse(grepl("_Score$", Variable), "Overall Score", "Item")
  )

# Reorder Variable so overall score is always first within each construct
results_df <- results_df %>%
  group_by(Construct) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        unique(Variable[IsOverall == "Overall Score"]),
        sort(unique(Variable[IsOverall == "Item"]))
      )
    )
  ) %>%
  ungroup()

my_timeframe_colors <- c(
  "Daily" = "#1b9e77",     # teal green
  "Weekly" = "#d95f02",    # orange
  "Monthly" = "#7570b3"    # purple
)

plot = ggplot(results_df, aes(y = Variable, x = V1, color = Timeframe)) +
  # Grey background rectangle for overall scores
  geom_tile(
    data = subset(results_df, IsOverall == "Overall Score"),
    aes(y = Variable, x = V1),
    fill = "grey90",   # light grey
    width = Inf,       # fill horizontally
    height = 0.5,      # cover the variable row
    inherit.aes = FALSE
  ) +
  geom_point(size = 3, alpha = 0.9) +
  facet_wrap(~ Construct, scales = "free_y", ncol = 2) +
  labs(
    title = "Consistency over Time (Test-Retest)",
    y = "",
    x = "ICC",
    color = ""
    #shape = ""                
  ) +
  scale_color_manual(values = my_timeframe_colors) +  
  #scale_shape_manual(values = c("Item" = 16, "Overall Score" = 17)) +
  theme_ipsum(base_size = 12) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 12),
    panel.spacing = unit(0.5, "lines")
  ) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ggsave("testretestnew.jpg", plot = plot, width = 9, height = 10)


######


library(ggplot2)
library(hrbrthemes)

library(dplyr)

# Base path
base_path <- "/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data"

# Read Monthly and Daily datasets
monthly <- read.csv(file.path(base_path, "Monthly_Cleaned_Dataset_Log_MinMax_Complete.csv"))
daily   <- read.csv(file.path(base_path, "Daily_Cleaned_Dataset_Log_MinMax_Complete.csv"))
weekly   <- read.csv(file.path(base_path, "Weekly_Cleaned_Dataset_Log_MinMax_Complete.csv"))


# Add a column to indicate data source
monthly$time <- "Monthly"
daily$time   <- "Daily"
weekly$time   <- "Weekly"

# Combine into one dataset
all_data <- bind_rows(monthly, daily, weekly)

# Subset 10 random users
uids <- sample(unique(all_data$uid), 10)
sub_data <- filter(all_data, uid %in% uids)

# Plot
ggplot(sub_data, aes(x = day_num, y = Sleep_Score, color = time, group = interaction(time, uid))) +
  geom_line() +
  facet_wrap(~ uid) +
  theme_ipsum() +
  labs(title = "Sleep Score Over Time",
       x = "Day Number",
       y = "Sleep Score",
       color = "Data Source") 




data[[time_var]] <- ave(seq_along(data[[uid_var]]),data[[uid_var]], FUN = seq_along)
data[[time_var]] <- ave(seq_along(data[[uid_var]]),data[[uid_var]], FUN = seq_along)

construct = BehavioralInactivation # Activity,AffectiveDysregulation, Social, Sleep

long <-reshape(data[,c(uid_var, time_var, construct)], direction ="long", idvar=c(uid_var, time_var), timevar = "item",varying =list(construct), v.names   ="resp")
uids <- sample(unique(long$uid), 10)
sub_data <- filter(long, uid %in% uids)

plot = ggplot(sub_data, aes(x = day, y = resp, color = as.factor(item))) +
  geom_line() +
  facet_wrap(~ uid) +
  theme_ipsum() +
  labs(title = "Score Over Time",
       x = "Day Number",
       y = "Score",
       color = "Feature") 





#------------------------------------------
########  Inter-Rater Reliability #########
#------------------------------------------
library(irr)

# Steps: step_count2 gm_dailies_step, garmin_steps
# Sleep: gm_sleep_duration, sleep_duration
# Stress: gm_dailies_average_stress, garmin_stress_mean_ep_0

# summarized raw sensor data will be used
data <- read.csv("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Daily_Cleaned_Dataset.csv")

icc_steps <- irr::icc(data[,c("step_count2","gm_dailies_step","garmin_steps")], model = "twoway", type = "agreement", unit = "single")$value
icc_sleep <- irr::icc(data[,c("gm_sleep_duration","sleep_duration")], model = "twoway", type = "agreement", unit = "single")$value
icc_stress <- irr::icc(data[,c("gm_dailies_average_stress","garmin_stress_mean_ep_0")], model = "twoway", type = "agreement", unit = "single")$value

# Random intercepts for subject and rater

long_data <- data %>%
  pivot_longer(
    cols = c("step_count2", "gm_dailies_step", "garmin_steps"),
    names_to = "rater",
    values_to = "score"
  ) %>%
  rename(subject = uid)  

model <- lmer(score ~ (1 | subject) + (1 | rater), data = long_data)

library(performance)
icc_ml_step = performance::icc(model)[,1]


# Random intercepts for subject and rater

long_data <- data %>%
  pivot_longer(
    cols = c("gm_sleep_duration","sleep_duration"),
    names_to = "rater",
    values_to = "score"
  ) %>%
  rename(subject = uid)  

model <- lmer(score ~ (1 | subject) + (1 | rater), data = long_data)

icc_ml_sleep = performance::icc(model)[,1]

long_data <- data %>%
  pivot_longer(
    cols = c("gm_dailies_average_stress","garmin_stress_mean_ep_0"),
    names_to = "rater",
    values_to = "score"
  ) %>%
  rename(subject = uid)  

model <- lmer(score ~ (1 | subject) + (1 | rater), data = long_data)

icc_ml_stress = performance::icc(model)[,1]

## Correlation
library(ggplot2)
library(reshape2)
library(patchwork)
library(gridExtra)
library(dplyr)
library(hrbrthemes)


# ---- Step 1: Melt matrices ----

melt_corr <- function(corr_matrix) melt(corr_matrix, na.rm = TRUE)
melt_steps <- melt_corr(cor(data[,c("step_count2","gm_dailies_step","garmin_steps")], use ="complete.obs"))
melt_sleep <- melt_corr(cor(data[,c("gm_sleep_duration","sleep_duration")], use ="complete.obs"))
melt_stress <- melt_corr(cor(data[,c("gm_dailies_average_stress","garmin_stress_mean_ep_0")], use ="complete.obs"))

# ---- Step 2: Heatmap function ----
plot_heatmap <- function(df, title) {
  ggplot(df, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low =  "#d95f02",# "moccasin"
      high = "#1b9e77", # color for +1
      limits = c(-1, 1)
    ) +
    geom_text(aes(label = round(value, 2)), size = 5, family = "Arial Narrow") +
    theme_ipsum() +
    labs(title = title, x = "", y = "") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(hjust = 1),
          plot.title = element_text(face = "bold", size = 14)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(hjust = 1),
          plot.title = element_text(face = "bold", size = 14),
          legend.position = "none")  # removes the legend
}

# ---- Step 3: Create heatmaps ----
p_steps <- plot_heatmap(melt_steps, "Steps")
p_sleep <- plot_heatmap(melt_sleep, "Sleep")
p_stress <- plot_heatmap(melt_stress, "Stress")

# ---- Step 4: ICC tables as white ----
create_icc_table_white <- function(methods, values) {
  df <- data.frame(Method = methods, Value = round(values, 3))
  
  ttheme <- ttheme_default(
    core = list(fg_params=list(cex=1, fontfamily="Arial Narrow", col="black"),
                bg_params = list(fill = "white", col = NA)),  # set cell background to white
    colhead = list(fg_params=list(cex=1.1, fontfamily="Arial Narrow", fontface="bold", col="black"),
                   bg_params = list(fill = "white", col = NA))  # header background white
  )
  
  tableGrob(df, rows = NULL, theme = ttheme)
}

table_steps <- create_icc_table_white(c("ICC (irr::icc)", "ICC (ML lmer)"), c(icc_steps, icc_ml_step))
table_sleep <- create_icc_table_white(c("ICC (irr::icc)", "ICC (ML lmer)"), c(icc_sleep, icc_ml_sleep))
table_stress <- create_icc_table_white(c("ICC (irr::icc)", "ICC (ML lmer)"), c(icc_stress, icc_ml_stress))

# ---- Step 5: Combine heatmap + table vertically ----
plot_steps <- arrangeGrob(p_steps, table_steps, ncol = 1, heights = c(3,1))
plot_sleep <- arrangeGrob(p_sleep, table_sleep, ncol = 1, heights = c(3,1))
plot_stress <- arrangeGrob(p_stress, table_stress, ncol = 1, heights = c(3,1))

# ---- Step 6: Arrange all three side by side ----
plot = grid.arrange(plot_steps, plot_stress, ncol = 2)

ggsave("interrater.jpg", plot = plot, width = 11, height = 5)


################
mlr_pa <- psych::mlr(data, grp = "uid", Time = "day", items = c('gm_dailies_step', 'step_count2'), 
                     lmer = TRUE, lme = FALSE, alpha = FALSE, aov = FALSE)

mlr_pa$RkRn # Check if this is Nezlek 2012 method: estimate of how reliable mean response is not of how consisently people respond across items!
mlr_pa$Rcn
mlr_pa$Rc









### ===== Other Code ====
# Activity & affective dysregulation: features seem to map into the same construct
# Sleep, Behavioral Inactivation: features seem to map into different constructs
# Social: mixed results

library(lavaan)
library(misty)

# # Non-log transformed
# df_daily <- read.csv('/Users/nenbarsalo/Downloads/4_Reliability Project_DataNoise/Daily_Cleaned_Dataset_MinMax_Complete_noise.csv')
# df_weekly <- read.csv('/Users/nenbarsalo/Downloads/4_Reliability Project_DataNoise/Weekly_Cleaned_Dataset_MinMax_Complete_noise.csv')
# df_monthly <- read.csv('/Users/nenbarsalo/Downloads/4_Reliability Project_DataNoise/Monthly_Cleaned_Dataset_MinMax_Complete_noise.csv')
# 
# # Log transformed
# df_daily_log <- read.csv('/Users/nenbarsalo/Downloads/4_Reliability Project_DataNoise/Daily_Cleaned_Dataset_Log_MinMax_Complete_noise.csv')
# df_weekly_log <- read.csv('/Users/nenbarsalo/Downloads/4_Reliability Project_DataNoise/Weekly_Cleaned_Dataset_Log_MinMax_Complete_noise.csv')
# df_monthly_log <- read.csv('/Users/nenbarsalo/Downloads/4_Reliability Project_DataNoise/Monthly_Cleaned_Dataset_Log_MinMax_Complete_noise.csv')


# Non-log transformed
df_daily <- read.csv("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Daily_Cleaned_Dataset_MinMax_Complete.csv")
df_weekly <- read.csv("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Weekly_Cleaned_Dataset_MinMax_Complete.csv")
df_monthly <- read.csv("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Monthly_Cleaned_Dataset_MinMax_Complete.csv")

# Log transformed
df_daily_log <- read.csv("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Daily_Cleaned_Dataset_Log_MinMax_Complete.csv")
df_weekly_log <- read.csv("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Weekly_Cleaned_Dataset_Log_MinMax_Complete.csv")
df_monthly_log <- read.csv("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Monthly_Cleaned_Dataset_Log_MinMax_Complete.csv")

# Define the items for each construct
constructs_items <- list(
  `Sleep Disturbance` = c('gm_sleep_duration', 'gm_sleep_duration_awake', 'gm_sleep_duration_deep', 'gm_sleep_duration_rem',  "sleep_duration"),
  `Activity` = c('gm_dailies_step', 'step_count2', 'garmin_steps', 'act_still_ep_0', 'gm_dailies_active_kcal', 'gm_dailies_active_sec', 'gm_dailies_distance', 'gm_dailies_moderate_sec'),
  `Affective Dysregulation` = c('garmin_hrv_mean_ep_0', 'gm_dailies_activity_stress_duration', 'gm_dailies_average_stress', 'garmin_stress_mean_ep_0', 'gm_dailies_high_stress_duration', 'gm_dailies_low_stress_duration', 'gm_dailies_max_stress', 'gm_dailies_medium_stress_duration'),
  `Behavioral Inactivation` = c('unlock_duration_ep_0', 'unlock_num_ep_0', 'home_ep_0', 'loc_visit_num_ep_0', 'loc_dist_ep_0'),
  `Social` = c('audio_convo_duration_ep_0', 'audio_convo_num_ep_0', 'call_in_duration_ep_0', 'call_in_num_ep_0', 'call_out_duration_ep_0', 'call_out_num_ep_0', 'sms_in_num_ep_0', 'sms_out_num_ep_0')
)

# Combine all items for the Depression construct
constructs_items$`Depression` <- unique(unlist(constructs_items))

# Define the datasets to loop through
datasets <- list(
  list(name = "Daily", df = df_daily, df_log = df_daily_log),
  list(name = "Weekly", df = df_weekly, df_log = df_weekly_log),
  list(name = "Monthly", df = df_monthly, df_log = df_monthly_log)
)

# Initialize empty data frame to store all results
all_results_df <- data.frame(
  Dataset = character(),
  Construct = character(),
  Harmonic_Mean = numeric(),
  Metric = character(),
  lavaan_est = numeric(),
  misty_est = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each dataset
for (dataset in datasets) {
  print(dataset$name)
  # Loop through non-log and log-transformed versions
  for (is_log in c(FALSE, TRUE)) {
    # Get the correct dataframe and dataset name
    if (is_log) {
      current_df <- dataset$df_log
      dataset_name <- paste0(dataset$name, " (Log Transformed)")
    } else {
      current_df <- dataset$df
      dataset_name <- paste0(dataset$name, " (Non-Log Transformed)")
    }
    
    # Calculate the harmonic mean of cluster sizes
    compliance_counts <- table(current_df$uid)
    harmonic_mean <- length(compliance_counts) / sum(1 / compliance_counts)
    
    # Loop through each construct
    for (construct_name in names(constructs_items)) {
      items <- constructs_items[[construct_name]]
      
      model_string <- "
level: 1
  wf =~ NA*item_1_name + L1*item_1_name"
      
      if (length(items) > 1) {
        for (i in 2:length(items)) {
          model_string <- paste0(model_string, " + L", i, "*", items[i])
        }
      }
      
      model_string <- paste0(model_string, "
  wf ~~ 1*wf
")
      
      for (i in 1:length(items)) {
        model_string <- paste0(model_string, "  ", items[i], " ~~ ew", i, " * ", items[i], "\n")
      }
      
      model_string <- paste0(model_string, "
level: 2
  bf =~ NA*item_1_name + L1*item_1_name")
      
      if (length(items) > 1) {
        for (i in 2:length(items)) {
          model_string <- paste0(model_string, " + L", i, "*", items[i])
        }
      }
      
      model_string <- paste0(model_string, "
  bf ~~ bfvar * bf
")
      
      for (i in 1:length(items)) {
        model_string <- paste0(model_string, "  ", items[i], " ~~ eb", i, " * ", items[i], "\n")
      }
      
      sum_L <- paste0("(L1", paste(paste0(" + L", 2:length(items)), collapse = ""), ")^2")
      sum_ew <- paste0("(ew1", paste(paste0(" + ew", 2:length(items)), collapse = ""), ")")
      sum_eb <- paste0("(eb1", paste(paste0(" + eb", 2:length(items)), collapse = ""), ")")
      
      model_string <- paste0(model_string, "
      
# within-level omega (Geldhof, 2014; Lai 2021)
tilomgw := ", sum_L, " *1 / (", sum_L, " + ", sum_ew, ")
          
# between-level 'tilde' omega (Geldhof, 2014), not used due to reliability issues, see Castro-Alvarez et al supplemental material (https://osf.io/kdn4g)
tilomgb := ", sum_L, " * bfvar / (", sum_L, " * bfvar + ", sum_eb, ")

# between-level omega (Lai, 2021)
omgb := (", sum_L, " * bfvar) / ((", sum_L, " * (bfvar + 1 / ", harmonic_mean, ")) + ", sum_eb, " + ", sum_ew, " / ", harmonic_mean, ")
omg2l := (", sum_L, " * (bfvar + 1)) / ((", sum_L, " * (bfvar + 1)) + ", sum_eb, " + ", sum_ew, ")
")
      
      model_string <- gsub("item_1_name", items[1], model_string)
      
      # Fit the model and extract reliability estimates
      mlcfa_full <- tryCatch({
        cfa(model_string, data = current_df, cluster = "uid", orthogonal = TRUE,
            estimator = "MLR", missing = "FIML", std.lv = TRUE)
      }, error = function(e) {
        warning(paste0("lavaan failed for ", construct_name, " in ", dataset_name, ": ", e$message))
        return(NULL)
      })
      
      if (!is.null(mlcfa_full)) {
        cfa_est <- parameterEstimates(mlcfa_full)
        ml_omega_cfa <- cfa_est$est[cfa_est$label %in% c("tilomgw", "tilomgb", "omgb", "omg2l")]
      } else {
        ml_omega_cfa <- c(NA, NA, NA, NA)
      }
      
      # Use the misty package to get reliability estimates
      mlcfa_misty <- tryCatch({
        multilevel.omega(current_df[, items], cluster = current_df$uid, missing = "listwise")
      }, error = function(e) {
        warning(paste0("misty failed for ", construct_name, " in ", dataset_name, ": ", e$message))
        return(NULL)
      })
      
      if (!is.null(mlcfa_misty)) {
        ml_omega_misty <- c(mlcfa_misty$result$omega$omega[1], NA, mlcfa_misty$result$omega$omega[2], mlcfa_misty$result$omega$omega[3])
      } else {
        ml_omega_misty <- c(NA, NA, NA, NA)
      }
      
      print(mlcfa_misty)
      print(dataset_name)
      print(construct_name)
      print(ml_omega_cfa)
      print(ml_omega_misty)
      
      # Store the results in the main data frame
      temp_df <- data.frame(
        Dataset = rep(dataset_name,4),
        Construct = rep(construct_name,4),
        Harmonic_Mean = rep(harmonic_mean,4),
        Metric = c("omega_w",NA, "omega_b", "omega_2l"),
        lavaan_est = ml_omega_cfa,
        misty_est = ml_omega_misty,
        stringsAsFactors = FALSE
      )
      
      
      all_results_df <- rbind(all_results_df, temp_df)
    }
  }
}

print(all_results_df)

all_results_df_copu = all_results_df

write.csv(all_results_df,"all_results_rel.csv")
multilevel.omega(data[, Activity], cluster = data$uid, missing = "listwise")
