## Create synthetic Data

#install.packages("synthpop")
library(synthpop)

df <- read.csv("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/complete_feat_dataset.csv")

# Variables
variables_table2 <- c(
  "act_walking_ep_0", "act_running_ep_0",
  # Sleep 
  "gm_sleep_duration","sleep_duration", "gm_sleep_duration_awake", 
  "gm_sleep_duration_deep", "gm_sleep_duration_rem", "gm_sleep_quality",
  
  # Activity 
  "gm_dailies_step", "step_count2", "garmin_steps", 
  "act_still_ep_0", "gm_dailies_active_kcal", "gm_dailies_active_sec", 
  "gm_dailies_distance", "gm_dailies_moderate_sec",
  
  # Affective Dysregulation 
  "garmin_hrv_mean_ep_0", "gm_dailies_activity_stress_duration", 
  "gm_dailies_average_stress", "garmin_stress_mean_ep_0", 
  "gm_dailies_high_stress_duration", "gm_dailies_low_stress_duration", 
  "gm_dailies_max_stress", "gm_dailies_medium_stress_duration",
  
  # Behavioral Inactivation 
  "unlock_duration_ep_0", "unlock_num_ep_0", "home_ep_0", 
  "loc_visit_num_ep_0", "loc_dist_ep_0",
  
  # Social Withdrawal 
  "audio_convo_duration_ep_0", "audio_convo_num_ep_0", 
  "call_in_duration_ep_0", "call_in_num_ep_0", 
  "call_out_duration_ep_0", "call_out_num_ep_0", 
  "sms_in_num_ep_0", "sms_out_num_ep_0"
)

# ID columns
id_cols <- c("uid", "day")

# Keep only columns that exist in df
keep_cols <- c(id_cols, variables_table2)
df <- df[, intersect(keep_cols, colnames(df))]


df$day <- as.POSIXct(df$day, format = "%Y-%m-%d", tz = "UTC")

syn_complete_data <- syn(df, maxfaclevels = 100)

syn_complete_data$syn

write.csv(syn_complete_data$syn,"complete_feat_dataset.csv")

