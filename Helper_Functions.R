############# Helper Functions ################

# Optimizer: Nelder_Mead
# Change REML to maximum likelihood
# Scale time & uid
# Number of iterations (increase)
# Bayesian Framework (Stan): library(brms) -> do all in bayesian
create_table_imp <- function(scaler, suffix){

  suffixes   <- c("_d", "_w", "_m")
  prefixes <- c("Daily", "Weekly", "Monthly")
  
  for (i in seq_along(prefixes)) {
    for (con in c("Sleep","AffectiveDysregulation","BehavioralInactivation")) {
      assign(
        paste0(con, "_Gen", suffixes[i]),
        readRDS(paste0("Results/",prefixes[i], "_", con, "_",scaler, "_", suffix, ".rds")),
        envir = .GlobalEnv
      )
    }
  }
  
  timescales <- c("Daily", "Weekly", "Monthly")
  suffixes   <- c("_d", "_w", "_m")
  constructs <- c("Sleep","AffectiveDysregulation","BehavioralInactivation")
  
  for (i in seq_along(timescales)) {
    for (con in constructs) {
      assign(
        paste0(con, "_Nez", suffixes[i]),
        readRDS(paste0("Results/",timescales[i], "_Nez_", con, "_", scaler, "_", suffix, ".rds")),
        envir = .GlobalEnv
      )
    }
  }
  
  timescales <- c("Daily", "Weekly", "Monthly")
  suffixes   <- c("_day", "_week", "_month")
  constructs <- c("Sleep","AffectiveDysregulation","BehavioralInactivation")
  
  
  for (i in seq_along(timescales)) {
    for (con in constructs) {
      assign(
        paste0(con, "_omega", suffixes[i]),
        readRDS(paste0("Results/",timescales[i], "_multi_", con, "_", scaler, "_", suffix, ".rds")),
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
    "Construct" = rep(c("Sleep","AffectiveDysregulation","BehavioralInactivation"), each = 2),
    "Level" = rep(c("Between","Within"), times = 3),
    "Cranford_Day" = c(
      round(Sleep_Gen_d[4],2), round(Sleep_Gen_d[5],2),
      #round(Activity_Gen_d[4],2), round(Activity_Gen_d[5],2),
      round(AffectiveDysregulation_Gen_d[4],2), round(AffectiveDysregulation_Gen_d[5],2),
      round(BehavioralInactivation_Gen_d[4],2), round(BehavioralInactivation_Gen_d[5],2)
      #round(Social_Gen_d[4],2), round(Social_Gen_d[5],2)
    ),
    "Cranford_Week" = c(
      round(Sleep_Gen_w[4],2), round(Sleep_Gen_w[5],2),
      #round(Activity_Gen_w[4],2), round(Activity_Gen_w[5],2),
      round(AffectiveDysregulation_Gen_w[4],2), round(AffectiveDysregulation_Gen_w[5],2),
      round(BehavioralInactivation_Gen_w[4],2), round(BehavioralInactivation_Gen_w[5],2)
      #round(Social_Gen_w[4],2), round(Social_Gen_w[5],2)
    ),
    "Cranford_Month" = c(
      round(Sleep_Gen_m[4],2), round(Sleep_Gen_m[5],2),
      #round(Activity_Gen_m[4],2), round(Activity_Gen_m[5],2),
      round(AffectiveDysregulation_Gen_m[4],2), round(AffectiveDysregulation_Gen_m[5],2),
      round(BehavioralInactivation_Gen_m[4],2), round(BehavioralInactivation_Gen_m[5],2)
      #round(Social_Gen_m[4],2), round(Social_Gen_m[5],2)
    ),
    "Nezlek_Day" = c(
      round(Sleep_Nez_d[1],2), round(Sleep_Nez_d[2],2),
      #round(Activity_Nez_d[1],2), round(Activity_Nez_d[2],2),
      round(AffectiveDysregulation_Nez_d[1],2), round(AffectiveDysregulation_Nez_d[2],2),
      round(BehavioralInactivation_Nez_d[1],2), round(BehavioralInactivation_Nez_d[2],2)
      #round(Social_Nez_d[1],2), round(Social_Nez_d[2],2)
    ),
    "Nezlek_Week" = c(
      round(Sleep_Nez_w[1],2), round(Sleep_Nez_w[2],2),
      #round(Activity_Nez_w[1],2), round(Activity_Nez_w[2],2),
      round(AffectiveDysregulation_Nez_w[1],2), round(AffectiveDysregulation_Nez_w[2],2),
      round(BehavioralInactivation_Nez_w[1],2), round(BehavioralInactivation_Nez_w[2],2)
      # round(Social_Nez_w[1],2), round(Social_Nez_w[2],2)
    ),
    "Nezlek_Month" = c(
      round(Sleep_Nez_m[1],2), round(Sleep_Nez_m[2],2),
      #round(Activity_Nez_m[1],2), round(Activity_Nez_m[2],2),
      round(AffectiveDysregulation_Nez_m[1],2), round(AffectiveDysregulation_Nez_m[2],2),
      round(BehavioralInactivation_Nez_m[1],2), round(BehavioralInactivation_Nez_m[2],2)
      #round(Social_Nez_m[1],2), round(Social_Nez_m[2],2)
    ),
    "Omega_Day" = c(
      safe_omega(Sleep_omega_day, 2), safe_omega(Sleep_omega_day, 1),
      #safe_omega(Activity_omega_day, 2), safe_omega(Activity_omega_day, 1),
      safe_omega(AffectiveDysregulation_omega_day, 2), safe_omega(AffectiveDysregulation_omega_day, 1),
      safe_omega(BehavioralInactivation_omega_day, 2), safe_omega(BehavioralInactivation_omega_day, 1)
      #safe_omega(Social_omega_day, 2), safe_omega(Social_omega_day, 1)
    ),
    "Omega_Week" = c(
      safe_omega(Sleep_omega_week, 2), safe_omega(Sleep_omega_week, 1),
      #safe_omega(Activity_omega_week, 2), safe_omega(Activity_omega_week, 1),
      safe_omega(AffectiveDysregulation_omega_week, 2), safe_omega(AffectiveDysregulation_omega_week, 1),
      safe_omega(BehavioralInactivation_omega_week, 2), safe_omega(BehavioralInactivation_omega_week, 1)
      #safe_omega(Social_omega_week, 2), safe_omega(Social_omega_week, 1)
    ),
    "Omega_Month" = c(
      safe_omega(Sleep_omega_month, 2), safe_omega(Sleep_omega_month, 1),
      #safe_omega(Activity_omega_month, 2), safe_omega(Activity_omega_month, 1),
      safe_omega(AffectiveDysregulation_omega_month, 2), safe_omega(AffectiveDysregulation_omega_month, 1),
      safe_omega(BehavioralInactivation_omega_month, 2), safe_omega(BehavioralInactivation_omega_month, 1)
      #safe_omega(Social_omega_month, 2), safe_omega(Social_omega_month, 1)
    )
  )
  
  library(flextable)
  
  ft <- flextable(reliability_summary) %>% 
    delete_part(part = "header") |>
    delete_part(part = "footer") |>
    merge_at(i = 1:2, j = 1, part = "body")  %>%
    merge_at(i = 3:4, j = 1, part = "body")  %>%
    merge_at(i = 5:6, j = 1, part = "body")  %>%
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

create_table <- function(scaler, suffix){

  suffixes   <- c("_d", "_w", "_m")
  prefixes <- c("Daily", "Weekly", "Monthly")
  
  for (i in seq_along(prefixes)) {
    for (con in c("Sleep","Activity","AffectiveDysregulation","BehavioralInactivation","Social")) {
      assign(
        paste0(con, "_Gen", suffixes[i]),
        readRDS(paste0("Results/",prefixes[i], "_", con, "_",scaler, "_", suffix, ".rds")),
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
        readRDS(paste0("Results/",timescales[i], "_Nez_", con, "_", scaler, "_", suffix, ".rds")),
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
        readRDS(paste0("Results/",timescales[i], "_multi_", con, "_", scaler, "_", suffix, ".rds")),
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



set.seed("12361488")
run_gen_models <- function(data,
                           vars,        # named list
                           timescale,
                           time_var,
                           id_var = "uid",
                           optimizers = c("nloptwrap", "Nelder_Mead"),
                           REML_list = c(TRUE, FALSE),
                           save_dir = "/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Results",
                           scale_name = "minmax",
                           suffix = "main") {
  
  # ---- Input checks ----
  if (!is.list(vars) || is.null(names(vars))) {
    stop("`vars` must be a named list.")
  }
  
  n <- length(vars)
  results <- list()
  summary <- data.frame(
    timescale = timescale,
    suffix = suffix,
    scale_name = scale_name,
    variable   = names(vars),
    optimizer  = NA,
    REML       = NA,
    converged  = NA,
    singular   = NA,
    error      = NA,
    stringsAsFactors = FALSE
  )
  
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  
  # =====================================================================
  # MAIN LOOP
  # =====================================================================
  for (i in seq_len(n)) {
    
    var_name <- names(vars)[i]
    var_vec  <- vars[[i]]
    
    message("\nRunning: ", var_name, " (", timescale, ")")
    
    # ---- Column existence check ----
    missing_cols <- setdiff(var_vec, names(data))
    if (length(missing_cols) > 0) {
      summary$converged[i] <- FALSE
      summary$error[i] <- paste0("Missing columns: ", paste(missing_cols, collapse = ", "))
      setTxtProgressBar(pb, i)
      next
    }
    
    # =================================================================
    # TRY → FIT GENERALIZABILITY MODEL WITH MULTIPLE OPTIMIZERS / REML
    # =================================================================
    fit_success <- FALSE
    for (rem in REML_list) {
      for (opt in optimizers) {
        message("Trying optimizer: ", opt, " with REML = ", rem)
        
        model_result <- try(
          GeneralizabilityTheory(
            data,
            construct = var_vec,
            time_var  = time_var,
            uid_var   = id_var,
            REML_YN   = rem,
            optimizer = opt
          ),
          silent = TRUE
        )
        
        # Check if fit converged
        if (paste(model_result$converged, collapse = " | ") > 0 && !grepl('failed to converge', paste(model_result$converged, collapse = " | "))){
          message("Converged with optimizer: ", opt, " REML = ", rem)
          fit_success <- TRUE
          break
        }
      }
      if (fit_success) break
    }
    
    # =================================================================
    # RECORD RESULTS
    # =================================================================
    if (fit_success) {
      summary$optimizer[i] <- opt
      summary$REML[i]      <- rem
      summary$converged[i] <- TRUE
      summary$singular[i]  <- model_result$is_singular
      summary$error[i]     <- NA
      
      # ---- Save full result (model + reliabilities + flags) ----
      file_name <- paste0(timescale, "_", var_name, "_", scale_name, "_", suffix, ".rds")
      file_path <- file.path(save_dir, file_name)
      
      
      saveRDS(model_result$gt_rel_pa, file_path)
      message("Saved → ", file_path)
      
      
      results[[var_name]] <- model_result
    } else {
      summary$converged[i] <- FALSE
      summary$error[i] <- "No optimizer / REML combination converged"
      results[[var_name]] <- NULL
      
      # ---- Save full result (model + reliabilities + flags) ----
      file_name <- paste0(timescale, "_", var_name, "_", scale_name, "_", suffix, ".rds")
      file_path <- file.path(save_dir, file_name)
      
      
      saveRDS(c(NA,NA,NA,NA,NA), file_path)
      message("Saved → ", file_path)
      
    }
    
    file_path <- "/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/4_Reliability Project_Data/Results/Summary2.csv"
    
    write.table(
      summary[i,],
      file_path,
      sep = ",",
      row.names = FALSE,
      col.names = !file.exists(file_path),
      append = file.exists(file_path)
    )
    
    
    setTxtProgressBar(pb, i)
  }
  
  
  close(pb)
  
  return(list(results = results, summary = summary))
}



Test_Retest <- function(data,construct, time_var,uid_var){
  
  
  data[[time_var]] <- ave(seq_along(data[[uid_var]]),data[[uid_var]], FUN = seq_along)
  
  long <-reshape(data[,c(uid_var, time_var, construct)], direction ="long", idvar=c(uid_var, time_var), timevar = "item",varying =list(construct), v.names   ="resp")
  

  formula <- as.formula(paste("resp ~ (1 |", uid_var, ")"))
  
  ml3_pa<-lmer(formula,data =long,  control = lmerControl(optimizer="Nelder_Mead"))
  
  print(summary(ml3_pa))
  print(isSingular(ml3_pa, tol = 1e-4))
  print(VarCorr(ml3_pa, tol = 1e-4))
  
  
  var_ml3_pa <- as.data.frame(VarCorr(ml3_pa))
  print(var_ml3_pa)
  
  varP   <- var_ml3_pa[1, "vcov"]
  varE   <- var_ml3_pa[2, "vcov"] # ID effect (Between subject variation)
  
  print(paste("Variance P:",varP))

  k <- length(unique(long[[time_var]])) # maximum number of observations like in mlr; possibly replace with harmonic mean
  m <- length(construct)
  
  print(paste("Measurement Occasions:",k))
  print(paste("Items:",m))
  
  
  test_retest <- varP / (varP +  varE )

  print(round(test_retest,2))
  
  return(test_retest)
  
}

############
# 
# 
# construct = Sleep
# time_var = "day"
# REML_YN = FALSE
# optimizer = "nloptwrap"
# uid_var = "time"
# 
# 
# 
# 
# data[[time_var]] <- ave(seq_along(data[[uid_var]]),data[[uid_var]], FUN = seq_along)
# 
# 
# long <-reshape(data[,c(uid_var, time_var, construct)], direction ="long", idvar=c(uid_var, time_var), timevar = "item",varying =list(construct), v.names   ="resp")
# 
# mlr(
#   x = long,
#   grp = "uid",
#   Time = "day",
#   items = "item",
#   values = "resp",
#   long = TRUE
# )  
# # 


GeneralizabilityTheory_Nezlek <- function(data,construct, time_var,uid_var){
  
  data[[time_var]] <- ave(seq_along(data[[uid_var]]),data[[uid_var]], FUN = seq_along)
  
  long <- reshape(data[,c(uid_var, time_var, construct)], direction ="long", idvar=c(uid_var, time_var), timevar = "item",varying =list(construct), v.names = "resp")
  long$resp = long$resp * 10
  
  formula <- as.formula(
    paste0("resp ~  (1 | ", uid_var, "/",time_var,")") # resp ~ (1|uid) + (1|time:uid) (time:uid = random intercept for each day for each uid (occasions are nested in people))
  )

  #formula <- as.formula(paste("resp ~ (1 |", uid_var, ") + (1 |", uid_var,":",time_var, ")"))
  
  ml3_pa<-lmer(formula,long,  control = lmerControl(optimizer="Nelder_Mead"))
  
  print(summary(ml3_pa))
  print(isSingular(ml3_pa, tol = 1e-4))
  print(VarCorr(ml3_pa, tol = 1e-4))
  
  
  var_ml3_pa <- VarCorr(ml3_pa)
  ran_eff = ranef((ml3_pa))
  #print(ranef((ml3_pa)))
  
  varP   <- var_ml3_pa[[uid_var]][[1]]  # ID effect (Between subject variation)
  varOxP <- var_ml3_pa[[paste0(time_var,":",uid_var)]][[1]] # Day:ID effect (random intercept for each day-uid combination -> 0 means no meaningful day to day variation)
  varE   <- (attributes(var_ml3_pa)$sc)^2 # Residual Variance
  
  print(paste("Variance P:",varP))
  print(paste("Variance PxO:",varOxP))
  
  k <- length(unique(long[[time_var]])) # maximum number of observations like in mlr; possibly replace with harmonic mean
  m <- length(construct)
  
  print(paste("Measurement Occasions:",k))
  print(paste("Items:",m))
  
  
  rkrn <- varP / (varP + varOxP / k + varE / (m * k))
  rcn  <- varOxP / (varOxP + varE / m)  # Within: Person by occasion/(person by occasion + measurement error), "the within-person reliability indicates how consistent the responses  of the persons are at a given occasion."
  
  
  ml3_rel_pa <-  c(rkrn, rcn)
  
  print(round(ml3_rel_pa,2))
  
  return(ml3_rel_pa)
  
}

GeneralizabilityTheory <- function(data,construct, time_var,uid_var,REML_YN, optimizer = "nloptwrap"){
  

  data[[time_var]] <- ave(seq_along(data[[uid_var]]),data[[uid_var]], FUN = seq_along)
  
  long <-reshape(data[,c(uid_var, time_var, construct)], direction ="long", idvar=c(uid_var, time_var), timevar = "item",varying =list(construct), v.names = "resp")
  #long$resp = long$resp * 10
  
  
  
  
  formula <- as.formula(
    paste0("resp ~ 1 + 
           (1 | ", uid_var, ") + 
           (1 | ", time_var, ") + 
           (1 | item) + 
           (1 | ", uid_var, ":", time_var, ") + # each subject has their own intercept at a specific day
           (1 | ", uid_var, ":item) + 
           (1 | item:", time_var,")")
  ) ### mlr package
  

  # formula <- as.formula(
  #   paste0("resp ~ 1 +
  #          (1 | ", uid_var, ") +
  #          (1 | ", time_var, ") +
  #          (1 | item) +
  #          (1 | ", uid_var, ":", time_var, ") + # each subject has their own intercept at a specific day
  #          (1 | ", uid_var, ":item) +
  #          (1 |",time_var,":item)")
  # )
  
  
  #test1 <- lmer(formula, data = long)
  test1 <- lmer(formula, data = long, REML = REML_YN, control = lmerControl(optimizer=optimizer)) #nloptwrap, #bobyqa, #nlminb
  print(summary(test1))
  #print(ranef((test1)))

  test <- VarCorr(test1)

  varP<-test[[uid_var]][[1]] # ID effect variance
  varI<-test[["item"]][[1]] # item effect variance
  varO<- test[[time_var]][[1]] # day effect variance
  varPxI<-test[[paste0(uid_var,":item")]][[1]]# ID:item effect variance
  varPxO<- test[[paste0(uid_var,":",time_var)]][[1]]# ID:day effect variance
  varIxO<- test[[paste0("item:",time_var)]][[1]]# day:item effect variance
  varE <- (attributes(test)$sc)^2 # residual variance
  # 
  # varP<-test[4,"vcov"]# ID effect variance
  # varI<-test[6,"vcov"]# item effect variance
  # varO<-test[5,"vcov"]# day effect variance
  # varPxI<-test[2,"vcov"]# ID:item effect variance
  # varPxO<-test[1,"vcov"]# ID:day effect variance
  # varIxO<-test[3,"vcov"]# day:item effect variance
  # varE<-test[7,"vcov"]# residual variance
  # 

  
  
  print(isSingular(test1, tol = 1e-4))
  print(VarCorr(test1))

  # Calculate time points and items
  k <- length(unique(long[[time_var]])) # maximum number of observations like in mlr; possibly replace with harmonic mean
  m <- length(construct)
  print(paste("Measurement Occasions:",k))
  print(paste("Items:",m))

  r1f <- (varP + varPxI / m) /
    (varP + varPxI / m + varE / (m))
  rkf <- (varP + varPxI / m) /
    (varP + varPxI / m + varE / (k * m))
  r1r <- (varP + varPxI / m) /
    (varP + varPxI / m + varO + varPxO + varE / m)


 # Rkr <- (MS_id + MS_pxitem/n.items)/((MS_id + MS_pxitem/n.items + MS_time/n.time + MS_pxt/n.time + error/( n.time * n.items)))


  rkr <- (varP + varPxI / m) /
    (varP + varPxI / m + varO / k + varPxO / (k) + varE / (k * m))

  rc <- (varPxO) / (varPxO + varE / m)

  gt_rel_pa <- c(r1f, rkf, r1r,rkr, rc)

  print(gt_rel_pa)
  #print(long$resp[1:10])
  conv <- unlist(test1@optinfo$conv$lme4)

  return(list(
    gt_rel_pa = gt_rel_pa,
    model        = test1,
    converged    = conv,
    is_singular  = isSingular(test1, tol = 1e-4)
  ))
# 
# 
#  vc <- lme4::VarCorr(test1)
#  MS_id <- vc$uid[1,1]
#  MS_time <- vc$month_num[1,1]
#  MS_items <- vc$item[1,1]
#  MS_pxt <- vc[["uid:month_num"]][[1]]
#  MS_pxitem <- vc[["uid:item"]][[1]]
#  MS_txitem <- vc[["item:month_num"]][[1]]
#  error <- MS_resid <- (attributes(vc)$sc)^2
#  s.lmer <- s.aov <- summary(test1)
# 
#  MS.df <- data.frame(variance= c(MS_id, MS_time ,MS_items, MS_pxt, MS_pxitem, MS_txitem, MS_resid,NA))
# 
#  #rownames(MS.df) <- c("ID","Time","Items","ID x time", "ID x items", "time x items", "Residual","Total")
# 
#  n.items = k
#  n.time = m
# 
# 
#  Rkf <- (MS_id + MS_pxitem/n.items)/((MS_id + MS_pxitem/n.items + error/(n.time * n.items)))
#  R1r <- (MS_id + MS_pxitem/n.items)/((MS_id + MS_pxitem/n.items + MS_time + MS_pxt + error/( n.items)))  #per Sean Lane
#  Rkr <- (MS_id + MS_pxitem/n.items)/((MS_id + MS_pxitem/n.items + MS_time/n.time + MS_pxt/n.time + error/( n.time * n.items)))
#  Rc <- (MS_pxt)/(MS_pxt + error/n.items)
# 
#  gt_rel_pa <- c(NA, Rkf, R1r,Rkr, Rc)
#  print(gt_rel_pa)
#   return(gt_rel_pa)
  
}




#Sleep_omega_day <- multilevel.omega(data[, Sleep], cluster = data$uid, missing = "listwise",nrep = 100000, optim.method = c("nlminb")) # No solution (implausible values)

