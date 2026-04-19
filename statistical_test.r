library(dplyr)

control_path <- "D:/wuyuxin/CMML/res/tmp/whole/L/allresult_processed.csv"
input_dir <- "D:/wuyuxin/CMML/res/tmp_modified_new"
output_dir <- "D:/wuyuxin/CMML/res/tmp_modified_new/statistical_test_results"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

control_raw <- read.csv(control_path, header = TRUE, stringsAsFactors = FALSE)
control_summary <- control_raw %>%
  group_by(Subject) %>%
  summarise(
    total_performance = sum(performance, na.rm = TRUE),
    mean_RT = mean(total_RT, na.rm = TRUE),
    mean_AC = mean(AC, na.rm = TRUE),
    .groups = "drop"
  )

exp_files <- list.files(input_dir, pattern = "^ac_perf_.*\\.csv$", full.names = TRUE)
if (length(exp_files) == 0) {
  stop("No experiment CSV files found in ", input_dir)
}

parse_param_value <- function(filepath) {
  base <- basename(filepath)
  base <- sub("\\.csv$", "", base)
  base <- sub("^ac_perf_", "", base)
  if (grepl("=", base)) {
    param <- sub("=.*$", "", base)
    value <- sub("^[^=]*=", "", base)
  } else {
    param <- "unknown"
    value <- base
  }
  list(param = param, value = value)
}

run_ttest <- function(control_values, exp_values) {
  test <- t.test(exp_values, control_values, var.equal = FALSE)
  pooled_sd <- sqrt(((length(exp_values) - 1) * var(exp_values, na.rm = TRUE) +
                      (length(control_values) - 1) * var(control_values, na.rm = TRUE)) /
                     (length(exp_values) + length(control_values) - 2))
  cohen_d <- (mean(exp_values, na.rm = TRUE) - mean(control_values, na.rm = TRUE)) / pooled_sd
  list(
    p_value = test$p.value,
    cohen_d = cohen_d
  )
}

results_perf <- tibble(
  Parameter = character(),
  Value = character(),
  `Total performance` = numeric(),
  `P value (raw)` = numeric(),
  `Cohen's d` = numeric()
)
results_rt <- tibble(
  Parameter = character(),
  Value = character(),
  `Mean RT` = numeric(),
  `P value (raw)` = numeric(),
  `Cohen's d` = numeric()
)
results_ac <- tibble(
  Parameter = character(),
  Value = character(),
  `Mean AC` = numeric(),
  `P value (raw)` = numeric(),
  `Cohen's d` = numeric()
)

for (exp_file in exp_files) {
  info <- parse_param_value(exp_file)
  exp_raw <- read.csv(exp_file, header = TRUE, stringsAsFactors = FALSE)

  exp_summary <- exp_raw %>%
    group_by(Subject) %>%
    summarise(
      total_performance = sum(performance, na.rm = TRUE),
      mean_RT = mean(total_RT, na.rm = TRUE),
      mean_AC = mean(AC, na.rm = TRUE),
      .groups = "drop"
    )

  perf_test <- run_ttest(control_summary$total_performance, exp_summary$total_performance)
  rt_test <- run_ttest(control_summary$mean_RT, exp_summary$mean_RT)
  ac_test <- run_ttest(control_summary$mean_AC, exp_summary$mean_AC)

  results_perf <- bind_rows(results_perf, tibble(
    Parameter = info$param,
    Value = info$value,
    `Total performance` = mean(exp_summary$total_performance, na.rm = TRUE),
    `P value (raw)` = perf_test$p_value,
    `Cohen's d` = perf_test$cohen_d
  ))

  results_rt <- bind_rows(results_rt, tibble(
    Parameter = info$param,
    Value = info$value,
    `Mean RT` = mean(exp_summary$mean_RT, na.rm = TRUE),
    `P value (raw)` = rt_test$p_value,
    `Cohen's d` = rt_test$cohen_d
  ))

  results_ac <- bind_rows(results_ac, tibble(
    Parameter = info$param,
    Value = info$value,
    `Mean AC` = mean(exp_summary$mean_AC, na.rm = TRUE),
    `P value (raw)` = ac_test$p_value,
    `Cohen's d` = ac_test$cohen_d
  ))
}



# multiple comparison using FDR adjusting
apply_fdr <- function(df, p_col = "P value (raw)") {
  raw_p <- df[[p_col]]
  adj_p <- p.adjust(raw_p, method = "fdr") 
  df[[paste0("P value (FDR)")]] <- adj_p
  df$Significance <- ifelse(adj_p < 0.05, 
                            ifelse(adj_p < 0.01, 
                                   ifelse(adj_p < 0.001, "***", "**"), "*"), 
                            "n.s.")
  return(df)
}

results_perf <- apply_fdr(results_perf, "P value (raw)")
results_rt   <- apply_fdr(results_rt,   "P value (raw)")
results_ac   <- apply_fdr(results_ac,   "P value (raw)")

format_p <- function(p) {
  ifelse(p < 0.001, "<0.001", sprintf("%.3f", round(p, 3)))
}

results_perf <- results_perf %>%
  mutate(
    `P value (raw)` = sapply(`P value (raw)`, format_p),
    `P value (FDR)` = sapply(`P value (FDR)`, format_p),
    across(where(is.numeric) & !contains("P value"), ~ round(.x, 3))
  ) %>%
  arrange(Parameter, Value)

results_rt <- results_rt %>%
  mutate(
    `P value (raw)` = sapply(`P value (raw)`, format_p),
    `P value (FDR)` = sapply(`P value (FDR)`, format_p),
    across(where(is.numeric) & !contains("P value"), ~ round(.x, 3))
  ) %>%
  arrange(Parameter, Value)

results_ac <- results_ac %>%
  mutate(
    `P value (raw)` = sapply(`P value (raw)`, format_p),
    `P value (FDR)` = sapply(`P value (FDR)`, format_p),
    across(where(is.numeric) & !contains("P value"), ~ round(.x, 3))
  ) %>%
  arrange(Parameter, Value)



write.csv(results_perf, file.path(output_dir, "stat_test_total_performance.csv"), row.names = FALSE)
write.csv(results_rt, file.path(output_dir, "stat_test_mean_RT.csv"), row.names = FALSE)
write.csv(results_ac, file.path(output_dir, "stat_test_mean_AC.csv"), row.names = FALSE)

cat("Wrote results to:\n")
cat(" - ", file.path(output_dir, "stat_test_total_performance.csv"), "\n")
cat(" - ", file.path(output_dir, "stat_test_mean_RT.csv"), "\n")
cat(" - ", file.path(output_dir, "stat_test_mean_AC.csv"), "\n")
