# Load required packages
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)

# Batch read CSV data and generate plots for each file
input_dir <- "D:/wuyuxin/CMML/res/tmp_modified_new"
output_root <- file.path(input_dir, "plots")

control <- read.csv("D:/wuyuxin/CMML/res/tmp/whole/L/allresult_processed.csv", header = TRUE)
#control$Phase <- as.numeric(control$Phase)
#control$total_RT <- control$RT_1+control$RT_2+control$RT_3+control$RT_4

csv_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(csv_files) == 0) {
  stop("No CSV files found in ", input_dir)
}

colors <- c("#627D8A","#41b6c4")

for (input_path in csv_files) {
  file_base <- tools::file_path_sans_ext(basename(input_path))
  output_dir <- file.path(output_root, file_base)

  if (dir.exists(output_dir)) {
    message("Skipping existing output for: ", basename(input_path), " (", output_dir, ")")
    next
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  message("Processing: ", basename(input_path), " -> ", output_dir)

  data <- read.csv(input_path, header = TRUE)
  data$Phase <- as.numeric(data$Phase)

  # # Group by Phase and calculate mean and standard error for AC and performance (optional)
  # summary_control <- control %>%
  #   group_by(Phase) %>%
  #   summarise(
  #     mean_AC = mean(AC, na.rm = TRUE),
  #     se_AC = sd(AC, na.rm = TRUE) / sqrt(n()),
  #     mean_performance = mean(performance, na.rm = TRUE),
  #     se_performance = sd(performance, na.rm = TRUE) / sqrt(n()),
  #     total_AC = sum(AC),
  #     total_perf = sum(performance)
  #   )

  # summary_data <- data %>%
  #   group_by(Phase) %>%
  #   summarise(
  #     mean_AC = mean(AC, na.rm = TRUE),
  #     se_AC = sd(AC, na.rm = TRUE) / sqrt(n()),
  #     mean_performance = mean(performance, na.rm = TRUE),
  #     se_performance = sd(performance, na.rm = TRUE) / sqrt(n()),
  #     total_AC = sum(AC),
  #     total_perf = sum(performance)
  #   )

  # # Combine data for average plots
  # combined_ac <- bind_rows(
  #   summary_data %>% mutate(group = "data"),
  #   summary_control %>% mutate(group = "control")
  # )

  # combined_perf <- bind_rows(
  #   summary_data %>% mutate(group = "data"),
  #   summary_control %>% mutate(group = "control")
  # )

  # # Draw combined average AC plot
  # p_ac_ave_combined <- ggplot(combined_ac, aes(x = Phase, y = mean_AC, color = group)) +
  #   geom_line(size = 1) +
  #   geom_point(size = 2) +
  #   scale_fill_manual(values = colors) +
  #   geom_ribbon(aes(ymin = mean_AC - se_AC, ymax = mean_AC + se_AC, fill = group), alpha = 0.2) +
  #   labs(title = "Average AC across Phases",
  #        x = "Phase",
  #        y = "Mean AC",
  #        color = "Group",
  #        fill = "Group") +
  #   theme_minimal()

  # # Draw combined average performance plot
  # p_perf_ave_combined <- ggplot(combined_perf, aes(x = Phase, y = mean_performance, color = group)) +
  #   geom_line(size = 1) +
  #   geom_point(size = 2) +
  #   scale_fill_manual(values = colors) +
  #   geom_ribbon(aes(ymin = mean_performance - se_performance, ymax = mean_performance + se_performance, fill = group), alpha = 0.2) +
  #   labs(title = "Average Performance across Phases",
  #        x = "Phase",
  #        y = "Mean Performance",
  #        color = "Group",
  #        fill = "Group") +
  #   theme_minimal()

  # print(p_ac_ave_combined)
  # print(p_perf_ave_combined)

  # # Draw p_ac and p_ac_ctrl
  # p_ac <- ggplot(data, aes(x = Phase, y = AC, color = as.factor(Subject))) +
  #   geom_line() +
  #   labs(title = "AC across Phases by Subject (Data)",
  #        x = "Phase",
  #        y = "AC",
  #        color = "Subject") +
  #   theme_minimal() +
  #   theme(legend.position = "right")

  # p_ac_ctrl <- ggplot(control, aes(x = Phase, y = AC, color = as.factor(Subject))) +
  #   geom_line() +
  #   labs(title = "AC across Phases by Subject (Control)",
  #        x = "Phase",
  #        y = "AC",
  #        color = "Subject") +
  #   theme_minimal() +
  #   theme(legend.position = "right")

  # # Arrange p_ac and p_ac_ctrl vertically
  # p_ac_subject <- grid.arrange(p_ac, p_ac_ctrl, nrow = 2)
  # print(p_ac_subject)

  # # Draw p_perf and p_perf_ctrl
  # p_perf <- ggplot(data, aes(x = Phase, y = performance, color = as.factor(Subject))) +
  #   geom_line() +
  #   labs(title = "Performance across Phases by Subject (Data)",
  #        x = "Phase",
  #        y = "Performance",
  #        color = "Subject") +
  #   theme_minimal() +
  #   theme(legend.position = "right")

  # p_perf_ctrl <- ggplot(control, aes(x = Phase, y = performance, color = as.factor(Subject))) +
  #   geom_line() +
  #   labs(title = "Performance across Phases by Subject (Control)",
  #        x = "Phase",
  #        y = "Performance",
  #        color = "Subject") +
  #   theme_minimal() +
  #   theme(legend.position = "right")

  # # Arrange p_perf and p_perf_ctrl vertically
  # p_perf_subject <- grid.arrange(p_perf, p_perf_ctrl, nrow = 2)
  # print(p_perf_subject)

  # Save plots to the output directory
  # ggsave(file.path(output_dir, "average_AC.png"), p_ac_ave_combined, width = 10, height = 6)
  # ggsave(file.path(output_dir, "average_Performance.png"), p_perf_ave_combined, width = 10, height = 6)
  # ggsave(file.path(output_dir, "AC_by_subject.png"), p_ac_subject, width = 10, height = 12)
  # ggsave(file.path(output_dir, "performance_by_subject.png"), p_perf_subject, width = 10, height = 12)

  # Summarize data by subject and perform statistical tests
  # Calculate total and mean values for each subject in the control group
  control_subject <- control %>%
    group_by(Subject) %>%
    summarise(
      total_performance = sum(performance, na.rm = TRUE),
      total_AC = sum(AC, na.rm = TRUE),
      mean_performance = mean(performance, na.rm = TRUE),
      mean_AC = mean(AC, na.rm = TRUE),
      mean_RT = mean(total_RT, na.rm=TRUE),
      .groups = "drop"
    ) %>%
    mutate(group = "control")

  # Calculate total and mean values for each subject in the experimental group
  data_subject <- data %>%
    group_by(Subject) %>%
    summarise(
      total_performance = sum(performance, na.rm = TRUE),
      total_AC = sum(AC, na.rm = TRUE),
      mean_performance = mean(performance, na.rm = TRUE),
      mean_AC = mean(AC, na.rm = TRUE),
      mean_RT = mean(total_RT, na.rm=TRUE),
      .groups = "drop"
    ) %>%
    mutate(group = "data")

  # Combine both groups
  all_subject <- bind_rows(control_subject, data_subject)

  t_test_res <- t.test(total_performance ~ group, data = all_subject, var.equal = FALSE)
  #print(t_test_res)

  # Optional: also test total_AC and mean_performance
  t_test_mean_ac <- t.test(mean_AC ~ group, data = all_subject, var.equal = FALSE)
  t_test_mean_perf <- t.test(mean_performance ~ group, data = all_subject, var.equal = FALSE)
  t_test_mean_rt <- t.test(mean_RT ~ group, data = all_subject, var.equal = FALSE)

  #print(t_test_ac)

  t_test_summary <- tibble(
    metric = c("total_performance", "mean_AC", "mean_performance", "mean_RT"),
    statistic = c(as.numeric(t_test_res$statistic), as.numeric(t_test_mean_ac$statistic), as.numeric(t_test_mean_perf$statistic), as.numeric(t_test_mean_rt$statistic)),
    df = c(as.numeric(t_test_res$parameter), as.numeric(t_test_mean_ac$parameter), as.numeric(t_test_mean_perf$parameter), as.numeric(t_test_mean_rt$parameter)),
    p_value = c(t_test_res$p.value, t_test_mean_ac$p.value, t_test_mean_perf$p.value, t_test_mean_rt$p.value),
    estimate_data = c(as.numeric(t_test_res$estimate[1]), as.numeric(t_test_mean_ac$estimate[1]), as.numeric(t_test_mean_perf$estimate[1]), as.numeric(t_test_mean_rt$estimate[1])),
    estimate_control = c(as.numeric(t_test_res$estimate[2]), as.numeric(t_test_mean_ac$estimate[2]), as.numeric(t_test_mean_perf$estimate[2]), as.numeric(t_test_mean_rt$estimate[2])),
    method = c(t_test_res$method, t_test_mean_ac$method, t_test_mean_perf$method, t_test_mean_rt$method)
  )

  get_signif_label <- function(p) {
    if (is.na(p) || p >= 0.05) {
      return(NA_character_)
    }
    if (p < 0.001) {
      return("***")
    }
    if (p < 0.01) {
      return("**")
    }
    return("*")
  }

  make_signif_annotation <- function(values, p_value) {
    label <- get_signif_label(p_value)
    if (is.na(label)) {
      return(tibble())
    }
    y_max <- max(values, na.rm = TRUE)
    y_min <- min(values, na.rm = TRUE)
    y_span <- max(y_max - y_min, abs(y_max) * 0.05, 0.1)
    y_line <- y_max + y_span * 0.08
    y_text <- y_line + y_span * 0.04
    tibble(
      x_start = 1,
      x_end = 2,
      y = y_line,
      y_text = y_text,
      label = label
    )
  }

  signif_total_perf <- make_signif_annotation(all_subject$total_performance, t_test_res$p.value)
  signif_mean_ac <- make_signif_annotation(all_subject$mean_AC, t_test_mean_ac$p.value)
  signif_mean_rt <- make_signif_annotation(all_subject$mean_RT, t_test_mean_rt$p.value)

  write.csv(t_test_summary, file.path(output_dir, "t_test_summary.csv"), row.names = FALSE)

  # # Output results table
  # cat("\n=== t-test results ===\n")
  # cat("Total Performance: t =", round(t_test_res$statistic, 3), 
  #     ", df =", round(t_test_res$parameter, 2), 
  #     ", p =", format(t_test_res$p.value, scientific = TRUE, digits = 3), "\n")
  # cat("Total AC: t =", round(t_test_ac$statistic, 3), 
  #     ", p =", format(t_test_ac$p.value, scientific = TRUE, digits = 3), "\n")
  # cat("Mean Performance: t =", round(t_test_mean_perf$statistic, 3), 
  #     ", p =", format(t_test_mean_perf$p.value, scientific = TRUE, digits = 3), "\n")


  p_box_total <- ggplot(all_subject, aes(x = group, y = total_performance, fill = group)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.6) +
    scale_fill_manual(values = colors) +
    labs(title = "Total Performance by Group",
         x = "Group", y = "Total Performance") +
    theme_minimal() +
    theme(legend.position = "none") +
    {
      if (nrow(signif_total_perf) > 0) {
        list(
          geom_segment(data = signif_total_perf, aes(x = x_start, xend = x_end, y = y, yend = y), color = "black", size = 0.8, inherit.aes = FALSE),
          geom_text(data = signif_total_perf, aes(x = (x_start + x_end) / 2, y = y_text, label = label), color = "black", inherit.aes = FALSE, size = 5)
        )
      }
    }

  #print(p_box_total)

  p_box_ac <- ggplot(all_subject, aes(x = group, y = mean_AC, fill = group)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.6) +
    scale_fill_manual(values = colors) +
    labs(title = "Mean Accuracy by Group",
         x = "Group", y = "Mean AC") +
    theme_minimal() +
    theme(legend.position = "none") +
    {
      if (nrow(signif_mean_ac) > 0) {
        list(
          geom_segment(data = signif_mean_ac, aes(x = x_start, xend = x_end, y = y, yend = y), color = "black", size = 0.8, inherit.aes = FALSE),
          geom_text(data = signif_mean_ac, aes(x = (x_start + x_end) / 2, y = y_text, label = label), color = "black", inherit.aes = FALSE, size = 5)
        )
      }
    }

  #print(p_box_ac)
  
  p_box_RT <- ggplot(all_subject, aes(x = group, y = mean_RT, fill = group)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.6) +
    scale_fill_manual(values = colors) +
    labs(title = "Mean Reaction Time by Group",
         x = "Group", y = "Mean Reaction Time (s)") +
    theme_minimal() +
    theme(legend.position = "none") +
    {
      if (nrow(signif_mean_rt) > 0) {
        list(
          geom_segment(data = signif_mean_rt, aes(x = x_start, xend = x_end, y = y, yend = y), color = "black", size = 0.8, inherit.aes = FALSE),
          geom_text(data = signif_mean_rt, aes(x = (x_start + x_end) / 2, y = y_text, label = label), color = "black", inherit.aes = FALSE, size = 5)
        )
      }
    }

  ggsave(file.path(output_dir, "boxplot_total_performance.png"), p_box_total, width = 6, height = 4)
  ggsave(file.path(output_dir, "boxplot_mean_AC.png"), p_box_ac, width = 6, height = 4)
  ggsave(file.path(output_dir, "boxplot_mean_RT.png"), p_box_RT, width = 6, height = 4)
  
}