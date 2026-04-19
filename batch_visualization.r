library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)

input_dir <- "D:/wuyuxin/CMML/res/tmp_modified_new"
output_dir <- file.path(input_dir, "batch_plots")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# data loading
schema_48 <- read.csv(file.path(input_dir, "ac_perf_thres_schema=48.csv"))
schema_45 <- read.csv(file.path(input_dir, "ac_perf_thres_schema=45.csv"))
schema_40 <- read.csv(file.path(input_dir, "ac_perf_thres_schema=40.csv"))
schema_35 <- read.csv(file.path(input_dir, "ac_perf_thres_schema=35.csv"))
schema_30 <- read.csv(file.path(input_dir, "ac_perf_thres_schema=30.csv"))


it_fi_13.35 <- read.csv("D:/wuyuxin/CMML/res/tmp_modified_new/ac_perf_thres_item_final=13.35.csv")
it_fi_13.55 <- read.csv("D:/wuyuxin/CMML/res/tmp_modified_new/ac_perf_thres_item_final=13.55.csv")
it_fi_13.65 <- read.csv("D:/wuyuxin/CMML/res/tmp_modified_new/ac_perf_thres_item_final=13.65.csv")
it_fi_12 <- read.csv("D:/wuyuxin/CMML/res/tmp_modified_new/ac_perf_thres_item_final=12.csv")
it_fi_10 <- read.csv("D:/wuyuxin/CMML/res/tmp_modified_new/ac_perf_thres_item_final=10.csv")

it_in_3 <- read.csv("D:/wuyuxin/CMML/res/tmp_modified_new/ac_perf_thres_item_inter=3.csv")
it_in_4 <- read.csv("D:/wuyuxin/CMML/res/tmp_modified_new/ac_perf_thres_item_inter=4.csv")
it_in_5 <- read.csv("D:/wuyuxin/CMML/res/tmp_modified_new/ac_perf_thres_item_inter=5.csv")
it_in_5.5 <- read.csv("D:/wuyuxin/CMML/res/tmp_modified_new/ac_perf_thres_item_inter=5.5.csv")

w_0.4 <- read.csv("D:/wuyuxin/CMML/res/tmp_modified_new/ac_perf_w=0.4.csv")
w_0.5 <- read.csv("D:/wuyuxin/CMML/res/tmp_modified_new/ac_perf_w=0.5.csv")
w_0.7 <- read.csv("D:/wuyuxin/CMML/res/tmp_modified_new/ac_perf_w=0.7.csv")

Phi_25 <- read.csv("D:/wuyuxin/CMML/res/tmp_modified_new/ac_perf_Phi=25.csv")
Phi_30 <- read.csv("D:/wuyuxin/CMML/res/tmp_modified_new/ac_perf_Phi=30.csv")
Phi_35 <- read.csv("D:/wuyuxin/CMML/res/tmp_modified_new/ac_perf_Phi=35.csv")

combination <- read.csv("D:/wuyuxin/CMML/res/tmp_modified_new/ac_perf_thres_item_final=13.65_thres_schema=35_w=0.5_Phi=25.csv")

control <- read.csv("D:/wuyuxin/CMML/res/tmp/whole/L/allresult_processed.csv", header = TRUE)

control <- control %>%
  group_by(Subject) %>%
  summarise(
    mean_AC = mean(AC, na.rm = TRUE),
    total_perf = sum(performance),
    mean_RT = mean(total_RT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    param = "control"
  )


sum_data <- bind_rows(
  # schema_48 %>% mutate(param = "thres_schema=48"),
  # schema_45 %>% mutate(param = "thres_schema=45"),
  # schema_40 %>% mutate(param = "thres_schema=40"),
  # schema_35 %>% mutate(param = "thres_schema=35"),
  # schema_30 %>% mutate(param = "thres_schema=30")
  
  # it_in_3 %>% mutate(param = "thres_item_inter=3"),
  # it_in_4 %>% mutate(param = "thres_item_inter=4"),
  # it_in_5 %>% mutate(param = "thres_item_inter=5"),
  # it_in_5.5 %>% mutate(param = "thres_item_inter=5.5")
  # 
  # it_fi_10 %>% mutate(param = "thres_item_final=10"),
  # it_fi_12 %>% mutate(param = "thres_item_final=12"),
  # it_fi_13.35 %>% mutate(param = "thres_item_final=13.35"),
  # it_fi_13.55 %>% mutate(param = "thres_item_final=13.55"),
  # it_fi_13.65 %>% mutate(param = "thres_item_final=13.65")
  # 
  # w_0.4 %>% mutate(param = "w=0.4"),
  # w_0.5 %>% mutate(param = "w=0.5"),
  # w_0.7 %>% mutate(param = "w=0.7")
  Phi_25 %>% mutate(param = "Phi=25"),
  Phi_30 %>% mutate(param = "Phi=30"),
  Phi_35 %>% mutate(param = "Phi=35")
)

sum_data <- sum_data %>%
  group_by(param, Subject) %>%
  summarise(
    mean_AC = mean(AC, na.rm = TRUE),
    total_perf = sum(performance),
    mean_RT = mean(total_RT, na.rm = TRUE),
    .groups = "drop"
  )

plot_data <- bind_rows(
  sum_data,control
)

plot_data <- plot_data %>%
  mutate(
    param = factor(param, levels = c(
      "control",
      
      # "thres_schema=48",
      # "thres_schema=45",
      # "thres_schema=40",
      # "thres_schema=35",
      # "thres_schema=30"
      
      # "thres_item_final=13.65",
      # "thres_item_final=13.55",
      # "thres_item_final=13.35",
      # "thres_item_final=12",
      # "thres_item_final=10"
      # 
      # "w=0.4",
      # "w=0.5",
      # "w=0.7"

      
      # "thres_item_inter=5.5",
      # "thres_item_inter=5",
      # "thres_item_inter=4",
      # "thres_item_inter=3"
      
      "Phi=25",
      "Phi=30",
      "Phi=35"
    ))
  )

# compute significance annotations with FDR correction
compute_signif_annotations <- function(data, metric) {
  control_values <- data %>% filter(param == "control") %>% pull({{ metric }})
  exp_params <- data %>% filter(param != "control") %>% distinct(param) %>% pull(param)
  metric_values <- data %>% select(param, value = {{ metric }})

  results <- lapply(exp_params, function(p) {
    group_values <- metric_values %>% filter(param == p) %>% pull(value)
    if (length(group_values) < 2 || length(control_values) < 2) {
      return(NULL)
    }
    test <- t.test(group_values, control_values, var.equal = FALSE)
    test$adjusted_p <- p.adjust(test$p.value, method = "fdr", n = length(exp_params))
    signif <- if (test$adjusted_p < 0.001) "***" else if (test$adjusted_p < 0.01) "**" else if (test$adjusted_p < 0.05) "*" else NA_character_
    tibble(
      param = p,
      p_value = test$adjusted_p,
      signif = signif
    )
  })

  results <- bind_rows(results)
  if (nrow(results) == 0) {
    return(tibble())
  }

  metric_max <- max(metric_values$value, na.rm = TRUE)
  metric_range <- diff(range(metric_values$value, na.rm = TRUE))
  if (metric_range == 0) {
    metric_range <- abs(metric_max) * 0.05
  }

  results %>%
    filter(!is.na(signif)) %>%
    mutate(
      x_start = 1,
      x_end = as.numeric(param),
      y = metric_max + seq_len(n()) * metric_range * 0.08,
      y_text = y + metric_range * 0.03,
      label = signif
    )
}

signif_ac <- compute_signif_annotations(plot_data, mean_AC)
signif_perf <- compute_signif_annotations(plot_data, total_perf)
signif_rt <- compute_signif_annotations(plot_data, mean_RT)



colors <- c("#627D8A","#7fcdbb", "#41b6c4", "#2c7fb8", "#74add1"#, "#3690c0"
            )

p_box_ac <- ggplot(plot_data, aes(x = param, y = mean_AC, fill = param)) +
  geom_boxplot(width = 0.35, outlier.size = 1, fatten = 1) +
  coord_cartesian(ylim = c(NA, max(plot_data$mean_AC, na.rm = TRUE) * 1.15)) +
  labs(
    title = "Mean Accuracy by Parameter",
    x = NULL,
    y = "Mean AC"
  ) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "#dfe3e6"),
    panel.grid.minor = element_line(color = "#f3f5f6"),
    axis.line = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 11),
    plot.title = element_text(hjust = 0.5)
  ) +
  {
    if (nrow(signif_ac) > 0) {
      list(
        geom_segment(data = signif_ac, aes(x = x_start, xend = x_end, y = y, yend = y), color = "black", size = 0.7, inherit.aes = FALSE),
        geom_text(data = signif_ac, aes(x = (x_start + x_end) / 2, y = y_text, label = label), color = "black", inherit.aes = FALSE, size = 5, vjust = -0.5)
      )
    }
  }

p_box_perf <- ggplot(plot_data, aes(x = param, y = total_perf, fill = param)) +
  geom_boxplot(width = 0.35, outlier.size = 1, fatten = 1) +
  coord_cartesian(ylim = c(NA, max(plot_data$total_perf, na.rm = TRUE) * 1.15)) +
  labs(
    title = "Total Performance by Parameter",
    x = NULL,
    y = "Total Performance"
  ) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "#dfe3e6"),
    panel.grid.minor = element_line(color = "#f3f5f6"),
    axis.line = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 11),
    plot.title = element_text(hjust = 0.5)
  ) +
  {
    if (nrow(signif_perf) > 0) {
      list(
        geom_segment(data = signif_perf, aes(x = x_start, xend = x_end, y = y, yend = y), color = "black", size = 0.7, inherit.aes = FALSE),
        geom_text(data = signif_perf, aes(x = (x_start + x_end) / 2, y = y_text, label = label), color = "black", inherit.aes = FALSE, size = 5, vjust = -0.5)
      )
    }
  }

p_box_rt <- ggplot(plot_data, aes(x = param, y = mean_RT, fill = param)) +
  geom_boxplot(width = 0.35, outlier.size = 1, fatten = 1) +
  coord_cartesian(ylim = c(NA, max(plot_data$mean_RT, na.rm = TRUE) * 1.15)) +
  labs(
    title = "Mean Reaction Time by Parameter",
    x = "Parameter",
    y = "Mean RT"
  ) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "#dfe3e6"),
    panel.grid.minor = element_line(color = "#f3f5f6"),
    axis.line = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    plot.title = element_text(hjust = 0.5)
  ) +
  {
    if (nrow(signif_rt) > 0) {
      list(
        geom_segment(data = signif_rt, aes(x = x_start, xend = x_end, y = y, yend = y), color = "black", size = 0.7, inherit.aes = FALSE),
        geom_text(data = signif_rt, aes(x = (x_start + x_end) / 2, y = y_text, label = label), color = "black", inherit.aes = FALSE, size = 5, vjust = -0.5)
      )
    }
  }

combined_boxplots <- grid.arrange(p_box_ac, p_box_perf, p_box_rt, ncol = 1)
ggsave(file.path(output_dir, "batch_boxplots_Phi.png"), combined_boxplots, width = 10, height = 14)

message("Batch plots saved to: ", output_dir)