############################################################
# ASRT Analysis Functions - Rewritten & Modular
# Preserves original logic, adds robustness and auto-detection
############################################################

# ---- Dependencies ----
require(dplyr)
require(rio)
require(purrr)
require(tidyr)
require(ggplot2)
require(trimr)
require(data.table)
require(stringr)
require(forcats)
require(DMwR)
require(rstatix)

############################################################
# 1. File I/O Utilities
############################################################

# List all participant files for a given task type
list_file <- function(tasktype) {
  ddir <- file.path('..', 'data', tasktype)
  fns <- list.files(ddir, pattern = '*.csv', full.names = TRUE)
  sids <- sub("_.*", "", basename(fns))
  tibble(file = fns, sid = sids)
}


# Match files with prefix (subject ID)
match_prefix <- function(prefix, file_list) {
  pattern <- paste0("^", prefix, "_")
  file_list[grep(pattern, file_list)]
}

# Select file for a specific subject ID
select_file <- function(sidstr, finfo) {
  fn <- match_prefix(sidstr, finfo$file)
  file.path(finfo$dir, fn)
}

# Import and minimally preprocess a file
import_d <- function(fpath, tasktype, sidstr) {
  d <- import(fpath)
  d$tasktype <- tasktype
  d$participant <- sidstr
  setDT(d)
  
  # Set mental target orientation
  d[, target_orientation1 := ifelse(tasktype == 'motor',
                                    correct_answer_index,
                                    orientation_index)]
  d[, target_orientation2 := ifelse(tasktype == 'motor',
                                    correct_answer_index + 1,
                                    orientation_index + 1)]
  
  # Build triplets (lag 2, lag 1, current)
  d[, triplet := paste(
    shift(target_orientation1, n = 2, type = "lag", fill = NA),
    shift(target_orientation1, n = 1, type = "lag", fill = NA),
    target_orientation1,
    sep = ""
  )]
  
  d[, triplet2 := paste(
    shift(target_orientation2, n = 2, type = "lag", fill = NA),
    shift(target_orientation2, n = 1, type = "lag", fill = NA),
    target_orientation2,
    sep = "-"
  )]
  
  setDF(d)
  return(d)
}

############################################################
# 2. Preprocessing: Add condition columns and trial metadata
############################################################

add_condition_cols <- function(d, tasktype) {
  # Determine testing variable name dynamically
  testvar <- paste0(tasktype, '_testing_seq_files')
  
  d2 <- d %>%
    filter(!is.na(orientation_index)) %>%
    mutate(
      block_type = case_when(
        nzchar(initial_random_seq_files) ~ "random",
        nzchar(practice_seq_files) ~ "practice",
        nzchar(learning_seq_files) ~ "learning",
        nzchar(sym(testvar)) ~ "testing",
        TRUE ~ "unknown"
      ),
      blockID = case_when(
        block_type == 'practice' ~ coalesce(!!!select(., matches("practice_blocks.thisTrialN"))),
        block_type == 'random' ~ 0,
        TRUE ~ coalesce(!!!select(., ends_with("trials.thisTrialN")))
      ),
      blockID2 = case_when(
        block_type == 'random' ~ 'R',
        block_type == 'practice' ~ sprintf('P%.2d', blockID + 1),
        block_type == 'learning' ~ sprintf('L%.2d', blockID + 1),
        block_type == 'testing' ~ sprintf('T%.2d', blockID + 1)
      ),
      trial = case_when(
        block_type == 'practice' ~ coalesce(!!!select(., matches("practice_trials.thisTrialN"))),
        block_type == 'random' ~ coalesce(!!!select(., matches("init_random_trials.thisTrialN"))),
        TRUE ~ coalesce(!!!select(., ends_with("loop.thisTrialN")))
      ),
      # Auto-detect RT column
      rt = if ("key_resp.rt" %in% names(.)) key_resp.rt * 1000 else if ("rt" %in% names(.)) rt else NA,
      accuracy = if ("key_resp.corr" %in% names(.)) key_resp.corr else if ("accuracy" %in% names(.)) accuracy else NA,
      epoch = case_when(
        block_type == 'learning' ~ as.character((blockID %/% 5) + 1),
        block_type == 'random' ~ 'R',
        block_type == 'practice' ~ 'P',
        block_type == 'testing' ~ 'T'
      )
    ) %>%
    select(participant, block_type, blockID, blockID2, epoch,
           orientation_degrees, orientation_index,
           target_orientation1, target_orientation2,
           correct_answer_direction, accuracy,
           trial, rt, triplet, triplet2,
           starts_with('motor_testing')) %>%
    group_by(block_type, blockID) %>%
    mutate(trial_rank = rank(trial) - 6)  # rank within block
  
  setDT(d2)
  
  # Token alternation (for triplet_type)
  d2[, target_token := ifelse(trial_rank %% 2 == 0, target_orientation2, 'r')]
  d2[, triplet_type := paste(
    shift(target_token, n = 2, type = "lag", fill = NA),
    shift(target_token, n = 1, type = "lag", fill = NA),
    target_token,
    sep = "-"
  )]
  
  setDF(d2)
  
  # Filter initial trials and assign trial_type
  d3 <- d2 %>%
    filter(trial_rank > 1) %>%
    mutate(trial_type = if_else(trial_rank %% 2 == 0, "regular", "random"),
           trial_type = case_when(
             block_type %in% c("random", "practice") ~ "random",
             TRUE ~ trial_type
           ))
  
  return(d3)
}

############################################################
# 3. Frequency Analysis
############################################################

# Compute triplet frequencies and categorize high/low
compute_frequency <- function(d) {
  triplet_frequency <- d %>%
    filter(!block_type %in% c("practice", "random")) %>%
    group_by(trial_type, triplet_type, triplet2) %>%
    summarise(trialtype_triplet_num_total = n(), .groups = 'drop')
  
  # Threshold = median of random triplet counts
  random_threshold <- triplet_frequency %>%
    filter(trial_type == 'random') %>%
    summarise(med = median(trialtype_triplet_num_total)) %>%
    pull(med)
  
  triplet_frequency <- triplet_frequency %>%
    mutate(frequency_type = ifelse(trialtype_triplet_num_total >= random_threshold, 'high', 'low'))
  
  triplet_frequency_byepoch <- d %>%
    filter(!block_type %in% c("practice", "random")) %>%
    group_by(epoch, trial_type, triplet_type, triplet2) %>%
    summarise(trialtype_triplet_num_epoch = n(), .groups = 'drop') %>%
    left_join(triplet_frequency %>%
                select(trial_type, triplet2, frequency_type, trialtype_triplet_num_total),
              by = c("trial_type", "triplet2"))
  
  # Add overall counts per frequency type
  triplet_frequency_byepoch <- triplet_frequency_byepoch %>%
    group_by(epoch, trial_type, triplet_type, frequency_type) %>%
    mutate(trialtype_triplettype_frequency_num_epoch = sum(trialtype_triplet_num_epoch)) %>%
    group_by(epoch, trial_type, frequency_type) %>%
    mutate(trialtype_frequencytype_num_epoch = sum(trialtype_triplet_num_epoch)) %>%
    ungroup() %>%
    mutate(trialtype_triplet_num_total_threshold = random_threshold)
  
  return(triplet_frequency_byepoch)
}

# Assign frequency category to trials
assign_freq <- function(d3) {
  d4 <- d3 %>%
    ungroup() %>%
    select(any_of(c(
      "participant", "accuracy", "rt",
      "block_type", "blockID", "blockID2", "epoch",
      "trial_type", "triplet", "triplet2", "trial_rank",
      "triplet_type", "target_token",
      "target_orientation1", "target_orientation2"
    )))
  
  triplet_frequency_byepoch <- compute_frequency(d4)
  
  d5 <- d4 %>%
    left_join(triplet_frequency_byepoch,
              by = c("epoch", "trial_type", "triplet_type", "triplet2")) %>%
    mutate(unitx1 = epoch, unitx2 = blockID2)
  
  return(d5)
}

############################################################
# 4. Cleaning
############################################################

clean_data <- function(d) {
  d %>%
    drop_na() %>%
    filter(rt < 1000, rt > 100, accuracy == 1) %>%
    filter(
      !str_detect(triplet, pattern = "(\\d)\\1\\1"),  # no triple repeats
      !str_detect(triplet, pattern = "(\\d)(\\d)\\1") # no trills
    )
}

############################################################
# 5. Summarization for Plotting
############################################################

create_summ4plot <- function(d5, unitx) {
  # Assign x-axis unit
  d5$unitx <- if (unitx == 'epoch') d5$unitx1 else d5$unitx2
  
  d5_summary <- d5 %>%
    mutate(block_type = forcats::fct_relevel(
      forcats::fct_expand(block_type,
                          c("random", "practice", "learning", "testing")),
      "random", "practice", "learning", "testing"
    )) %>%
    group_by(block_type, unitx, trial_type, frequency_type, triplet2) %>%
    reframe(rt.med = median(rt), rt.mean = mean(rt),
            n = n(), correctn = sum(accuracy)) %>%
    mutate(block_type = fct_relevel(block_type, "random", "practice", "learning", "testing")) %>%
    ungroup() %>%
    group_by(block_type, unitx, trial_type, frequency_type) %>%
    reframe(rt.med = median(rt.med), rt.mean = mean(rt.mean),
            n = sum(n), correctn = sum(correctn)) %>%
    mutate(acc = correctn / n)
  
  
  return(d5_summary)
}

############################################################
# 6. Learning Metrics Computation
############################################################

compute_learning_metrics <- function(d_summary) {
  # Filter by frequency and trial type
  random_high <- filter(d_summary, trial_type == "random", frequency_type == "high")
  regular_high <- filter(d_summary, trial_type == "regular", frequency_type == "high")
  random_low  <- filter(d_summary, trial_type == "random", frequency_type == "low")
  regular_low <- filter(d_summary, trial_type == "regular", frequency_type == "low")
  high <- filter(d_summary, frequency_type == "high") %>%
    group_by(block_type, unitx, frequency_type) %>%
    reframe(rt.med = median(rt.med), rt.mean = mean(rt.mean))
  low <- filter(d_summary, frequency_type == "low")
  
  # Sequence learning: random high - regular high
  sequence_learning <- inner_join(random_high, regular_high, by = c("block_type", "unitx"),
                                  suffix = c("_random", "_pattern")) %>%
    mutate(sequence_learning.med = rt.med_random - rt.med_pattern,
           sequence_learning.mean = rt.mean_random - rt.mean_pattern) %>%
    select(block_type, unitx, sequence_learning.med, sequence_learning.mean) %>%
    filter(!block_type %in% c("random", "practice"))
  
  # Statistical learning: random high - random low
  statistical_learning <- inner_join(random_high, random_low, by = c("block_type", "unitx"),
                                     suffix = c("_high", "_low")) %>%
    mutate(statistical_learning.med = rt.med_low - rt.med_high,
           statistical_learning.mean = rt.mean_low - rt.mean_high) %>%
    select(block_type, unitx, statistical_learning.med, statistical_learning.mean) %>%
    filter(!block_type %in% c("random", "practice"))
  
  # Overall learning: low - high
  learning <- inner_join(high, low, by = c("block_type", "unitx"),
                         suffix = c("_high", "_low")) %>%
    mutate(learning.med = rt.med_low - rt.med_high,
           learning.mean = rt.mean_low - rt.mean_high) %>%
    select(block_type, unitx, learning.med, learning.mean) %>%
    filter(!block_type %in% c("random", "practice"))
  
  list(sequence_learning = sequence_learning,
       statistical_learning = statistical_learning,
       learning = learning)
}

############################################################
# 7. Plotting Functions
############################################################

# ---- RT Plot ----
# Plots reaction time curves across unitx (epoch/block) and block_type
# Color: frequency_type (high/low), Shape & linetype: trial_type (random/regular)
asrt_plot <- function(
    unitx, tasktype, d_summary, finfo, sidstr,
    show_legend = TRUE,
    legend_text_size = 12
) {
  print(match_prefix(sidstr, finfo$file))
  x_var <- "unitx"
  base_size <- 12
  
  # Axis label & facet settings
  if (unitx == 'epoch') {
    x_label <- "Epoch"
    figpath_suffix <- "_epoch.jpg"
    facet_params <- list(. ~ block_type, scales = "free_x")
  } else {
    x_label <- "Block"
    figpath_suffix <- "_block.jpg"
    facet_params <- list(. ~ block_type, scales = "free_x", space = "free_x")
  }
  
  # Build plot
  fig <- ggplot(d_summary, aes_string(
    x = x_var, y = "rt.med",
    group = "interaction(trial_type, frequency_type)",
    color = "frequency_type",
    shape = "trial_type",
    linetype = "trial_type"
  )) +
    geom_line() +
    geom_point(size = 4) +
    do.call(facet_grid, facet_params) +
    labs(
      title = paste(tasktype, sidstr, sep = "-"),
      x = x_label, y = "Reaction Time (ms)",
      color = "Frequency Category", linetype = "Trial Type"
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      text = element_text(size = base_size + 4)
    )
  
  # Legend control
  if (!show_legend) {
    fig <- fig + theme(legend.position = "none")
  } else {
    fig <- fig + theme(
      legend.text = element_text(size = legend_text_size),
      legend.title = element_text(size = legend_text_size)
    )
  }
  
  if (unitx != 'epoch') {
    fig <- fig + theme(strip.text = element_blank(), strip.background = element_blank())
  }
  
  # Save figure
  fn <- match_prefix(sidstr, basename(finfo$file))
  figpath <- paste0('figures/', tasktype, '/', str_replace(fn, '.csv', figpath_suffix))
  
  dir.create(file.path("figures", tasktype), recursive = TRUE, showWarnings = FALSE)
  
  ggsave(figpath, width = ifelse(unitx == 'epoch', 20, 40), height = 15, units = "cm")
  
  print(fig)
  return(fig)
}

# ---- Accuracy Plot ----
# Similar to asrt_plot but plots accuracy (acc) instead of RT
asrt_acc_plot <- function(
    unitx, tasktype, d_summary, finfo, sidstr,
    show_legend = TRUE,
    legend_text_size = 12
) {
  print(match_prefix(sidstr, finfo$file))
  x_var <- "unitx"
  base_size <- 12
  
  if (unitx == 'epoch') {
    x_label <- "Epoch"
    figpath_suffix <- "_acc_epoch.jpg"
    facet_params <- list(. ~ block_type, scales = "free_x")
  } else {
    x_label <- "Block"
    figpath_suffix <- "_acc_block.jpg"
    facet_params <- list(. ~ block_type, scales = "free_x", space = "free_x")
  }
  
  fig <- ggplot(d_summary, aes_string(
    x = x_var, y = "acc",
    group = "interaction(trial_type, frequency_type)",
    color = "frequency_type",
    shape = "trial_type",
    linetype = "trial_type"
  )) +
    geom_line() +
    geom_point(size = 4) +
    do.call(facet_grid, facet_params) +
    labs(
      title = paste(tasktype, sidstr, sep = "-"),
      x = x_label, y = "Accuracy",
      color = "Frequency Category", linetype = "Trial Type"
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      text = element_text(size = base_size + 4)
    )
  
  if (!show_legend) {
    fig <- fig + theme(legend.position = "none")
  } else {
    fig <- fig + theme(
      legend.text = element_text(size = legend_text_size),
      legend.title = element_text(size = legend_text_size)
    )
  }
  
  if (unitx != 'epoch') {
    fig <- fig + theme(strip.text = element_blank(), strip.background = element_blank())
  }
  #sidstr <- 'sub105'
  #finfo <- files
  fn <- match_prefix(sidstr, basename(finfo$file))
  
  figpath <- paste0('figures/', tasktype, '/', str_replace(fn, '.csv', figpath_suffix))
  message(figpath)
  ggsave(figpath, width = ifelse(unitx == 'epoch', 20, 40), height = 15, units = "cm")
  
  print(fig)
  return(fig)
}

# ---- Combined Learning Metrics Plot ----
# Combines sequence and statistical learning into a single plot
plot_combined_learning_metrics <- function(
    learning_metrics, finfo, sidstr, tasktype, unitx,
    fontsize = 12, show_legend = TRUE, legend_text_size = 12
) {
  # Rename metrics to generic 'learning'
  sequence_learning <- learning_metrics$sequence_learning %>%
    rename(learning = sequence_learning.med)
  
  statistical_learning <- learning_metrics$statistical_learning %>%
    rename(learning = statistical_learning.med)
  
  # Combine
  combined_learning <- bind_rows(
    sequence_learning %>% mutate(type = "Sequence"),
    statistical_learning %>% mutate(type = "Statistical")
  )
  
  # Use unitx as x-axis
  x_var <- "unitx"
  x_label <- if (unitx == "epoch") "Epoch" else "Block"
  figpath_suffix <- if (unitx == "epoch") "_learning_metrics_epoch.jpg" else "_learning_metrics_block.jpg"
  facet_params <- if (unitx == "epoch") list(. ~ block_type, scales = "free_x") else list(. ~ block_type, scales = "free_x", space = "free_x")
  
  # Plot
  fig <- ggplot(combined_learning, aes_string(
    x = x_var, y = "learning",
    group = "interaction(type, block_type)",
    color = "type"
  )) +
    geom_line() +
    geom_point(size = 4) +
    do.call(facet_grid, facet_params) +
    labs(
      title = "Learning Metrics",
      x = x_label,
      y = "Difference RT (ms)",
      color = "Learning Type"
    ) +
    theme_minimal(base_size = fontsize) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      text = element_text(size = fontsize + 4)
    )
  
  if (!show_legend) {
    fig <- fig + theme(legend.position = "none")
  } else {
    fig <- fig + theme(
      legend.text = element_text(size = legend_text_size),
      legend.title = element_text(size = legend_text_size)
    )
  }
  
  # Ensure folder exists
  dir.create(file.path("figures", tasktype), recursive = TRUE, showWarnings = FALSE)
  
  fn <- match_prefix(sidstr, basename(finfo$file))
  figpath <- paste0('figures/', tasktype, '/', str_replace(fn, '.csv', figpath_suffix))
  ggsave(figpath, plot = fig, width = ifelse(unitx == 'epoch', 20, 40), height = 15, units = "cm")
  
  print(fig)
  return(fig)
}


############################################################
# 8. Sequence Report Analysis
############################################################

# Extract stimulus and reported sequences
extr_stiseq_respseq <- function(d, d_add_cond) {
  # Extract stimulus sequence (first 4 regular trials)
  tmp <- d_add_cond %>%
    ungroup() %>%
    select(trial_type, correct_answer_direction) %>%
    filter(trial_type == 'regular') %>%
    slice(1:4)
  stiseq <- tmp$correct_answer_direction
  
  # Extract reported sequences (key presses)
  tmp2 <- d %>%
    filter(!is.na(seq_report_key.keys)) %>%
    select(starts_with('seq_report'), starts_with('trials.this'))
  
  reportseq <- tmp2 %>%
    select(trials.thisRepN, seq_report_key.keys) %>%
    group_by(trials.thisRepN) %>%
    mutate(blockID = sprintf("L%.2d", row_number())) %>%
    ungroup()
  
  list(stiseq = stiseq, reportseq = reportseq)
}

# Generate all rotations of a sequence vector
generate_rotations <- function(vec) {
  n <- length(vec)
  lapply(0:(n - 1), function(i) c(tail(vec, n - i), head(vec, i)))
}

# Count matches for a rotation within response sequence
count_repeated_elements_and_matches <- function(rotation, respseq) {
  match_count <- 0
  element_match_count <- 0
  len_a <- length(rotation)
  
  for (i in 1:(length(respseq) - len_a + 1)) {
    if (all(rotation == respseq[i:(i + len_a - 1)])) {
      match_count <- match_count + 1
      element_match_count <- element_match_count + len_a
    }
  }
  
  list(match_count = match_count, element_match_count = element_match_count)
}

# Find best rotation with max match count
max_repeated_rotation_in_respseq <- function(stiseq, respseq) {
  rotations <- generate_rotations(stiseq)
  results <- lapply(rotations, count_repeated_elements_and_matches, respseq = respseq)
  
  max_match_count <- max(sapply(results, function(x) x$match_count))
  best_rotation_index <- which.max(sapply(results, function(x) x$match_count))
  best_rotation <- rotations[[best_rotation_index]]
  element_match_count <- results[[best_rotation_index]]$element_match_count
  
  list(max_match_count = max_match_count,
       best_rotation = best_rotation,
       element_match_count = element_match_count)
}

# Plot element match count per block
plot_element_match_count <- function(result, finfo, sidstr, tasktype) {
  fig <- ggplot(result, aes(x = factor(blockID), y = element_match_count)) +
    geom_point(size = 3, color = "blue") +
    geom_line(group = 1, color = "blue") +
    geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
    labs(x = "Learning Block", y = "Element Match Count",
         title = "Sequence Report Accuracy per Learning Block") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5))
  
  fn <- match_prefix(sidstr, basename(finfo$file))
  figpath <- paste0('figures/', tasktype, '/', str_replace(fn, '.csv', '_seqreport.jpg'))
  ggsave(figpath, height = 15, units = "cm")
  
  print(fig)
  return(fig)
}


############################################################
# 9. Statistical Analysis Utilities
############################################################

# Perform paired t-tests and correlations for learning metrics
perform_analysis <- function(
    data_seq, data_stat,
    wide_data_seq, wide_data_stat,
    scenario, unitx
) {
  # Paired t-tests
  t_test_seq <- data_seq %>% t_test(learning ~ tasktype, paired = TRUE)
  t_test_stat <- data_stat %>% t_test(learning ~ tasktype, paired = TRUE)
  print(paste("Paired t-test (Sequence) -", scenario)); print(t_test_seq)
  print(paste("Paired t-test (Statistical) -", scenario)); print(t_test_stat)
  
  # Correlations
  correlation_seq <- cor.test(wide_data_seq$motor, wide_data_seq$percept, use = "complete.obs")
  correlation_stat <- cor.test(wide_data_stat$motor, wide_data_stat$percept, use = "complete.obs")
  print(paste("Correlation (Sequence) -", scenario)); print(correlation_seq)
  print(paste("Correlation (Statistical) -", scenario)); print(correlation_stat)
  
  # Plot correlations (sequence and statistical)
  p_seq <- ggplot(wide_data_seq, aes(x = motor, y = percept)) +
    geom_point(color = "blue", size = 3) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(title = paste("Corr(Motor, Percept): Sequence -", scenario),
         x = "Motor", y = "Percept") +
    theme_minimal()
  print(p_seq)
  
  p_stat <- ggplot(wide_data_stat, aes(x = motor, y = percept)) +
    geom_point(color = "blue", size = 3) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(title = paste("Corr(Motor, Percept): Statistical -", scenario),
         x = "Motor", y = "Percept") +
    theme_minimal()
  print(p_stat)
}

# Perform analysis across blocks (repeated t-tests per block)
perform_analysis_block <- function(
    data_seq, data_stat,
    wide_data_seq, wide_data_stat,
    scenario, unitx
) {
  unique_units <- unique(data_seq$unitx)
  
  for (unit in unique_units) {
    data_seq_unit <- filter(data_seq, unitx == unit)
    data_stat_unit <- filter(data_stat, unitx == unit)
    
    t_test_seq <- data_seq_unit %>% t_test(learning ~ tasktype, paired = TRUE)
    t_test_stat <- data_stat_unit %>% t_test(learning ~ tasktype, paired = TRUE)
    print(paste("Paired t-test (Sequence) - Unit:", unit, "-", scenario)); print(t_test_seq)
    print(paste("Paired t-test (Statistical) - Unit:", unit, "-", scenario)); print(t_test_stat)
  }
}

# Analysis for high vs low frequency learning
perform_analysis_highlow <- function(data, wide_data, scenario) {
  t_test <- data %>% t_test(learning ~ tasktype, paired = TRUE)
  print(paste("Paired t-test (Learning) -", scenario)); print(t_test)
  
  correlation <- cor.test(wide_data$motor, wide_data$percept, use = "complete.obs")
  print(paste("Correlation (Learning) -", scenario)); print(correlation)
}

############################################################
# 10. Outlier Detection
############################################################

get_outliers <- function(data, k = 5) {
  outlier_scores <- lofactor(data, k = k)
  order(outlier_scores, decreasing = TRUE)[1:k]
}

############################################################
# End of asrt_ana_functions.R
############################################################
