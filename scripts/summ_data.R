# Summarize and preprocess raw ASRT data (Rewritten for new modular functions)
# Usage: Run this script to process raw data and save summary for downstream analysis

source('asrt_ana_functions.R')

# Parameters
tasktype <- 'motor'   # or 'percept'
#unitx <- 'block'      # 'epoch' or 'block'
unitx <- 'epoch'      # 'epoch' or 'block'
metric.type <- 'median'  # 'median' or 'mean'

# Step 1: Get file list (returns tibble with file paths and subject IDs)
files <- list_file(tasktype)

# Step 2: Process each participant
all_summary <- files %>%
  mutate(summary = map2(file, sid, function(file_path, sid) {
    # --- Full preprocessing pipeline ---
    # 1. Import data
    d <- import_d(file_path, tasktype, sid)
    # 2. Add block/trial metadata
    d <- add_condition_cols(d, tasktype)
    # 3. Assign triplet frequency (high/low)
    d <- assign_freq(d)
    # 4. Clean trials (RT range, accuracy, no trills/repeats)
    d <- clean_data(d)
        # 5. Summarize by epoch/block using new function
    create_summ4plot(d, unitx)
    
  })) %>%
  select(sid, summary) %>%
  unnest(summary)

# Step 3: Save processed summary
output_path <- file.path('../output', sprintf('all_d_summary_X%s.csv', unitx))
export(all_summary, output_path)

cat("Summary saved to:", output_path, "\n")
