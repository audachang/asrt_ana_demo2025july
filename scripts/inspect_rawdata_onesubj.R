# Inspect raw data for a single subject
source('asrt_ana_functions.R')

tasktype <- 'motor'
sidstr <- 'sub105'
unitx <- 'epoch'

# Load single subject file
files <- list_file(tasktype)
file_path <- files$file[files$sid == sidstr]

if (length(file_path) == 0) stop('Subject file not found.')

# Full preprocessing pipeline
data <- import_d(file_path, tasktype, sidstr) %>%
  add_condition_cols(tasktype) %>%
  assign_freq() %>%
  clean_data()

# Summarize for plotting
dsummary <- create_summ4plot(data, unitx)

# Plot RT learning curve
asrt_plot(unitx, tasktype, dsummary, files, sidstr)

# Plot accuracy curve
asrt_acc_plot(unitx, tasktype, dsummary, files, sidstr)

