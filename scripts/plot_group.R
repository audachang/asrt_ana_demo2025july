# Plot group-level learning curves
# Load summarized data and generate group plots

source('asrt_ana_functions.R')

unitx <- 'epoch'

# Load processed data
all_summary <- import(file.path('../output', sprintf('all_d_summary_X%s.csv', unitx)))

# Plot learning curves
p <- plot_learning_curve(all_summary, unitx)

# Save plot
ggsave(file.path('../output', sprintf('learning_curve_%s.png', unitx)), p, width = 8, height = 5)
