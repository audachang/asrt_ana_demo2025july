# ASRT Analysis Pipeline and Visualization

This repository contains an updated modular pipeline for processing, analyzing, and visualizing data from the Alternating Serial Reaction Time (ASRT) task, supporting both **motor** and **ual** task types. It includes:

-   A **functions library** (`asrt_ana_functions.R`) providing core data processing and plotting utilities.
-   **Analysis scripts** for summarizing data, group-level plots, statistical analysis, and single-subject inspection.
-   A **Shiny app** for interactive visualization of individual participant data.

------------------------------------------------------------------------

## **Repository Structure**

```         
asrt_ana_demo2025july/
│
├── scripts/
│   ├── asrt_ana_functions.R        # Main functions library (data processing, plotting, analysis)
│   ├── summ_data.R                 # Summarize all participant data into a CSV
│   ├── plot_group.R                # Generate group-level plots
│   ├── inspect_rawdata_onesubj.R   # Inspect and plot single participant data
│   └── inspect_sids_shiny.R        # Shiny app for interactive visualization
│
├── data/
│   └── motor/                      # Raw motor task CSV data

│
├── figures/
│   └── motor/                      # Saved plots for motor task participants

│
├── output/
│   └── all_d_summary_Xepoch.csv    # Example summary file (can be epoch or block based)
│
└── README.md                       # This documentation
```

------------------------------------------------------------------------

## **Key Functions (in `asrt_ana_functions.R`)**

### **1. File Management**

-   `list_file(tasktype)`: List available participant CSV files for a given task type (`motor` or \`\`). Returns tibble with file paths and subject IDs.
-   `select_file(sidstr, finfo)`: Select a specific participant file from the list by subject ID.
-   `match_prefix(prefix, file_list)`: Utility to match files by prefix (auto-handles basename).

### **2. Data Import and Preprocessing**

-   `import_d(file_path, tasktype, sid)`: Import raw ASRT data, set target orientations, create triplet codes.
-   `add_condition_cols(d, tasktype)`: Add block types (random, practice, learning, testing), block IDs, trial numbers, and trial types (random/regular).
-   `assign_freq(d)`: Compute high/low frequency classification for triplets.
-   `clean_data(d)`: Filter data by RT range, accuracy, and remove trills/repetitions.

### **3. Summarization**

-   `create_summ4plot(d, unitx)`: Summarize trial-level data into block/epoch summaries (median/mean RT, accuracy by trial/frequency type).

### **4. Learning Metrics**

-   `compute_learning_metrics(d_summary)`: Compute sequence learning (random high − pattern high), statistical learning (random high − random low), and overall learning metrics.

### **5. Plotting**

-   `asrt_plot(unitx, tasktype, d_summary, files, sidstr)`: Plot RT learning curves by trial/frequency type.
-   `asrt_acc_plot(unitx, tasktype, d_summary, files, sidstr)`: Plot accuracy curves by trial/frequency type.
-   `plot_combined_learning_metrics(learning_metrics, finfo, sidstr, tasktype, unitx)`: Plot combined sequence and statistical learning.
-   `plot_element_match_count(result, finfo, sidstr, tasktype)`: Plot sequence report accuracy (element match counts).

### **6. Sequence Report Analysis**

-   `extr_stiseq_respseq(d, d_add_cond)`: Extract stimulus and reported sequences for sequence reporting blocks.

------------------------------------------------------------------------

## **Example Usage**

### **Summarize All Participants**

``` r
source('scripts/asrt_ana_functions.R')
source('scripts/summ_data.R')  # Summarizes and exports combined data
```

### **Inspect Single Participant**

``` r
source('scripts/asrt_ana_functions.R')

tasktype <- 'motor'
sidstr <- 'sub105'
unitx <- 'epoch'

files <- list_file(tasktype)
file_path <- files$file[files$sid == sidstr]

data <- import_d(file_path, tasktype, sidstr) %>%
  add_condition_cols(tasktype) %>%
  assign_freq() %>%
  clean_data()

dsummary <- create_summ4plot(data, unitx)

# Plot RT and accuracy
asrt_plot(unitx, tasktype, dsummary, files, sidstr)
asrt_acc_plot(unitx, tasktype, dsummary, files, sidstr)
```

### **Run Shiny App**

``` r
shiny::runApp('scripts/inspect_sids_shiny.R')
```

------------------------------------------------------------------------

## **Outputs**

-   **Figures**: Saved automatically to `figures/<tasktype>/` per participant.
-   **Summary Data**: Saved to `output/all_d_summary_X<unitx>.csv` for group-level analysis.

------------------------------------------------------------------------

## **Dependencies**

-   R packages: `dplyr`, `ggplot2`, `purrr`, `tidyr`, `forcats`, `stringr`, `data.table`, `rio`, `trimr`, `DMwR`, `shiny`.

Install all required packages:

``` r
install.packages(c(
  "dplyr", "ggplot2", "purrr", "tidyr", "forcats", "stringr",
  "data.table", "rio", "trimr", "DMwR", "shiny"
))
```

------------------------------------------------------------------------

## **Notes**

-   Both **motor** and **ual** task data follow the same pipeline.
-   All plots and summaries are automatically ordered by block type: random → practice → learning → testing.
-   The modular structure allows easy integration into other analysis workflows or adaptation for similar sequence learning tasks.

------------------------------------------------------------------------

## **Details: Frequency Categorization (compute_frequency)**

The function `compute_frequency()` classifies triplets (three-target sequences) into **high-frequency** and **low-frequency** categories using a data-driven median threshold derived from random trials.

### **Step-by-step logic**

1.  **Input expected**\
    A dataset containing:

    -   `trial_type` (random/regular)
    -   `triplet_type` and `triplet2` (different triplet encodings)
    -   `block_type` and `epoch` for grouping

2.  **Filter data**\
    Exclude `practice` and initial `random` blocks, analyze only `learning` and `testing` phases.

3.  **Count occurrences**\
    For each `(trial_type, triplet_type, triplet2)` combination, count total occurrences across the dataset.

4.  **Determine threshold**\
    Compute **median count** of triplets appearing in random trials. This median serves as the cut-off:

    ``` r
    random_threshold <- median(trialtype_triplet_num_total for trial_type == "random")
    ```

5.  **Assign frequency type**

    -   Triplets with counts **≥ threshold** → `high`
    -   Triplets with counts **\< threshold** → `low`

6.  **Aggregate by epoch**\
    Compute per-epoch statistics:

    -   `trialtype_triplet_num_epoch` (count of each triplet per epoch)
    -   `trialtype_triplettype_frequency_num_epoch` (sum by type/frequency per epoch)
    -   `trialtype_frequencytype_num_epoch` (sum by frequency/trial type per epoch)

7.  **Output**\
    Returns enriched dataset with:

    -   Frequency classification (`high` or `low`)
    -   Counts per triplet, frequency type, trial type, and epoch
    -   Threshold used (`trialtype_triplet_num_total_threshold`)

------------------------------------------------------------------------

### **Why use median of random triplets?**

-   Provides an **adaptive threshold** for each dataset’s distribution.
-   Balances high/low classification for random trials.
-   Ensures meaningful contrast for **statistical learning effects** (random high − random low).

------------------------------------------------------------------------

### **Schematic Diagram**

Below is a conceptual flow of `compute_frequency()`:

```         
Raw Data (trial_type, triplet, block_type, epoch)
        │
        ├─ Filter out practice/random blocks
        │
        ├─ Count triplets by (trial_type, triplet2)
        │
        ├─ Compute median count (random triplets) = threshold
        │
        ├─ Label triplets:
        │      count >= threshold → HIGH
        │      count < threshold  → LOW
        │
        └─ Aggregate by epoch & join back with frequency labels
               ↓
         Output: triplet frequency by epoch with HIGH/LOW tag
```

