#### utils.R ####
# Shared helpers. Sourced by the analysis scripts; run them from the project root.

suppressPackageStartupMessages({
  library(tidyverse)
  library(survival)
  library(survminer)
})

PROCESSED_DIR <- file.path("data", "processed")
TABLE_DIR     <- file.path("output", "tables")
FIGURE_DIR    <- file.path("output", "figures")
dir.create(TABLE_DIR,  recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)

# Load the per-fly survival files for all blocks and both sexes.
# One row per fly: timeINhours = time of death or censoring,
# Censor = 1 (died) or 0 (alive at the end of the experiment).
load_survival_data <- function(dir = PROCESSED_DIR) {
  files <- list.files(dir, pattern = "^survival_(fe)?males_B\\d\\.csv$", full.names = TRUE)

  map_dfr(files, function(f) {
    read_csv(f, col_types = cols(.default = "c"), name_repair = "minimal")[, -1] %>%
      mutate(Block = as.integer(str_extract(basename(f), "(?<=_B)\\d")))
  }) %>%
    filter(!is.na(OR)) %>%                       # 13 rows in B1 females have no line information
    mutate(
      DateStart   = as.Date(lubridate::parse_date_time(DateStart, c("Ymd", "dmy"))),
      timeINhours = as.numeric(timeINhours),
      Censor      = as.numeric(Censor),
      Sex         = factor(Sex, levels = c("Female", "Male")),
      Block       = factor(Block),
      OR          = factor(OR)
    )
}

# Proportion of flies alive at the end of the experiment, per line, sex and block
survival_proportions <- function(data) {
  data %>%
    group_by(OR, Sex, Block) %>%
    summarise(survival = mean(Censor == 0), n = n(), .groups = "drop")
}

save_plot <- function(plot, name, width = 18, height = 12) {
  ggsave(file.path(FIGURE_DIR, name), plot, width = width, height = height,
         units = "cm", dpi = 300, bg = "white")
}
