#### 01_prepare_survival_data.R ####
#
# Turns the raw lab sheets for one block and one sex into one row per fly,
# the format needed for survival analysis:
#
#   vial counts (dead per check, flies at start)  ->  one row per fly
#   Censor = 1  fly died at timeINhours
#   Censor = 0  fly alive at the last check (~72 h)
#
# Adapted and extended by Margarida Antunes from a mortality-analysis
# script by Elvira Lafuente (2023).
#
# NOTE: the raw Excel sheets are not included in this repository. The
# processed output of this script is in data/processed/, and every other
# script starts from there.
#
# Expected input:
#   data/raw/B<n>/B<n>_OR_Data.xlsx       sheets "Mortality" and "backbone"
#   data/raw/B<n>/Backbone_Library.xlsx   sheet  "library"
#
# The processed files were checked by hand after this step (rows with
# merge problems corrected) before analysis.


#### Settings ####

BLOCK <- "B1"        # "B1", "B2", "B3" or "B4"
SEX   <- "Female"    # "Female" or "Male"

# Flies lost during the experiment (escaped, stuck in food) were excluded in
# the thesis analysis. Set to TRUE to keep them as censored observations.
INCLUDE_CENSORED <- FALSE

raw_dir <- file.path("data", "raw", BLOCK)


#### Libraries ####

suppressPackageStartupMessages({
  library(readxl)
  library(tidyverse)
})


#### Load and merge ####

mort       <- read_excel(file.path(raw_dir, paste0(BLOCK, "_OR_Data.xlsx")),
                         sheet = "Mortality", na = "NA")
backbone   <- read_excel(file.path(raw_dir, paste0(BLOCK, "_OR_Data.xlsx")),
                         sheet = "backbone", na = "NA")
or_library <- read_excel(file.path(raw_dir, "Backbone_Library.xlsx"),
                         sheet = "library", na = "NA")

# mortality checks + vial information (start date, sex) + OR line information
checks <- mort %>%
  left_join(backbone,   by = "VialNumber") %>%
  left_join(or_library, by = "Code") %>%
  filter(Sex == SEX)

hours_since_start <- function(date, hour, date_start, hour_start) {
  as.numeric(difftime(date, date_start, units = "days") * 24 + (hour - hour_start))
}

fly_columns <- c("DateStart", "VialNumber", "OR", "Chromossome", "Stock_Center",
                 "Line", "Code", "Type", "timeINhours", "Sex")


#### Flies that died ####

checks <- checks %>%
  mutate(timeINhours = hours_since_start(Date, Hour, DateStart, HourStart))

dead <- checks %>%
  filter(!is.na(DeadFlies), DeadFlies > 0) %>%
  select(all_of(fly_columns), DeadFlies) %>%
  uncount(DeadFlies) %>%                       # one row per dead fly
  mutate(Censor = 1)

censored <- NULL
if (INCLUDE_CENSORED) {
  censored <- checks %>%
    filter(!is.na(CensoredFlies), CensoredFlies > 0) %>%
    select(all_of(fly_columns), CensoredFlies) %>%
    uncount(CensoredFlies) %>%
    mutate(Censor = 0)
}


#### Flies alive at the end ####

# time of the last (72 h) check
end_date <- max(checks$Date[checks$Timepoint == 72])
end_hour <- max(checks$Hour[checks$Timepoint == 72])

vial_counts <- checks %>%
  group_by(VialNumber) %>%
  summarise(dead     = sum(DeadFlies,     na.rm = TRUE),
            censored = sum(CensoredFlies, na.rm = TRUE))

# one row of line information per vial
vial_info <- checks %>%
  distinct(VialNumber, DateStart, HourStart, OR, Chromossome, Stock_Center,
           Line, Code, Type, Sex)

alive <- vial_counts %>%
  left_join(select(backbone, VialNumber, ID_start), by = "VialNumber") %>%
  mutate(Alive = ID_start - (dead + censored)) %>%
  filter(!is.na(Alive), Alive > 0) %>%
  left_join(vial_info, by = "VialNumber") %>%
  mutate(timeINhours = hours_since_start(end_date, end_hour, DateStart, HourStart)) %>%
  select(all_of(fly_columns), Alive) %>%
  uncount(Alive) %>%                           # one row per surviving fly
  mutate(Censor = 0)


#### Check and save ####

# sanity check: no vial should have more flies than it started with
vial_check <- vial_counts %>%
  left_join(select(backbone, VialNumber, ID_start), by = "VialNumber") %>%
  filter(dead + censored > ID_start)
if (nrow(vial_check) > 0) {
  warning("Some vials have more dead + censored flies than flies at start:")
  print(vial_check)
}

all_flies <- bind_rows(dead, censored, alive)

out_file <- file.path("data", "processed",
                      paste0("survival_", tolower(SEX), "s_", BLOCK, ".csv"))
write.csv(all_flies, out_file)
message("Saved ", nrow(all_flies), " flies to ", out_file)
