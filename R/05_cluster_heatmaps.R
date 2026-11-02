#### 05_cluster_heatmaps.R ####
#
# Heatmaps of the clusters used in the thesis (Figure 3.3):
#
#   rows    = knockdown lines, ordered by average survival
#   columns = average of all blocks, then block 1-4
#   colour  = cluster (darker = higher mortality), pink = line not tested
#   side    = lines chosen for the reproductive-output experiment
#
# Run from the project root. Output: output/figures/


source(file.path("R", "utils.R"))
suppressPackageStartupMessages(library(readxl))

thesis_file <- file.path("data", "clustering", "cluster_results_thesis.xlsx")


#### Lines selected in the thesis (section 3.3, final list) ####

selected <- tribble(
  ~Sex,     ~OR,             ~group,
  "Female", "Or59a",         "High mortality",
  "Female", "Or56a",         "High mortality",
  "Female", "Or33c",         "High mortality",
  "Female", "Or33b",         "High mortality",
  "Female", "Or65c",         "High mortality",
  "Female", "Or85b",         "Low mortality",
  "Female", "Or82a",         "Low mortality",
  "Female", "Or98a",         "Low mortality",
  "Female", "Or22b",         "Low mortality",
  "Female", "Or83c",         "Low mortality",
  "Female", "Or83b - Orco",  "ORCO",
  "Female", "Or30a",         "Larval control",
  "Female", "Or94b",         "Larval control",
  "Male",   "Or59a",         "High mortality",
  "Male",   "Or10a",         "High mortality",
  "Male",   "Or65c",         "High mortality",
  "Male",   "Or46aA/Or46aB", "High mortality",
  "Male",   "Or1a",          "High mortality",
  "Male",   "Or85f",         "Low mortality",
  "Male",   "Or85a",         "Low mortality",
  "Male",   "Or83b - Orco",  "Low mortality",
  "Male",   "Or83c",         "Low mortality",
  "Male",   "Or24a",         "Low mortality",
  "Male",   "Or67a",         "Low mortality",
  "Male",   "Or30a",         "Larval control",
  "Male",   "Or94b",         "Larval control"
)

group_colours <- c("High mortality" = "#b13a40", "Low mortality" = "#54828a",
                   "ORCO" = "#858e35", "Larval control" = "#b8713c")


#### Load the thesis clusters ####

load_clusters <- function(sex) {
  read_excel(thesis_file, sheet = tolower(sex), na = "NA") %>%
    filter(!is.na(OR), OR != "average") %>%   # drop the summary row
    transmute(Sex = sex, OR,
              average_survival = as.numeric(.data[[paste0("ave_", sex)]]),
              Average = ave_k, B1 = B1_k, B2 = B2_k, B3 = B3_k, B4 = B4_k) %>%
    mutate(across(Average:B4, ~ suppressWarnings(as.integer(.x)))) %>%
    filter(!is.na(Average))                       # lines not tested in this sex
}


#### Plot ####

plot_heatmap <- function(sex) {
  d <- load_clusters(sex)
  line_order <- d$OR[order(-d$average_survival)]   # highest mortality at the bottom

  tiles <- d %>%
    pivot_longer(Average:B4, names_to = "column", values_to = "cluster") %>%
    mutate(OR = factor(OR, levels = line_order),
           column = factor(column, levels = c("Average", "B1", "B2", "B3", "B4")),
           cluster = factor(cluster, levels = 1:4))

  side <- d %>%
    select(OR) %>%
    left_join(filter(selected, Sex == sex), by = "OR") %>%
    mutate(OR = factor(OR, levels = line_order), column = "Selected")

  ggplot() +
    geom_tile(data = tiles, aes(column, OR, fill = cluster), colour = "white") +
    scale_fill_manual(values = c("1" = "#b2d8d8", "2" = "#66b2b2", "3" = "#008080",
                                 "4" = "#006666"),
                      na.value = "#d8b2b2", name = "Cluster",
                      labels = c("1 (highest survival)", "2", "3",
                                 "4 (highest mortality)", "not tested")) +
    geom_point(data = filter(side, !is.na(group)),
               aes(x = 0.3, y = OR, colour = group), shape = 15, size = 3) +
    scale_colour_manual(values = group_colours, name = "Selected as") +
    scale_x_discrete(expand = expansion(add = c(0.7, 0.2))) +
    labs(x = NULL, y = NULL, title = paste(sex, "knockdown lines: survival clusters")) +
    theme_minimal(base_size = 10) +
    theme(panel.grid = element_blank(), axis.text.y = element_text(size = 7))
}

for (sx in c("Female", "Male")) {
  save_plot(plot_heatmap(sx), paste0("cluster_heatmap_", tolower(sx), "s.png"),
            width = 16, height = 18)
}
