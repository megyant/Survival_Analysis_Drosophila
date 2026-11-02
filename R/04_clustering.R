#### 04_clustering.R ####
#
# Groups knockdown lines into high- and low-mortality clusters.
#
#   survival at 72 h per line  ->  one column per block + the average
#   k-means (k = 4) on each column separately
#   cluster 1 = highest survival ... cluster 4 = lowest survival (highest mortality)
#
#   selection:  high mortality = cluster 4 on average AND in >= 2 blocks
#               low mortality  = cluster 1 on average AND in >= 2 blocks
#
# k = 4 is the value used in the thesis. The silhouette check below is kept
# for transparency: on this data it scores k = 2-3 slightly higher, so k = 4
# is a practical choice (enough groups to separate the extremes), not the
# statistical optimum. Lines are also grouped with Ward hierarchical
# clustering (Euclidean distance).
#
# The clusters used in the thesis are in data/clustering/cluster_results_thesis.xlsx.
# This script recomputes them from the raw survival data and reports how
# closely they agree (k-means can split borderline lines differently).
#
# Run from the project root. Output: output/tables/, output/figures/


source(file.path("R", "utils.R"))
suppressPackageStartupMessages({
  library(readxl)
  library(cluster)
})

K <- 4
set.seed(240)

flies <- load_survival_data()


#### 1. Survival table: one row per line, one column per block ####

survival_wide <- survival_proportions(flies) %>%
  select(OR, Sex, Block, survival) %>%
  pivot_wider(names_from = Block, values_from = survival, names_prefix = "B") %>%
  mutate(average = rowMeans(across(starts_with("B")), na.rm = TRUE)) %>%
  select(Sex, OR, average, B1, B2, B3, B4)


#### 2. Choose k with the silhouette method ####

silhouette_scores <- survival_wide %>%
  group_by(Sex) %>%
  group_modify(~ {
    x <- .x$average
    map_dfr(2:6, function(k) {
      km <- kmeans(x, centers = k, nstart = 25)
      tibble(k = k, silhouette = mean(silhouette(km$cluster, dist(x))[, "sil_width"]))
    })
  }) %>%
  ungroup()

write_csv(silhouette_scores, file.path(TABLE_DIR, "clustering_silhouette.csv"))

p_sil <- ggplot(silhouette_scores, aes(k, silhouette)) +
  geom_line() + geom_point(size = 2) +
  geom_vline(xintercept = K, linetype = "dashed", colour = "grey50") +
  facet_wrap(~ Sex) +
  labs(x = "Number of clusters (k)", y = "Mean silhouette width",
       title = "Choosing the number of clusters") +
  theme_minimal(base_size = 11)

save_plot(p_sil, "clustering_silhouette.png", width = 16, height = 8)


#### 3. k-means per column, clusters ordered by survival ####

# k-means labels are arbitrary; reorder so 1 = highest survival, k = lowest
ordered_kmeans <- function(x, k = K) {
  out <- rep(NA_integer_, length(x))
  ok <- !is.na(x)
  km <- kmeans(x[ok], centers = k, nstart = 25)
  rank_of_cluster <- rank(-km$centers[, 1])
  out[ok] <- as.integer(rank_of_cluster[km$cluster])
  out
}

clusters <- survival_wide %>%
  group_by(Sex) %>%
  mutate(across(c(average, B1, B2, B3, B4), ordered_kmeans, .names = "{.col}_k")) %>%
  ungroup()

select_lines <- function(df) {
  df %>%
    mutate(
      blocks_high = rowSums(across(c(B1_k, B2_k, B3_k, B4_k)) == K, na.rm = TRUE),
      blocks_low  = rowSums(across(c(B1_k, B2_k, B3_k, B4_k)) == 1, na.rm = TRUE),
      selection = case_when(
        average_k == K & blocks_high >= 2 ~ "high mortality",
        average_k == 1 & blocks_low  >= 2 ~ "low mortality",
        TRUE ~ "")
    )
}

clusters <- select_lines(clusters)
write_csv(clusters, file.path(TABLE_DIR, "clusters_recomputed.csv"))


#### 4. Compare with the clusters used in the thesis ####

read_thesis_clusters <- function(sex) {
  read_excel(file.path("data", "clustering", "cluster_results_thesis.xlsx"),
             sheet = tolower(sex), na = "NA") %>%
    filter(!is.na(OR), OR != "average") %>%   # drop the summary row
    transmute(Sex = sex, OR,
              average_k = as.integer(ave_k),
              B1_k = as.integer(B1_k), B2_k = as.integer(B2_k),
              B3_k = as.integer(B3_k), B4_k = as.integer(B4_k))
}

thesis_clusters <- bind_rows(read_thesis_clusters("Female"), read_thesis_clusters("Male"))

agreement <- clusters %>%
  select(Sex, OR, ends_with("_k")) %>%
  pivot_longer(ends_with("_k"), names_to = "column", values_to = "recomputed") %>%
  inner_join(thesis_clusters %>%
               pivot_longer(ends_with("_k"), names_to = "column", values_to = "thesis"),
             by = c("Sex", "OR", "column")) %>%
  filter(!is.na(recomputed), !is.na(thesis)) %>%
  group_by(Sex, column = str_remove(column, "_k$")) %>%
  summarise(lines = n(), same_cluster = sum(recomputed == thesis),
            agreement = round(same_cluster / lines, 2), .groups = "drop")

write_csv(agreement, file.path(TABLE_DIR, "clustering_agreement_with_thesis.csv"))
print(agreement)


#### 5. Ward hierarchical clustering ####

for (sx in c("Female", "Male")) {
  m <- clusters %>%
    filter(Sex == sx) %>%
    # lines not tested in a block get their own average for that block,
    # so every pair of lines can be compared
    mutate(across(c(B1, B2, B3, B4), ~ coalesce(.x, average))) %>%
    select(OR, B1, B2, B3, B4) %>%
    column_to_rownames("OR") %>%
    as.matrix()

  tree <- hclust(dist(m, method = "euclidean"), method = "ward.D2")

  png(file.path(FIGURE_DIR, paste0("clustering_dendrogram_", tolower(sx), "s.png")),
      width = 22, height = 12, units = "cm", res = 300)
  plot(tree, main = paste(sx, "lines: Ward clustering of survival per block"),
       xlab = "", sub = "", ylab = "Height", cex = 0.7)
  rect.hclust(tree, k = K, border = "grey40")
  dev.off()
}

message("Done: clusters in ", file.path(TABLE_DIR, "clusters_recomputed.csv"))
