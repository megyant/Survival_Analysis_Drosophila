#### run_all.R ####
# Runs the full analysis from the processed data. Run from the project root:
#   Rscript R/run_all.R
# (01_prepare_survival_data.R is not included here: it needs the raw lab
#  sheets, which are not in this repository.)

for (script in c("02_survival_analysis.R", "03_sex_differences.R",
                 "04_clustering.R", "05_cluster_heatmaps.R")) {
  message("\n==== ", script, " ====")
  source(file.path("R", script), local = new.env())
}
