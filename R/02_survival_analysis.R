#### 02_survival_analysis.R ####
#
# Survival of 43 OR knockdown lines after P. entomophila infection.
#
#   per sex, all blocks pooled  -> Kaplan-Meier curves, log-rank test
#   per sex and block           -> Cox model, log-rank test,
#                                  top and bottom 10% / 20% of lines
#
# Run from the project root. Output: output/tables/, output/figures/


source(file.path("R", "utils.R"))

flies <- load_survival_data()


#### 1. Differences between lines, per sex (blocks pooled) ####

# log-rank test across lines, stratified by block so block effects
# don't count as line differences
line_tests <- flies %>%
  group_by(Sex) %>%
  group_modify(~ {
    test <- survdiff(Surv(timeINhours, Censor) ~ OR + strata(Block), data = .x)
    tibble(n_flies = nrow(.x), n_lines = n_distinct(.x$OR),
           chisq = test$chisq, df = length(test$n) - 1, p_value = test$pvalue)
  }) %>%
  ungroup()

write_csv(line_tests, file.path(TABLE_DIR, "logrank_lines_by_sex.csv"))
print(line_tests)


#### 2. Kaplan-Meier curves per sex (blocks pooled) ####

km_curves <- flies %>%
  group_by(Sex) %>%
  group_modify(~ broom::tidy(survfit(Surv(timeINhours, Censor) ~ OR, data = .x))) %>%
  ungroup() %>%
  mutate(OR = str_remove(strata, "^OR="))

# add the starting point (time 0, survival 1) so every curve starts at 1
km_curves <- km_curves %>%
  bind_rows(distinct(km_curves, Sex, OR) %>% mutate(time = 0, estimate = 1)) %>%
  arrange(Sex, OR, time)

p_km <- ggplot(km_curves, aes(time, estimate, group = OR, colour = OR)) +
  geom_step(alpha = 0.8) +
  facet_wrap(~ Sex) +
  scale_colour_manual(values = hcl.colors(n_distinct(km_curves$OR), "Dark 3")) +
  scale_x_continuous(breaks = c(0, 24, 48, 72)) +
  coord_cartesian(ylim = c(0.4, 1)) +
  labs(x = "Hours after infection", y = "Survival probability",
       title = "Survival of OR knockdown lines after P. entomophila infection") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

save_plot(p_km, "km_all_lines_by_sex.png", width = 22, height = 11)


#### 3. Per block and sex: Cox model and log-rank ####

block_results <- flies %>%
  group_by(Sex, Block) %>%
  group_modify(~ {
    cox <- coxph(Surv(timeINhours, Censor) ~ OR, data = .x)
    broom::tidy(cox, exponentiate = TRUE, conf.int = TRUE) %>%
      mutate(term = str_remove(term, "^OR"),
             reference_line = levels(droplevels(.x$OR))[1])
  }) %>%
  ungroup() %>%
  rename(OR = term, hazard_ratio = estimate)

write_csv(block_results, file.path(TABLE_DIR, "cox_by_sex_and_block.csv"))

block_tests <- flies %>%
  group_by(Sex, Block) %>%
  group_modify(~ {
    test <- survdiff(Surv(timeINhours, Censor) ~ OR, data = .x)
    tibble(n_flies = nrow(.x), n_lines = n_distinct(.x$OR),
           chisq = test$chisq, p_value = test$pvalue)
  }) %>%
  ungroup()

write_csv(block_tests, file.path(TABLE_DIR, "logrank_by_sex_and_block.csv"))
print(block_tests)


#### 4. Top and bottom lines per block and sex ####

final_survival <- survival_proportions(flies)

extremes <- final_survival %>%
  group_by(Sex, Block) %>%
  mutate(rank_best = rank(-survival, ties.method = "min"),
         rank_worst = rank(survival, ties.method = "min"),
         n_lines = n(),
         group = case_when(
           rank_best  <= ceiling(0.1 * n_lines) ~ "top 10%",
           rank_worst <= ceiling(0.1 * n_lines) ~ "bottom 10%",
           rank_best  <= ceiling(0.2 * n_lines) ~ "top 20%",
           rank_worst <= ceiling(0.2 * n_lines) ~ "bottom 20%",
           TRUE ~ "middle")) %>%
  ungroup() %>%
  select(Sex, Block, OR, survival, n, group) %>%
  arrange(Sex, Block, desc(survival))

write_csv(extremes, file.path(TABLE_DIR, "top_bottom_lines_by_block.csv"))


#### 5. Plot: curves per block, top and bottom 10% highlighted ####

block_curves <- flies %>%
  group_by(Sex, Block) %>%
  group_modify(~ broom::tidy(survfit(Surv(timeINhours, Censor) ~ OR, data = .x))) %>%
  ungroup() %>%
  mutate(OR = str_remove(strata, "^OR="))

block_curves <- block_curves %>%
  bind_rows(distinct(block_curves, Sex, Block, OR) %>% mutate(time = 0, estimate = 1)) %>%
  left_join(select(extremes, Sex, Block, OR, group), by = c("Sex", "Block", "OR")) %>%
  mutate(highlight = if_else(group %in% c("top 10%", "bottom 10%"), group, "other")) %>%
  arrange(Sex, Block, OR, time)

for (sx in levels(flies$Sex)) {
  d <- filter(block_curves, Sex == sx)
  p <- ggplot(d, aes(time, estimate, group = OR)) +
    geom_step(data = filter(d, highlight == "other"), colour = "grey80") +
    geom_step(data = filter(d, highlight != "other"), aes(colour = highlight),
              linewidth = 0.9) +
    facet_wrap(~ Block, labeller = labeller(Block = function(b) paste("Block", b))) +
    scale_colour_manual(values = c("top 10%" = "#00AFBB", "bottom 10%" = "#E7B800"),
                        name = NULL) +
    scale_x_continuous(breaks = c(0, 24, 48, 72)) +
    labs(x = "Hours after infection", y = "Survival probability",
         title = paste(sx, "survival per block: top and bottom 10% of lines"),
         caption = "Line names: output/tables/top_bottom_lines_by_block.csv") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  save_plot(p, paste0("km_top_bottom_", tolower(sx), "s.png"), width = 20, height = 16)
}

message("Done: tables in ", TABLE_DIR, ", figures in ", FIGURE_DIR)
