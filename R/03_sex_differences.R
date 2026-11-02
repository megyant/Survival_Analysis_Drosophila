#### 03_sex_differences.R ####
#
# Do males and females of the same knockdown line differ in survival?
#
#   for each line:  Cox model  Surv ~ Sex + strata(Block)
#                   hazard ratio < 1  ->  males less likely to die than females
#
# Stratifying by block lets each block have its own baseline risk, so
# block-to-block variation is not mistaken for a sex effect.
#
# Run from the project root. Output: output/tables/, output/figures/


source(file.path("R", "utils.R"))

flies <- load_survival_data()


#### 1. Cox model per line ####

# only lines with both sexes tested
lines_both_sexes <- flies %>%
  group_by(OR) %>%
  filter(n_distinct(Sex) == 2) %>%
  ungroup() %>%
  mutate(OR = droplevels(OR))

sex_effects <- lines_both_sexes %>%
  group_by(OR) %>%
  group_modify(~ {
    cox <- coxph(Surv(timeINhours, Censor) ~ Sex + strata(Block), data = .x)
    broom::tidy(cox, exponentiate = TRUE, conf.int = TRUE) %>%
      mutate(n_female = sum(.x$Sex == "Female"), n_male = sum(.x$Sex == "Male"))
  }) %>%
  ungroup() %>%
  transmute(OR, n_female, n_male,
            hazard_ratio_male = estimate, conf.low, conf.high, p_value = p.value,
            p_adjusted_fdr = p.adjust(p.value, method = "fdr")) %>%
  arrange(p_value)

write_csv(sex_effects, file.path(TABLE_DIR, "sex_differences_by_line.csv"))

significant <- filter(sex_effects, p_value < 0.05)
print(significant)


#### 2. Plot: male vs female curves for lines with a significant sex effect ####

if (nrow(significant) > 0) {
  sig_flies <- filter(lines_both_sexes, OR %in% significant$OR) %>%
    mutate(OR = droplevels(OR))

  curves <- sig_flies %>%
    group_by(OR) %>%
    group_modify(~ broom::tidy(survfit(Surv(timeINhours, Censor) ~ Sex, data = .x))) %>%
    ungroup() %>%
    mutate(Sex = str_remove(strata, "^Sex="))

  curves <- curves %>%
    bind_rows(distinct(curves, OR, Sex) %>%
                mutate(time = 0, estimate = 1, conf.low = 1, conf.high = 1)) %>%
    arrange(OR, Sex, time)

  hr_labels <- significant %>%
    mutate(label = sprintf("HR = %.2f\nP = %.3f", hazard_ratio_male, p_value))

  p <- ggplot(curves, aes(time, estimate, colour = Sex, fill = Sex)) +
    geom_step(linewidth = 0.9) +
    geom_text(data = hr_labels, aes(x = 2, y = 0.45, label = label),
              inherit.aes = FALSE, hjust = 0, size = 3) +
    facet_wrap(~ OR) +
    scale_colour_manual(values = c(Female = "#C0392B", Male = "#2471A3")) +
    scale_x_continuous(breaks = c(0, 24, 48, 72)) +
    coord_cartesian(ylim = c(0.4, 1)) +
    labs(x = "Hours after infection", y = "Survival probability", colour = NULL,
         title = "Lines where males and females differ in survival",
         subtitle = "Blocks pooled; HR = male vs female hazard (Cox, stratified by block)") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  save_plot(p, "sex_differences_significant_lines.png", width = 18, height = 11)
}


#### 3. Overview: male vs female survival for every line ####

surv_by_line_sex <- survival_proportions(lines_both_sexes) %>%
  group_by(OR, Sex) %>%
  summarise(survival = mean(survival), .groups = "drop") %>%
  pivot_wider(names_from = Sex, values_from = survival) %>%
  left_join(select(sex_effects, OR, p_value), by = "OR") %>%
  mutate(significant = p_value < 0.05)

p_overview <- ggplot(surv_by_line_sex, aes(Female, Male)) +
  geom_abline(linetype = "dashed", colour = "grey60") +
  geom_point(aes(colour = significant), size = 2.5) +
  ggrepel::geom_text_repel(data = filter(surv_by_line_sex, significant),
                           aes(label = OR), size = 3) +
  scale_colour_manual(values = c(`FALSE` = "grey60", `TRUE` = "#C0392B"),
                      labels = c("not significant", "P < 0.05"), name = NULL) +
  coord_equal() +
  labs(x = "Female survival at 72 h (mean of blocks)",
       y = "Male survival at 72 h (mean of blocks)",
       title = "Male vs female survival per knockdown line") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

save_plot(p_overview, "sex_survival_per_line.png", width = 14, height = 15)
