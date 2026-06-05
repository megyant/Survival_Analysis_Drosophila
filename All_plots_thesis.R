# All the plots to be presented at thesis

# Libraries
library(readxl)
library(ggplot2)
library(survival)
library(survivalAnalysis)
library(survminer)
library(paletteer)
library(cowplot)

# Functions
select <- dplyr::select
filter <- dplyr::filter
summarise <- dplyr::summarise

## Mortality

## Read dataset

All <- read.csv("~/Tese/ORs/Datasets/Blocks/All/All.csv")
All_females<-All[All$Sex=="Female",]
All_males<-All[All$Sex=="Male",]

## Functions necessary for plotting

## Females

# Creating survfit model
# Survfit: This function creates survival curves from 
#either a formula (e.g. the Kaplan-Meier), 
#a previously fitted Cox model, or a previously fitted 
#accelerated failure time model.

fit_females <- survfit(Surv(timeINhours, Censor) ~  OR, data = All_females)
coxfit_females <- coxph(Surv(timeINhours, Censor) ~  OR, data = All_females)
surv_object_females <- Surv(time = All_females$timeINhours, event = All_females$Censor)

# If necessary to turn coxph into a csv file
# coxdf<-cox_as_data_frame(coxfit, unmangle_dict=NULL,factor_id_sep = ":",sort_by = NULL)
# #write.csv(coxdf, file = "~/Tese/ORs/Datasets/Blocks/B1/Males/coxfit_males.csv")

## Males

fit_males <- survfit(Surv(timeINhours, Censor) ~  OR, data = All_males)
coxfit_males <- coxph(Surv(timeINhours, Censor) ~  OR, data = All_males)
surv_object_males <- Surv(time = All_males$timeINhours, event = All_males$Censor)


# Plotting

# Females

F_surv<-ggsurvplot(
  fit_females,
  data = All_females,
  xlab = "Time in Hours",
  ylab = "Survival Probability (Females)",
  legend.title = "Olfactory Receptors konockdown lines",
  legend.labs = levels(All_females$OR), # Assign unique labels to each OR
  ggtheme = theme_bw(base_size = 12), # Minimal theme with readable font size
  palette = paletteer_c('ggthemes::Blue-Teal', 43),                        # Use color palette for better distinction,                         # Show confidence intervals                    # Facet by block for readability
  font.legend = 10,                         # Adjust legend font size
  font.tickslab = 10,                      # Adjust tick labels font size
  font.x = 10,                             # Adjust x-axis font size
  font.y = 10,
  labels = "A",
  label_size = 14,
  ylim=c(0.40,1)
  # Adjust y-axis font size
)


F_surv

F_surv_gg <- F_surv$plot

F_surv_gg

F_surv_label<- cowplot::ggdraw() +
  draw_plot(F_surv_gg) +
  draw_label(
    "A", 
    x = 0.05, y = 0.95,    # Adjust position (x, y in normalized coordinates: 0 to 1)
    hjust = 0, vjust = 1,  # Align left and top
    size = 14, fontface = "bold"
  )

F_surv_label
# 1000*800.png

# Males

M_surv <-ggsurvplot(
  fit_males,
  data = All_males,
  xlab = "Time in Hours",
  ylab = "Survival Probability (Males)",
  legend.title = "Olfactory Receptors konockdown lines",
  legend.labs = levels(All_males$OR), # Assign unique labels to each OR
  ggtheme = theme_bw(base_size = 12), # Minimal theme with readable font size
  palette = paletteer_c('ggthemes::Classic Gray', 43),                        # Use color palette for better distinction,                         # Show confidence intervals                    # Facet by block for readability
  font.legend = 10,                         # Adjust legend font size
  font.tickslab = 10,                      # Adjust tick labels font size
  font.x = 10,                             # Adjust x-axis font size
  font.y = 10,
  ylim=c(0.40,1)# Adjust y-axis font size
)

M_surv

M_surv_gg <- M_surv$plot

M_surv_label<- cowplot::ggdraw() +
  draw_plot(M_surv_gg) +
  draw_label(
    "B", 
    x = 0.05, y = 0.95,    # Adjust position (x, y in normalized coordinates: 0 to 1)
    hjust = 0, vjust = 1,  # Align left and top
    size = 14, fontface = "bold"
  )

M_surv_label

# Plot them together


# Combine the ggplot objects vertically
combined_plot <- plot_grid(
  F_surv_gg, M_surv_gg, 
  ncol = 1,            # Stack vertically
  rel_heights = c(1, 1), # Equal height for both plots
  labels = c("A", "B"), # Add labels
  label_size = 14       # Adjust label size if needed
)
combined_plot

# Save to A4 size (21 x 29.7 cm)
ggsave("~/Tese/Escrita/Plots/survival_stacked_plots_A4.pdf", combined_plot, width = 30, height = 45, units = "cm")
ggsave("~/Tese/Escrita/Plots/survival_stacked_plots_A4.png", combined_plot, width = 30, height = 45, units = "cm")



## Sex Mortality Plots

### Or7a

All_Or7a<- All_females<-All[All$OR=="Or7a",]
All_Or7a<-na.omit(All_Or7a)

fit_Or7a <- survfit(Surv(timeINhours, Censor) ~  OR + Sex, data = All_Or7a)

Or7a_surv<-ggsurvplot(
  fit_Or7a,
  data = All_Or7a,
  xlab = "Time in Hours",
  ylab = "Survival Probability",
  legend.title = "Or7a konockdown line",
  legend.labs = c('Female','Male'),
  ggtheme = theme_bw(base_size = 10),
  palette=c("#008384","#4D4D4D"))

Or7a_surv

Or7a_surv_gg <- Or7a_surv$plot

Or7a_surv_label<- cowplot::ggdraw() +
  draw_plot(Or7a_surv_gg) +
  draw_label(
    "A", 
    x = 0.05, y = 0.95,    # Adjust position (x, y in normalized coordinates: 0 to 1)
    hjust = 0, vjust = 1,  # Align left and top
    size = 11, fontface = "bold"
  )
Or7a_surv_label

ggsave("~/Tese/Escrita/Plots/Or7a_Sex_plot_A.pdf", Or7a_surv_gg, width = 25, height = 20, units = "cm")
ggsave("~/Tese/Escrita/Plots/Or7a_Sex_plot_A.png", Or7a_surv_gg, width = 25, height = 20, units = "cm")
ggsave("~/Tese/Escrita/Plots/Or7a_Sex_plot_A.pdf", Or7a_surv_label, width = 25, height = 20, units = "cm")
ggsave("~/Tese/Escrita/Plots/Or7a_Sex_plot_A.png", Or7a_surv_label, width = 25, height = 20, units = "cm")


### Or33b

All_Or33b<- All_females<-All[All$OR=="Or33b",]
All_Or33b<-na.omit(All_Or33b)

fit_Or33b <- survfit(Surv(timeINhours, Censor) ~  OR + Sex, data = All_Or33b)

Or33b_surv<-ggsurvplot(
  fit_Or33b,
  data = All_Or33b,
  xlab = "Time in Hours",
  ylab = "Survival Probability",
  legend.title = "Or33b konockdown line",
  legend.labs = c('Female','Male'),
  ggtheme = theme_bw(base_size = 10),
  palette=c("#008384","#4D4D4D"))

Or33b_surv

Or33b_surv_gg <- Or33b_surv$plot

Or33b_surv_label<- cowplot::ggdraw() +
  draw_plot(Or33b_surv_gg) +
  draw_label(
    "B", 
    x = 0.05, y = 0.95,    # Adjust position (x, y in normalized coordinates: 0 to 1)
    hjust = 0, vjust = 1,  # Align left and top
    size = 11, fontface = "bold"
  )
Or33b_surv_label

ggsave("~/Tese/Escrita/Plots/Or33b_Sex_plot.pdf", Or33b_surv_gg, width = 25, height = 20, units = "cm")
ggsave("~/Tese/Escrita/Plots/Or33b_Sex_plot.png", Or33b_surv_gg, width = 25, height = 20, units = "cm")
ggsave("~/Tese/Escrita/Plots/Or33b_Sex_plot_B.pdf", Or33b_surv_label, width = 25, height = 20, units = "cm")
ggsave("~/Tese/Escrita/Plots/Or33b_Sex_plot_B.png", Or33b_surv_label, width = 25, height = 20, units = "cm")

### Or33c

All_Or33c<- All_females<-All[All$OR=="Or33c",]
All_Or33c<-na.omit(All_Or33c)

fit_Or33c <- survfit(Surv(timeINhours, Censor) ~  OR + Sex, data = All_Or33c)

Or33c_surv<-ggsurvplot(
  fit_Or33c,
  data = All_Or33c,
  xlab = "Time in Hours",
  ylab = "Survival Probability",
  legend.title = "Or33c konockdown line",
  legend.labs = c('Female','Male'),
  ggtheme = theme_bw(base_size = 10),
  palette=c("#008384","#4D4D4D"))

Or33c_surv

Or33c_surv_gg <- Or33c_surv$plot

Or33c_surv_label<- cowplot::ggdraw() +
  draw_plot(Or33c_surv_gg) +
  draw_label(
    "C", 
    x = 0.05, y = 0.95,    # Adjust position (x, y in normalized coordinates: 0 to 1)
    hjust = 0, vjust = 1,  # Align left and top
    size = 11, fontface = "bold"
  )
Or33c_surv_label

ggsave("~/Tese/Escrita/Or33c_Sex_plot.pdf", Or33c_surv_gg, width = 25, height = 20, units = "cm")
ggsave("~/Tese/Escrita/Or33c_Sex_plot.png", Or33c_surv_gg, width = 25, height = 20, units = "cm")
ggsave("~/Tese/Escrita/Or33c_Sex_plot_C.pdf", Or33b_surv_label, width = 25, height = 20, units = "cm")
ggsave("~/Tese/Escrita/Or33c_Sex_plot_C.png", Or33b_surv_label, width = 25, height = 20, units = "cm")

### Or56a

All_Or56a<- All_females<-All[All$OR=="Or56a",]
All_Or56a<-na.omit(All_Or56a)

fit_Or56a <- survfit(Surv(timeINhours, Censor) ~  OR + Sex, data = All_Or56a)

Or56a_surv<-ggsurvplot(
  fit_Or56a,
  data = All_Or56a,
  xlab = "Time in Hours",
  ylab = "Survival Probability",
  legend.title = "Or56a konockdown line",
  legend.labs = c('Female','Male'),
  ggtheme = theme_bw(base_size = 10),
  palette=c("#008384","#4D4D4D"))

Or56a_surv

Or56a_surv_gg <- Or56a_surv$plot

Or56a_surv_label<- cowplot::ggdraw() +
  draw_plot(Or56a_surv_gg) +
  draw_label(
    "D", 
    x = 0.05, y = 0.95,    # Adjust position (x, y in normalized coordinates: 0 to 1)
    hjust = 0, vjust = 1,  # Align left and top
    size = 11, fontface = "bold"
  )
Or56a_surv_label

ggsave("~/Tese/Escrita/Plots/Or56a_Sex_plot.pdf", Or56a_surv_gg, width = 25, height = 20, units = "cm")
ggsave("~/Tese/Escrita/Plots/Or56a_Sex_plot.png", Or56a_surv_gg, width = 25, height = 20, units = "cm")
ggsave("~/Tese/Escrita/Plots/Or56a_Sex_plot_D.pdf", Or56a_surv_label, width = 25, height = 20, units = "cm")
ggsave("~/Tese/Escrita/Plots/Or56a_Sex_plot_D.png", Or56a_surv_label, width = 25, height = 20, units = "cm")


# Combine them
combined_plot_Sex <- plot_grid(
  Or7a_surv_gg, Or33b_surv_gg, Or33c_surv_gg, Or56a_surv_gg, 
  ncol = 2,            # Stack vertically
  rel_heights = c(1, 1), # Equal height for both plots
  labels = c("A", "B", "C", "D"), # Add labels
  label_size = 14       # Adjust label size if needed
)

combined_plot_Sex
ggsave("~/Tese/Escrita/survival_Sex_plots.pdf", combined_plot_Sex, width = 25, height = 20, units = "cm")
ggsave("~/Tese/Escrita/survival_Sex_plots.png", combined_plot_Sex, width = 25, height = 20, units = "cm")


## Clustering 

# See clustering r

## Fecundity - Done the altering

# See Mortal_Fecund_Data.R




