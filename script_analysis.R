# Zusätzliche Pakete installieren. Anschließend kann die Zeile mit #
# auskommentiert werden.
install.packages(c("cowplot", "apa", "afex"))

# Benötigte Pakete laden
library(tidyverse)
library(cowplot)
library(apa)
library(afex)

# Daten laden
load(url("https://github.com/dgromer/projws20/blob/main/projws20_online.RData?raw=true"))

# Hilfsfunktionen um Plots anzuordnen ------------------------------------------

ggplot_legend <- function(p)
{
  tmp <- ggplotGrob(p)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

ggplot_y_axis_title <- function(p)
{
  tmp <- ggplotGrob(p)
  tit <- grep("axis.title.y.left", sapply(tmp$grobs, function(x) x$name))
  title <- tmp$grobs[[tit]]
  title
}

# Stichprobenbeschreibung ------------------------------------------------------

# Anzahl Versuchspersonen
nrow(proj)

# Anzahl Versuchspersonen pro Gruppe (0 = hohe Erwartung, 1 = niedrige Erwartung)
table(proj$group)

# Geschlecht der Versuchspersonen
table(proj$sex)

# Alter der Versuchspersonen aus Datenschutzgründen nicht im Online-Datensatz
# enthalten. Siehe PowerPoint-Präsentation im Kurs.

# Hypothesentests --------------------------------------------------------------

# Gruppenvergleich Summe der Erwartungsratings in der Extinktionsphase
# (Manipulations-Check)
proj %>%
  t_test(sum_exp_ext ~ group, ., var.equal = TRUE) %>%
  t_apa()

# Gruppenvergleich Erwartungsrating nach dem Reinstatement
proj %>%
  t_test(exp_reinst ~ group, ., var.equal = TRUE) %>%
  t_apa()

# ANOVA:
#   Abhängige Variable: Erwartungsratings
#   Unabhängige Variablen: Gruppe (between),
#                          Zeitpunkt (within, letztes @ Extinktion vs. Reinstatement)
proj %>%
  select(id, group, exp_end_ext, exp_reinst) %>%
  pivot_longer(cols = starts_with("exp"), names_to = "phase",
               values_to = "exp") %>%
  aov_ez(id = "id", dv = "exp", data = ., between = "group",
         within = "phase") %>%
  anova_apa()

# Gruppenvergleich Bedrohlichkeitsratings nach der Extinktionsphase
proj %>%
  t_test(threat_ext ~ group, ., var.equal = TRUE) %>%
  t_apa()

# Gruppenvergleich Bedrohlichkeitsratings nach dem Reinstatement
proj %>%
  t_test(threat_reinst ~ group, ., var.equal = TRUE) %>%
  t_apa()

# ANOVA:
#   Abhängige Variable: Bedrohlichkeitsrating
#   Unabhängige Variablen: Gruppe (between),
#                          Zeitpunkt (within, nach Extinktion vs. Reinstatement)
proj %>%
  select(id, group, threat_ext, threat_reinst) %>%
  pivot_longer(cols = starts_with("threat"), names_to = "phase",
               values_to = "threat") %>%
  aov_ez(id = "id", dv = "threat", data = ., between = "group",
         within = "phase") %>%
  anova_apa()

# Plots ------------------------------------------------------------------------

exp_data_plots <-
  exp %>%
  group_by(group, phase, cs_type, trial) %>%
  summarize(mean_expectancy = mean(expectancy),
            se_expectancy = sd(expectancy) / sqrt(n()))

p_exp_cond <-
  exp_data_plots %>%
  filter(phase == "conditioning") %>%
  ggplot(aes(x = trial, y = mean_expectancy, color = cs_type)) +
  geom_path(size = .3) +
  geom_point(size = .7) +
  geom_errorbar(aes(ymin = mean_expectancy - se_expectancy,
                    ymax = mean_expectancy + se_expectancy),
                width = .4, size = .3) +
  facet_wrap(group ~ ., nrow = 2, strip.position = "top",
             labeller = labeller(group = c(`0` = "HE", `1` = "NE"))) +
  scale_x_continuous(limits = c(.5, 12.5), expand = c(0, .0),
                     breaks = seq(2, 12, 2)) +
  scale_y_continuous(limits = c(0, 100), expand = c(.02, .02),
                     labels = function(x) paste0(x, "%")) +
  scale_color_viridis_d(end = .7, labels = c("CS+", "CS-", "CSk")) +
  labs(title = "Konditionierung",
       y = "Erwartung",
       color = "Reiz") +
  theme_gray(base_size = 9)

p_exp_ext <-
  exp_data_plots %>%
  filter(phase == "extinction") %>%
  
  ggplot(aes(x = trial, y = mean_expectancy, color = cs_type)) +
  geom_path(size = .3) +
  geom_point(size = .7) +
  geom_errorbar(aes(ymin = mean_expectancy - se_expectancy,
                    ymax = mean_expectancy + se_expectancy),
                width = .4, size = .3) +
  facet_wrap(group ~ ., nrow = 2, strip.position = "top",
             labeller = labeller(group = c(`0` = "HE", `1` = "NE"))) +
  scale_x_continuous(limits = c(.5, 12.5), expand = c(0, .0),
                     breaks = seq(2, 12, 2)) +
  scale_y_continuous(limits = c(0, 100), expand = c(.02, .02)) +
  scale_color_viridis_d(end = .7) +
  labs(title = "Extinktion") +
  theme_gray(base_size = 9)

p_exp_reinst <-
  exp_data_plots %>%
  filter(phase == "reinstatement") %>%
  ggplot(aes(x = trial, y = mean_expectancy, color = cs_type)) +
  geom_path(size = .3) +
  geom_point(size = .7) +
  geom_errorbar(aes(ymin = mean_expectancy - se_expectancy,
                    ymax = mean_expectancy + se_expectancy),
                width = .075, size = .3) +
  facet_wrap(group ~ ., nrow = 2, strip.position = "top",
             labeller = labeller(group = c(`0` = "HE", `1` = "NE"))) +
  scale_x_continuous(limits = c(.75, 1.25), expand = c(0, .0),
                     breaks = 1) +
  scale_y_continuous(limits = c(0, 100), expand = c(.02, .02)) +
  scale_color_viridis_d(end = .7) +
  labs(title = "Reinstatement") +
  theme_gray(base_size = 9)

# Extract legend and y axis title from first plot
legend <- ggplot_legend(p_exp_cond)
title <- ggplot_y_axis_title(p_exp_cond)

p_exp <-
  plot_grid(title,
            p_exp_cond +
              guides(color = FALSE) +
              theme(axis.title = element_blank(),),
            p_exp_ext +
              guides(color = FALSE) +
              theme(axis.title = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank()),
            p_exp_reinst +
              guides(color = FALSE) +
              theme(axis.title = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank()),
            legend,
            nrow = 1, rel_widths = c(.12, 1.07, .93, .4, .4))

p_exp

p_threat <-
  threat %>%
  group_by(group, phase, cs_type) %>%
  summarize(mean_threat = mean(threat),
            se_threat = sd(threat) / sqrt(n())) %>%
  ggplot(aes(x = phase, y = mean_threat, color = cs_type,
             group = cs_type)) +
  geom_path() +
  geom_point() +
  facet_wrap(vars(group), labeller = labeller(group = c(`0` = "HE",
                                                        `1` = "NE"))) +
  scale_x_discrete(labels = c("Habituation", "Konditionierung", "Extinktion",
                              "Reinstatement")) +
  scale_y_continuous(limits = c(0, 100), expand = c(.02, .02)) +
  scale_color_viridis_d(end = .7, labels = c("CS+", "CS-", "CSk")) +
  geom_errorbar(aes(ymin = mean_threat - se_threat,
                    ymax = mean_threat + se_threat),
                width = .3) +
  labs(x = "Phase",
       y = "Bedrohlichkeit",
       color = "Reiz") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_threat

