# Figure 1: mutually exclusive employment categories in the pooled lean sample.
library(tidyverse)
source("code/helpers/fixed_employment_sample.R")
source("code/helpers/pooled_employment_sample.R")

categories <- c(
  self_employed = "Self-employed (farm/nonfarm)",
  wage_employed = "Wage employed (farm/nonfarm)",
  salary_employed = "Salary employed",
  no_activity = "Inactive / no reported activity"
)
sample <- fixed_employment_sample() |>
  filter(region != "Addis Ababa") |>
  classify_pooled_employment()
stopifnot(all(sample$employment_group %in% names(categories)))

shares <- sample |>
  count(region, employment_group) |>
  complete(region, employment_group = names(categories), fill = list(n = 0L)) |>
  group_by(region) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  mutate(
    region = factor(region, levels = rev(sort(unique(region)))),
    category = factor(employment_group, levels = names(categories),
                      labels = unname(categories))
  )
stopifnot(all(near(summarise(group_by(shares, region), total = sum(share))$total, 1)))

percent_label <- scales::label_percent(accuracy = 1)
plot <- ggplot(shares, aes(share, region)) +
  geom_col(aes(fill = category), width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = percent_label(share)), hjust = -0.2,
            size = 3, colour = "#253341") +
  facet_wrap(vars(category), ncol = 2) +
  scale_fill_manual(values = c("#19756D", "#315F89", "#9A5526", "#626C7A")) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2), labels = percent_label,
    expand = expansion(mult = c(0, 0.12))
  ) +
  expand_limits(x = 0.6) +
  labs(x = "Share of observations within region", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = grid::unit(1.1, "lines"),
    strip.background = element_rect(fill = "#EEF2F5", colour = NA),
    strip.text = element_text(face = "bold", colour = "#253341", margin = margin(6, 4, 6, 4)),
    axis.text = element_text(colour = "#253341"),
    plot.margin = margin(8, 10, 6, 8)
  )

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("output/figures/employ_gr.pdf", plot, device = grDevices::pdf,
       width = 175, height = 112, units = "mm", bg = "white")
print(select(shares, region, category, n, share), n = Inf)
