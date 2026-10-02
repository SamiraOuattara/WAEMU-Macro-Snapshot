# WAEMU macro snapshot using the IMF World Economic Outlook (April 2026)
# Question: How have growth, inflation and public debt in the WAEMU countries
# changed since 2015-2019, and what do the April 2026 projections imply
# as oil prices rise?
#
# Data: IMF WEO database, April 2026 vintage (published April 14, 2026).
# Put WEOApr2026all.xlsx in the same folder as this script, or change `file`.

library(tidyverse)
library(readxl)
library(scales)
library(ggrepel)   

file <- "WEOApr2026all.xlsx"

# ---- 1. Settings ------------------------------------------------------------

waemu <- c("BEN", "BFA", "CIV", "GNB", "MLI", "NER", "SEN", "TGO")

indicators <- c(
  NGDP_RPCH   = "Real GDP growth (%)",
  PCPIPCH     = "Inflation, avg consumer prices (%)",
  GGXWDG_NGDP = "Gross public debt (% of GDP)",
  BCA_NGDPD   = "Current account balance (% of GDP)"
)

# ---- 2. Load and reshape ----------------------------------------------------
# The Countries sheet is wide: one row per country and indicator, one column
# per year. We turn it into long format (one row per country-indicator-year).

raw <- read_excel(file, sheet = "Countries")

weo <- raw |>
  filter(`COUNTRY.ID` %in% waemu, `INDICATOR.ID` %in% names(indicators)) |>
  select(country = COUNTRY, indicator_id = `INDICATOR.ID`,
         latest_actual = LATEST_ACTUAL_ANNUAL_DATA, matches("^[0-9]{4}$")) |>
  pivot_longer(matches("^[0-9]{4}$"), names_to = "year", values_to = "value") |>
  mutate(
    year          = as.integer(year),
    value         = suppressWarnings(as.numeric(value)),
    latest_actual = suppressWarnings(as.integer(latest_actual)),
    # years after the last actual data point are IMF estimates or projections
    projection    = year > latest_actual,
    indicator     = unname(indicators[indicator_id])
  ) |>
  filter(!is.na(value))

# Quick checks
glimpse(weo)
weo |> count(country, indicator_id) |> print(n = Inf)

# ---- 3. Summary table: baseline vs recent vs projection ---------------------

summary_tbl <- weo |>
  group_by(country, indicator) |>
  summarise(
    avg_2015_2019 = mean(value[year %in% 2015:2019]),
    y2024         = first(value[year == 2024], default = NA_real_),
    y2025         = first(value[year == 2025], default = NA_real_),
    proj_2026     = first(value[year == 2026], default = NA_real_),
    proj_2027     = first(value[year == 2027], default = NA_real_),
    .groups = "drop"
  ) |>
  mutate(change_vs_baseline = proj_2026 - avg_2015_2019) |>
  arrange(indicator, desc(change_vs_baseline))

print(summary_tbl, n = Inf)
write_csv(summary_tbl, "waemu_summary_table.csv")

# ---- 4. Oil prices (Commodity Prices sheet) ---------------------------------

brent <- read_excel(file, sheet = "Commodity Prices") |>
  filter(`INDICATOR.ID` == "POILBRE") |>
  select(matches("^[0-9]{4}$")) |>
  pivot_longer(everything(), names_to = "year", values_to = "usd_per_barrel") |>
  mutate(year = as.integer(year),
         usd_per_barrel = as.numeric(usd_per_barrel)) |>
  filter(year >= 2010, !is.na(usd_per_barrel))

# ---- 5. Charts --------------------------------------------------------------


ink_dark <- "#0b0b0b"
ink_mid  <- "#52514e"
grid_col <- "#e6e5e1"

country_cols <- c(
  "Benin"         = "#0072B2",   # blue
  "Burkina Faso"  = "#E69F00",   # orange
  "Côte d'Ivoire" = "#009E73",   # green
  "Guinea-Bissau" = "#CC79A7",   # pink
  "Mali"          = "#56B4E9",   # sky blue
  "Niger"         = "#D55E00",   # vermillion
  "Senegal"       = "#000000",   # black
  "Togo"          = "#6A3D9A"    # violet
)

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      plot.background   = element_rect(fill = "white", colour = NA),
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(colour = grid_col, linewidth = 0.4),
      plot.title        = element_text(face = "bold", colour = ink_dark),
      plot.subtitle     = element_text(colour = ink_mid),
      plot.caption      = element_text(colour = ink_mid, size = 9),
      text              = element_text(colour = ink_dark),
      axis.text         = element_text(colour = ink_mid),
      legend.position   = "bottom",
      legend.text       = element_text(colour = ink_dark)
    )
)

#  all countries on one chart. Solid = actual data, dashed = IMF estimates
# and projections. The dashed line starts at the last actual point,
# so there is no gap between the two.
plot_indicator <- function(ind_id, ylab, from = 2010, zero_line = FALSE) {
  d <- weo |>
    filter(indicator_id == ind_id, year >= from) |>
    mutate(is_senegal = country == "Senegal")
  actual <- d |> filter(!projection)
  proj   <- d |> filter(year >= latest_actual)
  ends   <- d |> group_by(country) |> filter(year == max(year)) |> ungroup()

  p <- ggplot(d, aes(year, value, colour = country, linewidth = is_senegal))
  if (zero_line) p <- p + geom_hline(yintercept = 0, colour = "#b9b8b2", linewidth = 0.4)
  p +
    geom_line(data = actual) +
    geom_line(data = proj, linetype = "dashed") +
    geom_text_repel(data = ends, aes(year, value, label = country, colour = country),
                    inherit.aes = FALSE, direction = "y", hjust = 0,
                    nudge_x = 0.6, segment.colour = NA, size = 3.6,
                    show.legend = FALSE, xlim = c(max(ends$year) + 0.4, NA)) +
    scale_colour_manual(values = country_cols, name = NULL) +
    scale_linewidth_manual(values = c(`FALSE` = 0.9, `TRUE` = 1.6), guide = "none") +
    scale_x_continuous(breaks = seq(2010, 2030, 5),
                       expand = expansion(mult = c(0.02, 0.16))) +
    guides(colour = guide_legend(nrow = 2, override.aes = list(linewidth = 1.4))) +
    labs(title = indicators[[ind_id]],
         subtitle = "WAEMU countries. Dashed = IMF estimates and projections",
         x = NULL, y = ylab,
         caption = "Source: IMF World Economic Outlook, April 2026")
}

p_debt   <- plot_indicator("GGXWDG_NGDP", "% of GDP")
p_infl   <- plot_indicator("PCPIPCH", "Percent", zero_line = TRUE)
p_growth <- plot_indicator("NGDP_RPCH", "Percent", zero_line = TRUE)
p_ca     <- plot_indicator("BCA_NGDPD", "% of GDP", zero_line = TRUE)

# Oil: solid = actual, dashed = projection
brent_actual <- brent |> filter(year <= 2025)
brent_proj   <- brent |> filter(year >= 2025)

p_brent <- ggplot(brent, aes(year, usd_per_barrel)) +
  geom_line(data = brent_actual, linewidth = 1.1, colour = "#0072B2") +
  geom_line(data = brent_proj, linewidth = 1.1, colour = "#0072B2", linetype = "dashed") +
  labs(title = "Brent crude oil price",
       subtitle = "US dollars per barrel. Dashed = IMF projection",
       x = NULL, y = "US$ per barrel",
       caption = "Source: IMF World Economic Outlook, April 2026")

# Debt: how much has each country moved from its 2015-2019 baseline?
debt_change <- summary_tbl |>
  filter(indicator == "Gross public debt (% of GDP)") |>
  mutate(country = fct_reorder(country, change_vs_baseline))

p_debt_change <- ggplot(debt_change, aes(change_vs_baseline, country, fill = country)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = round(change_vs_baseline)), hjust = -0.25,
            colour = ink_dark, size = 3.8) +
  scale_fill_manual(values = country_cols, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Change in public debt",
       subtitle = "2015-2019 average to 2026 projection, percentage points of GDP",
       x = NULL, y = NULL,
       caption = "Source: IMF World Economic Outlook, April 2026")

print(p_debt); print(p_infl); print(p_growth); print(p_ca)
print(p_brent); print(p_debt_change)

dir.create("charts", showWarnings = FALSE)
ggsave("charts/debt.png",        p_debt,        width = 10, height = 5.5, dpi = 200, bg = "white")
ggsave("charts/inflation.png",   p_infl,        width = 10, height = 5.5, dpi = 200, bg = "white")
ggsave("charts/growth.png",      p_growth,      width = 10, height = 5.5, dpi = 200, bg = "white")
ggsave("charts/current_acct.png", p_ca,         width = 10, height = 5.5, dpi = 200, bg = "white")
ggsave("charts/brent.png",       p_brent,       width = 9, height = 5, dpi = 200, bg = "white")
ggsave("charts/debt_change.png", p_debt_change, width = 9, height = 5, dpi = 200, bg = "white")

# ---- 6. Next steps ( analysis) ------------------------------------------
# - Which countries have the biggest debt increase vs their 2015-2019 baseline?
# - Does inflation track oil prices (try cor() or a simple lm() by country)?
# - Which countries run current account deficits, and are they widening
#   in the projection years?

