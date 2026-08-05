##############################
# Container Traffic Analysis #
##############################

# 1.0 Setup ----
## 1.1 Load libraries ----
library(tidyverse)
library(ggridges)
library(httr2)
library(jsonlite)
library(zoo)
library(janitor)

# 2.0 Load Data ----
## 2.1 Define API variables ----
fetch_mpa_dataset <- function(dataset_id) {
  api_url <- "https://data.gov.sg/api/action/datastore_search"

  response <- request(api_url) %>%
    req_url_query(resource_id = dataset_id, limit = 5e+04) %>%
    req_perform()

  parsed_data <- response %>%
    resp_body_json(simplifyVector = TRUE)
  return(as_tibble(parsed_data$result$records))
}

id_container_throughput <- "d_da030f7028200d19ffcbe4a2d71af39c"
id_container_cargo <- "d_835d43b9238c6fc877dfcd62d73054a9"

## 2.2 Retrieve data ----
container_throughput_raw <- fetch_mpa_dataset(id_container_throughput)
container_cargo_raw <- fetch_mpa_dataset(id_container_cargo)

print(container_throughput_raw)
print(container_cargo_raw)

# 3.0 Prep Data ----
## 3.1 Correct date data type and convert container numbers to numeric ----
container_throughput_clean <- container_throughput_raw %>%
  mutate(
    year_month = as.yearmon(month),
    container_throughput = as.numeric(container_throughput)
  ) %>%
  drop_na(container_throughput) %>%
  select(year_month, container_throughput)

print(container_throughput_clean)

container_cargo_clean <- container_cargo_raw %>%
  mutate(
    year_month = as.yearmon(month),
    cargo_throughput = as.numeric(cargo_throughput)
  ) %>%
  drop_na(cargo_throughput) %>%
  select(year_month, cargo_type_secondary, cargo_throughput) %>%
  pivot_wider(
    names_from = cargo_type_secondary,
    values_from = cargo_throughput,
    values_fill = 0
  ) %>%
  clean_names()

print(container_cargo_clean)

## 3.2 Join datasets and calculate maritime metrics ----
container_traffic_metrics <- container_throughput_clean %>%
  inner_join(container_cargo_clean, by = "year_month") %>%
  mutate(
    year = as.integer(format(year_month, "%Y")),
    month_num = as.integer(format(year_month, "%m")),
    month_label = factor(format(year_month, "%b"), levels = rev(month.abb)),
    container_ratio = containerised / (containerised + conventional),
    tonnes_per_teu = containerised / container_throughput
  )

print(container_traffic_metrics)

## 3.3 Shared plot configurations ----
base_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.caption = element_text(hjust = 0, color = "gray40", size = 8),
      panel.grid.minor = element_blank()
    )
}

caption <- "Data: Maritime and Port Authority of Singapore (MPA) | Project: https://github.com/weiyuet/singapore-container-traffic"

# 4.0 Explore Data ----
## 4.1 Segmented Annual CAGR calculation ----
# Filter complete 12-month calendar years to prevent edge-year distortion
annual_throughput <- container_traffic_metrics %>%
  group_by(year) %>%
  filter(n() == 12) %>%
  summarise(total_teu = sum(container_throughput), .groups = "drop")

print(annual_throughput)

min_yr <- min(annual_throughput$year)
max_yr <- max(annual_throughput$year)

# Define macroeconomic periods within dataset limits
cagr_periods <- tribble(
  ~period                  , ~start_yr , ~end_yr ,
  "1995–2007 (Pre-GFC)"    ,      1995 ,    2007 ,
  "2008–2019 (Post-GFC)"   ,      2008 ,    2019 ,
  "2020–2025 (Post-COVID)" ,      2020 ,    2025
) %>%
  filter(start_yr >= min_yr & end_yr <= max_yr) %>%
  inner_join(annual_throughput, by = c("start_yr" = "year")) %>%
  rename(v_start = total_teu) %>%
  inner_join(annual_throughput, by = c("end_yr" = "year")) %>%
  rename(v_end = total_teu) %>%
  mutate(
    n_years = end_yr - start_yr,
    cagr = (v_end / v_start)^(1 / n_years) - 1,
    cagr_str = sprintf("%s: %.2f%%", period, cagr * 100)
  )

print(cagr_periods)

cagr_annotation <- paste0(
  "Segmented Annual CAGR:\n",
  paste(cagr_periods$cagr_str, collapse = "\n")
)

## 4.2 Monthly throughput & CAGR plot
plot_1 <- container_traffic_metrics %>%
  ggplot(aes(x = year_month, y = container_throughput)) +
  geom_line(color = "steelblue") +
  annotate(
    "label",
    x = min(container_traffic_metrics$year_month),
    y = max(container_traffic_metrics$container_throughput),
    label = cagr_annotation,
    hjust = 0,
    vjust = 1,
    fill = "white",
    alpha = 0.85,
    size = 3.5,
    fontface = "bold"
  ) +
  base_theme() +
  labs(
    title = "Singapore Port Monthly Container Throughput",
    caption = caption,
    x = NULL,
    y = "Throughput ('000 TEUs)"
  )

## 4.3 Seasonal effects ----
plot_2 <- container_traffic_metrics %>%
  ggplot(aes(x = container_throughput, y = month_label, fill = after_stat(x))) +
  geom_density_ridges_gradient(
    scale = 3,
    rel_min_height = 0.01,
    show.legend = FALSE
  ) +
  scale_fill_viridis_c(option = "magma") +
  base_theme() +
  labs(
    title = "Singapore Port Container Throughput Distributions by Month",
    subtitle = "Visualizing historical density variance (Note lower throughput every February)",
    caption = caption,
    x = "Throughput ('000 TEUs)",
    y = NULL,
  )

## 4.4 Container density trends ----
plot_3 <- container_traffic_metrics %>%
  ggplot(aes(x = year_month, y = tonnes_per_teu)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_smooth(
    method = "gam",
    color = "darkorange",
    se = FALSE,
    linetype = "dashed"
  ) +
  base_theme() +
  labs(
    title = "Historical Container Density Trends",
    subtitle = "Freight Weight / Volume Ratio (Tonnes per TEU)",
    x = NULL,
    y = "(Tonnes / TEU)",
    caption = caption
  )

# 5.0 Export and Save Images ----
all_plots <- list(
  "cumulative-throughput" = plot_1,
  "seasonal-effects" = plot_2,
  "container-density-trends" = plot_3
)

iwalk(
  all_plots,
  ~ ggsave(
    filename = paste0("figures/container-traffic-", .y, ".png"),
    plot = .x,
    width = 10,
    height = 8
  )
)

# End ----
