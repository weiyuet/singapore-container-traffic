##############################
# Container Traffic Analysis #
##############################

# 1.0 Setup ----
## 1.1 Load libraries ----
library(tidyverse)
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
  select(year_month, container_throughput)

print(container_throughput_clean)

container_cargo_clean <- container_cargo_raw %>%
  mutate(
    year_month = as.yearmon(month),
    cargo_throughput = as.numeric(cargo_throughput)
  ) %>%
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
  mutate(year = as.integer(format(year_month, "%Y")),
         month_num = as.integer(format(year_month, "%m")),
         month_label = format(year_month, "%b"),
         container_ratio = containerised / (containerised + conventional),
         tonnes_per_teu = containerised / container_throughput)

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

caption <- "Data: Maritime and Port Authority of Singapore (MPA) | Project: https://github.com/weiyuet/singapore-data"

# 4.0 Explore Data ----
## 4.1 Exploratory plot ----
container_traffic_metrics %>%
  ggplot(aes(x = year_month, y = container_throughput)) +
  geom_line()

## 4.2 Seasonal effects ----
plot_1 <- container_traffic_metrics %>%
  ggplot(aes(x = month_num, y = container_throughput, group = year, color = year)) +
  geom_line(alpha = 0.7, linewidth = 0.8) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_viridis_c(option = "magma") +
  base_theme() +
  labs(
    title = "Singapore Port Throughput: The Chinese New Year Dip",
    subtitle = "Note the dramatic, recurring drop every February",
    caption = caption,
    x = NULL,
    y = "Throughput ('000 TEUs)",
    color = "Year"
  )

## 4.3 Container density trends ----
plot_2 <- container_traffic_metrics %>% 
  ggplot(aes(x = year_month, y = tonnes_per_teu)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_smooth(method = "gam", color = "darkorange", se = FALSE, linetype = "dashed") +
  base_theme() +
  labs(
    title = "Historical Container Density Trends",
    subtitle = "Freight Weight / Volume Ratio (Tonnes per TEU)",
    x = NULL,
    y = "Density Value (Tonnes / TEU)",
    caption = caption
  )

# 5.0 Export and Save Images ----
all_plots <- list(
  "seasonal-effects" = plot_1,
  "container-density-trends" = plot_2
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
