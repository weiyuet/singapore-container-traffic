# Singapore Container Traffic Analysis

[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Stack](https://img.shields.io/badge/Analytics-R%20%7C%20Tidyverse%20%7C%20Time--Series-seagreen)](#)
[![Data Source](https://img.shields.io/badge/Data-data.gov.sg-red.svg)](https://data.gov.sg)

Created: 2026-07-19

Updated: 2026-07-31

Data source:
  - [`Open source data from Maritime Port Authority (MPA) data.gov.sg`](https://data.gov.sg/)
  
An end-to-end data analytics and ETL pipeline evaluating multi-decade monthly container throughput and cargo density metrics for the Port of Singapore.

## Background and Insights

As a primary node in global supply chains and one of the world's busiest container transshipment hubs, the port volume through Singapore serves as a barometer for international trade health. This project extracts, normalizes, and analyzes historical maritime dataset series directly from the **Maritime and Port Authority of Singapore (MPA)** open APIs.

### Key Insights
* **Long-Term Trajectory:** Consistent upward volume expansion tracking global containerization, interrupted with acute macroeconomic shocks (2008 Global Financial Crisis, 2020 COVID-19 supply chain disruptions, and recent Red Sea rerouting surges).
* **Structural Seasonality:** Recurrent throughput drop every **February**, driven by factory shutdowns across East Asia during the Lunar New Year.
* **Freight Density Evolution:** Multi-decade decline in average cargo weight per Twenty-Foot Equivalent Unit (TEU), illustrating changes in global shipping stowage factors, types of goods carried, and empty container repositioning cycles.

## Singapore Port Container Cumulative Throughput
![](https://github.com/weiyuet/singapore-container-traffic/blob/main/figures/container-traffic-cumulative-throughput.png)

## Singapore Port Container Throughput Distributions by Month
![](https://github.com/weiyuet/singapore-container-traffic/blob/main/figures/container-traffic-seasonal-effects.png)

## Historical Container Density Trends
![](https://github.com/weiyuet/singapore-container-traffic/blob/main/figures/container-traffic-container-density-trends.png)