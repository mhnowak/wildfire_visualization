if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  RSQLite, DBI, dplyr, ggplot2, scales, viridis, reactable,
  tidyr, lubridate
)

con <- dbConnect(RSQLite::SQLite(), "FPA_FOD_20170508.sqlite")

fires <- tbl(con, "Fires") |> collect()

doy_summary <- fires |>
  group_by(DISCOVERY_DOY) |>
  summarise(Count = n()) |>
  arrange(DISCOVERY_DOY)

sample_dates <- as.Date(paste0("2023-", 1:12, "-01"))
dynamic_breaks <- as.numeric(format(sample_dates, "%j"))
dynamic_labels <- month.abb

doy_plot <- ggplot(
  fires,
  aes(x = DISCOVERY_DOY)
) +
  geom_histogram(binwidth = 1, fill = "orange") +
  scale_x_continuous(
    breaks = dynamic_breaks,
    labels = dynamic_labels,
  ) +
  theme_minimal() +
  labs(
    title = "Fires by Day of Year",
    x = "Day of Year",
    y = "Number of Fires"
  )

month_summary <- fires |>
  mutate(
    temp_date = as.Date(paste(FIRE_YEAR, DISCOVERY_DOY), format = "%Y %j"),
    Month_Num = month(temp_date)
  ) |>
  group_by(Month_Num) |>
  summarise(Count = n())

month_plot <- ggplot(
  month_summary,
  aes(x = factor(Month_Num), y = Count)
) +
  geom_col(fill = "orange") +
  scale_x_discrete(labels = month.abb) +
  theme_minimal() +
  labs(
    title = "Fires by Month",
    x = "Month",
    y = "Number of Fires"
  )

year_summary <- fires |>
  group_by(FIRE_YEAR) |>
  summarise(Count = n())

year_plot <- ggplot(
  year_summary,
  aes(x = factor(FIRE_YEAR), y = Count)
) +
  geom_col(fill = "orange") +
  theme_minimal() +
  labs(
    title = "Fires by Year",
    x = "Year",
    y = "Number of Fires"
  )

print(doy_plot)
print(month_plot)
print(year_plot)

dbDisconnect(con)
