if (!require("pacman")) install.packages("pacman")
pacman::p_load(RSQLite, DBI, dplyr, ggplot2, scales, viridis, reactable)

con <- dbConnect(RSQLite::SQLite(), "FPA_FOD_20170508.sqlite")

fires <- tbl(con, "Fires") %>% collect()

data_summary <- fires %>%
  summarise(
    Total_Records_Sampled = n(),
    Avg_Fire_Size = mean(FIRE_SIZE, na.rm = TRUE),
    Earliest_Year = min(FIRE_YEAR),
    Latest_Year = max(FIRE_YEAR),
    Unique_States = n_distinct(STATE)
  )

cause_summary <- fires %>%
  group_by(STAT_CAUSE_DESCR) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))


cause_plot <- ggplot(cause_summary, aes(x = reorder(STAT_CAUSE_DESCR, Count), y = Count, fill = Count)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis() +
  labs(title = "Primary Causes of Wildfires (1992-2015)",
       x = "Cause",
       y = "Number of Fires") +
  theme_minimal()

yearly_summary <- fires %>%
  group_by(FIRE_YEAR) %>%
  summarise(Count = n()) %>%
  arrange(FIRE_YEAR)

trend_plot <- ggplot(yearly_summary, aes(x = FIRE_YEAR, y = Count)) +
  geom_line(color = "firebrick", size = 1) +
  geom_point() +
  scale_y_continuous(labels = comma) +
  labs(title = "Wildfire Frequency Over Time",
       x = "Year",
       y = "Total Incidents") +
  theme_minimal()

print("Data Summary:")
print(data_summary)
print(cause_plot)
print(trend_plot)

dbDisconnect(con)