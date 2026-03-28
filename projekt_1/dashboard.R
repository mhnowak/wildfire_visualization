if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  RSQLite, DBI, dplyr, ggplot2, scales, viridis, reactable,
  tidyr, lubridate, shiny, shinydashboard, flexdashboard,
  plotly, ggridges, stringr
)

### PLOTS AND DATA
script_dir <- dirname(sys.frame(1)$ofile %||% ".")
db_path <- file.path(script_dir, "..", "FPA_FOD_20170508.sqlite")

con <- dbConnect(RSQLite::SQLite(), db_path)

fires <- tbl(con, "Fires") |> collect()

# cause_counts <- fires %>%
#   count(OWNER_DESCR , name = "count_FOD_ID")

# print(cause_counts)


#### fires_count_by_date.R

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

#### rozmiar pożaru - przyczyna
max_fire <- quantile(fires$FIRE_SIZE, 0.85, na.rm = TRUE)
fires_filtered <- fires %>%
  filter(FIRE_SIZE <= max_fire)


fire_size_cause_plot <- ggplot(fires_filtered, aes(x = FIRE_SIZE, y = STAT_CAUSE_DESCR, fill = STAT_CAUSE_DESCR)) +
                                geom_density_ridges() +
                                theme_ridges() + 
                                theme(legend.position = "none") + 
                                labs(
                                    title = "Fire cause vs size",
                                    x = "Fire size",
                                    y = "Fire cause"
                                )


#### właściciel terenu (TODO: plot the FOD_ID count for OWNER_DESCR )
owner_counts <- fires %>%
  group_by(OWNER_DESCR) %>%
  summarise(count_FOD_ID = n(), .groups = "drop") %>%
  arrange(desc(count_FOD_ID))

print(owner_counts)

teren_owner_plot <- ggplot(owner_counts, aes(x = reorder(str_wrap(OWNER_DESCR, 10), count_FOD_ID), 
                         y = count_FOD_ID)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  
  geom_point(aes(y = count_FOD_ID), color = "darkred", size = 3) +
  
  geom_segment(aes(x = reorder(str_wrap(OWNER_DESCR, 10), count_FOD_ID),
                   xend = reorder(str_wrap(OWNER_DESCR, 10), count_FOD_ID),
                   y = 0,
                   yend = count_FOD_ID),
               color = "darkred") +
  
  coord_polar() +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8)) +
  labs(x = "", y = "FOD_ID Count", title = "Count of fires by land owner")

# print(teren_owner_plot)

#### czas trwania - rok pożaru


#### rozkład wielkości pożarów


#### przyczyna - czas trwania



### DASHBOARD

### END
dbDisconnect(con)