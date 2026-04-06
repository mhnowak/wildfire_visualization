if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  RSQLite,
  DBI,
  dplyr,
  ggplot2,
  scales,
  viridis,
  reactable,
  tidyr,
  lubridate,
  shiny,
  shinydashboard,
  flexdashboard,
  plotly,
  ggridges,
  stringr,
  ggdist
)
theme(
  plot.title = element_text(
    family = "Maven Pro",
    color = "#599191",
    face = "bold",
    size = 16
  )
)
### ============= PLOTS AND DATA ===================================================
script_dir <- dirname(sys.frame(1)$ofile %||% ".")
db_path <- file.path(script_dir, "..", "FPA_FOD_20170508.sqlite")

con <- dbConnect(RSQLite::SQLite(), db_path)

fires <- tbl(con, "Fires") |> collect()

# print(head(fires$DISCOVERY_DATE))
# print(head(fires$DISCOVERY_TIME))
# print(head(fires$CONT_DATE))
# print(head(fires$CONT_TIME))

# cause_counts <- fires %>%
#   count(OWNER_DESCR , name = "count_FOD_ID")

# print(cause_counts)

### Fire duration
fires <- fires %>%
  mutate(
    DISCOVERY_DATE2 = as.Date(DISCOVERY_DATE, origin = "1970-01-01") - 2440587,
    CONT_DATE2 = as.Date(CONT_DATE, origin = "1970-01-01") - 2440587
  )

fires <- fires %>%
  mutate(
    DISCOVERY_TIME_NUM = as.numeric(DISCOVERY_TIME),
    CONT_TIME_NUM = as.numeric(CONT_TIME)
  )

fires_duration <- fires %>%
  mutate(
    DISCOVERY_DATETIME = as.POSIXct(
      paste(DISCOVERY_DATE2, sprintf("%04d", DISCOVERY_TIME_NUM)),
      format = "%Y-%m-%d %H%M"
    ),
    CONT_DATETIME = as.POSIXct(
      paste(CONT_DATE2, sprintf("%04d", CONT_TIME_NUM)),
      format = "%Y-%m-%d %H%M"
    ),
    duration_hours = as.numeric(difftime(
      CONT_DATETIME,
      DISCOVERY_DATETIME,
      units = "hours"
    ))
  ) %>%
  filter(!is.na(duration_hours), duration_hours >= 0)

# print(head(fires_duration$DISCOVERY_DATETIME))

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
  geom_histogram(binwidth = 1, fill = "#599191") +
  scale_x_continuous(
    breaks = dynamic_breaks,
    labels = dynamic_labels
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
  geom_col(fill = "#599191") +
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
  geom_col(fill = "#599191") +
  theme_minimal() +
  labs(
    title = "Fires by Year",
    x = "Year",
    y = "Number of Fires"
  )

### placeholders for other plots ------------------------------------------ !!!!

xValue <- 1:10
yValue <- cumsum(rnorm(10))
data <- data.frame(xValue, yValue)

fire_size_cause_plot_2 = ggplot(data, aes(x = xValue, y = yValue)) +
  geom_line() +
  labs(title = "TEMP fire_size_cause_plot_2")
time_size_cause_plot = ggplot(data, aes(x = xValue, y = yValue)) +
  geom_line() +
  labs(title = "TEMP time_size_cause_plot")

# ====================================================================================================================
#### rozmiar pożaru - przyczyna
max_fire <- quantile(fires$FIRE_SIZE, 0.85, na.rm = TRUE)
fires_filtered <- fires %>%
  filter(FIRE_SIZE <= max_fire)


fire_size_cause_plot <- ggplot(
  fires_filtered,
  aes(x = FIRE_SIZE, y = STAT_CAUSE_DESCR, fill = STAT_CAUSE_DESCR)
) +
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

# print(owner_counts)

teren_owner_plot <- ggplot(
  owner_counts,
  aes(x = reorder(str_wrap(OWNER_DESCR, 10), count_FOD_ID), y = count_FOD_ID)
) +
  geom_col(fill = "#599191", alpha = 0.8) +

  geom_point(aes(y = count_FOD_ID), color = "#599191", size = 3) +

  geom_segment(
    aes(
      x = reorder(str_wrap(OWNER_DESCR, 10), count_FOD_ID),
      xend = reorder(str_wrap(OWNER_DESCR, 10), count_FOD_ID),
      y = 0,
      yend = count_FOD_ID
    ),
    color = "#599191"
  ) +

  coord_polar() +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8)) +
  labs(x = "", y = "FOD_ID Count", title = "Count of fires by land owner")

# print(teren_owner_plot)

#### czas trwania - rok pożaru

duration_year_summary <- fires_duration %>%
  group_by(FIRE_YEAR) %>%
  summarise(
    mean_duration = mean(duration_hours),
    sd_duration = sd(duration_hours),
    .groups = "drop"
  )

duration_year_plot <- ggplot(
  duration_year_summary,
  aes(x = FIRE_YEAR, y = mean_duration)
) +
  geom_line(color = "#599191") +
  geom_ribbon(
    aes(
      ymin = 0,
      ymax = mean_duration + sd_duration
    ),
    alpha = 0.2,
    fill = "#599191"
  ) +
  theme_minimal() +
  expand_limits(y = 0) +
  labs(
    title = "Fire Duration Over Years",
    x = "Year",
    y = "Duration (hours)"
  )

#### rozkład trwania pożarów

max_fire_h <- quantile(fires_duration$duration_hours, 0.85, na.rm = TRUE)
fires_filtered_h <- fires_duration %>% filter(duration_hours <= max_fire_h)

duration_distribution_plot <- ggplot(
  fires_filtered_h,
  aes(x = duration_hours)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 50,
    fill = "#599191",
    alpha = 0.7
  ) +
  geom_density(color = "#599191", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Distribution of Fire Duration",
    x = "Duration (hours)",
    y = "Density"
  )

#### rozkład wielkości pożarów
size_distribution_plot <- ggplot(fires_filtered, aes(x = FIRE_SIZE)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 50,
    fill = "#599191",
    alpha = 0.7
  ) +
  geom_density(color = "#599191", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Distribution of Fire Size",
    x = "Size",
    y = "Density"
  )


#### przyczyna - czas trwania
fires_duration_small <- fires_duration %>%
  filter(duration_hours < quantile(duration_hours, 0.85))

cause_duration_plot <- ggplot(
  fires_duration_small,
  aes(x = STAT_CAUSE_DESCR, y = duration_hours)
) +
  ggdist::stat_halfeye(
    adjust = 0.5,
    width = 0.6,
    .width = c(0.5, 1)
  ) +
  ggdist::stat_dots(
    side = "left",
    dotsize = 0.4,
    justification = 1.1
  ) +
  theme_minimal() +
  labs(
    title = "Fire Duration by Cause",
    x = "Cause",
    y = "Duration (hours)"
  )


#### Size vs Duration relation
fires_bubble <- fires_duration %>%
  filter(FIRE_SIZE > 0, duration_hours > 0)

### END
dbDisconnect(con)

### ============= DASHBOARD =====================================================================================
ui <- dashboardPage(
  dashboardHeader(title = "Wildfire Dashboard"),

  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "main_page", icon = icon("home")),
      menuItem(
        "Project 1",
        tabName = "project1",
        icon = icon("chart-line"),
        menuSubItem("Fire duration analisys", tabName = "subitemP1"),
        menuSubItem("Fire size analisys", tabName = "subitemP2"),
        menuSubItem("Number of occurrences", tabName = "subitemP3"),
        menuSubItem("Custom plot", tabName = "subitemP4")
      ),
      menuItem("Project 2", tabName = "project2", icon = icon("chart-bar")),
      menuItem("Project 3", tabName = "project3", icon = icon("fire"))
    )
  ),

  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    conditionalPanel(
      condition = "input.tabs == 'main_page'",
      fluidRow(
        box(
          width = 12,
          status = "primary",
          solidHeader = FALSE,
          h1(
            "Wizualizacje Danych Projects",
            align = "center",
            class = "main-title"
          ),
          br(),
          h3("By:", align = "center"),
          h4("Aleksandra Krasicka 512751", align = "center"),
          h4("Michał Nowak 473622", align = "center"),
          br()
        )
      )
    ),
    tabItems(
      # ==================== PROJECT 1
      tabItem(
        tabName = "subitemP1",
        fluidRow(
          tabBox(
            width = 12,
            tabPanel(
              "General Analysis",
              fluidRow(
                box(plotOutput("duration_year_plot"), width = 6),
                box(plotOutput("duration_distribution_plot"), width = 6)
              ),
              fluidRow(
                box(
                  title = "Select fire causes",
                  width = 4,
                  checkboxGroupInput(
                    inputId = "selected_causes",
                    label = NULL,
                    choices = unique(fires$STAT_CAUSE_DESCR),
                    selected = NULL
                  )
                ),
                box(
                  title = "Fire Duration by Cause",
                  width = 8,
                  plotOutput("cause_duration_plot")
                )
              )
            ),
            tabPanel(
              "Size vs Duration Relation",
              fluidRow(
                box(
                  title = "Filters",
                  width = 3,
                  checkboxGroupInput(
                    inputId = "bubble_causes",
                    label = "Select Causes:",
                    choices = unique(fires$STAT_CAUSE_DESCR),
                    selected = unique(fires$STAT_CAUSE_DESCR)
                  ),
                  sliderInput(
                    inputId = "bubble_years",
                    label = "Select Timeline (Years):",
                    min = min(fires$FIRE_YEAR, na.rm = TRUE),
                    max = max(fires$FIRE_YEAR, na.rm = TRUE),
                    value = c(
                      min(fires$FIRE_YEAR, na.rm = TRUE),
                      max(fires$FIRE_YEAR, na.rm = TRUE)
                    ),
                    step = 1,
                    sep = ""
                  )
                ),
                box(
                  plotOutput("size_duration_bubble_plot", height = "600px"),
                  width = 9
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "subitemP2",
        fluidRow(
          tabBox(
            width = 12,

            tabPanel("Size vs Cause", plotOutput("fire_size_cause_plot")),
            tabPanel("Size Distribution", plotOutput("size_distribution_plot")),
            tabPanel(
              "Terrain owner Distribution",
              plotOutput("teren_owner_plot")
            )
          )
        )
      ),
      tabItem(
        tabName = "subitemP3",

        tabBox(
          width = 12,

          tabPanel("By Day", plotOutput("doy_plot")),

          tabPanel("By Month", plotOutput("month_plot")),

          tabPanel("By Year", plotOutput("year_plot"))
        )
      ),
      tabItem(
        tabName = "subitemP4",
        box(plotOutput("time_size_cause_plot"), width = 6)
      ),

      # ==================== Project 2
      tabItem(
        tabName = "project2",
        fluidRow(
          tabBox(
            width = 12,

            tabPanel("project2", h3("Project 2 content goes here"))
          )
        )
      ),

      # ==================== Project 3
      tabItem(
        tabName = "project3",
        fluidRow(
          tabBox(
            width = 12,

            tabPanel("project3", h3("Project 3 content goes here"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$doy_plot <- renderPlot({
    doy_plot
  })

  output$month_plot <- renderPlot({
    month_plot
  })

  output$year_plot <- renderPlot({
    year_plot
  })

  output$fire_size_cause_plot <- renderPlot({
    fire_size_cause_plot
  })

  output$teren_owner_plot <- renderPlot({
    teren_owner_plot
  })

  output$duration_year_plot <- renderPlot({
    duration_year_plot
  })

  output$duration_distribution_plot <- renderPlot({
    duration_distribution_plot
  })

  output$cause_duration_plot <- renderPlot({
    req(input$selected_causes)

    filtered_data <- fires_duration_small %>%
      filter(STAT_CAUSE_DESCR %in% input$selected_causes)

    ggplot(
      filtered_data,
      aes(x = STAT_CAUSE_DESCR, y = duration_hours)
    ) +
      ggdist::stat_halfeye(
        adjust = 0.5,
        width = 0.6,
        .width = c(0.5, 1)
      ) +
      ggdist::stat_dots(
        side = "left",
        dotsize = 0.4,
        justification = 1.1
      ) +
      theme_minimal() +
      labs(
        title = "Fire Duration by Cause",
        x = "Cause",
        y = "Duration (hours)"
      )
  })

  output$size_distribution_plot <- renderPlot({
    size_distribution_plot
  })

  output$time_size_cause_plot <- renderPlot({
    time_size_cause_plot
  })
  output$size_duration_bubble_plot <- renderPlot({
    req(input$bubble_causes, input$bubble_years)

    filtered_bubble_data <- fires_bubble %>%
      filter(
        STAT_CAUSE_DESCR %in% input$bubble_causes,
        FIRE_YEAR >= input$bubble_years[1],
        FIRE_YEAR <= input$bubble_years[2]
      )

    ggplot(
      filtered_bubble_data,
      aes(
        x = duration_hours,
        y = FIRE_SIZE,
        color = STAT_CAUSE_DESCR,
        size = FIRE_SIZE
      )
    ) +
      geom_point(alpha = 0.5, stroke = 0) +
      scale_x_log10(labels = scales::comma) +
      scale_y_log10(labels = scales::comma) +
      scale_size_continuous(range = c(2, 12), guide = "none") +
      theme_minimal() +
      labs(
        title = sprintf(
          "Fire Size vs Duration by Cause (%d - %d)",
          input$bubble_years[1],
          input$bubble_years[2]
        ),
        x = "Duration (hours, log scale)",
        y = "Fire Size (log scale)",
        color = "Fire Cause"
      ) +
      theme(legend.position = "right")
  })
}

shinyApp(ui, server)
