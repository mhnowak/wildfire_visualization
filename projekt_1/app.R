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
  ggdist,
  shinycssloaders,
  maps
)

### ============= PLOTS AND DATA ===================================================
script_dir <- dirname(sys.frame(1)$ofile %||% ".")
db_path <- file.path(script_dir, "..", "FPA_FOD_20170508.sqlite")

con <- dbConnect(RSQLite::SQLite(), db_path)

fires <- tbl(con, "Fires") %>%
  select(
    FIRE_YEAR,
    DISCOVERY_DOY,
    FIRE_SIZE,
    STAT_CAUSE_DESCR,
    OWNER_DESCR,
    DISCOVERY_DATE,
    CONT_DATE,
    DISCOVERY_TIME,
    CONT_TIME,
    LATITUDE,
    LONGITUDE,
    STATE
  ) %>%
  collect()

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

max_fire <- quantile(fires$FIRE_SIZE, 0.85, na.rm = TRUE)
max_duration <- quantile(fires_duration$duration_hours, 0.85, na.rm = TRUE)

us_map <- map_data("state")

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

fire_size_cause_plot_2 <- ggplot(data, aes(x = xValue, y = yValue)) +
  geom_line() +
  labs(title = "TEMP fire_size_cause_plot_2")
time_size_cause_plot <- ggplot(data, aes(x = xValue, y = yValue)) +
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


#### właściciel terenu
owner_counts <- fires %>%
  group_by(OWNER_DESCR) %>%
  summarise(count_FOD_ID = n(), .groups = "drop") %>%
  arrange(desc(count_FOD_ID))


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


#### czas trwania - rok pożaru

duration_year_summary <- fires_duration %>%
  group_by(FIRE_YEAR) %>%
  summarise(
    mean_duration = mean(duration_hours),
    sd_duration = sd(duration_hours),
    .groups = "drop"
  )

duration_year_plot <- plot_ly(
  data = duration_year_summary
) %>%
  add_lines(
    x = ~FIRE_YEAR,
    y = ~mean_duration,
    name = "Mean Duration",
    line = list(color = "#599191"),
    hovertemplate = paste(
      "Year: %{x}<br>",
      "Mean Duration: %{y:.2f} h<br>"
    )
  ) %>%
  add_ribbons(
    x = ~FIRE_YEAR,
    ymin = 0,
    ymax = ~mean_duration + sd_duration,
    name = "SD Range",
    fillcolor = "rgba(89,145,145,0.2)",
    line = list(color = "transparent"),
    hovertemplate = paste(
      "Year: %{x}<br>",
      "Upper Bound: %{y:.2f} h<br>"
    )
  ) %>%
  layout(
    title = "Fire Duration Over Years",
    showlegend = FALSE,
    xaxis = list(title = "Year"),
    yaxis = list(
      title = "Duration (hours)",
      rangemode = "tozero"
    )
  )

#### rozkład trwania pożarów

max_fire_h <- quantile(fires_duration$duration_hours, 0.85, na.rm = TRUE)
fires_filtered_h <- fires_duration %>% filter(duration_hours <= max_fire_h)

dens <- density(fires_filtered_h$duration_hours, na.rm = TRUE)

duration_distribution_plot <- plot_ly() %>%
  add_histogram(
    data = fires_filtered_h,
    x = ~duration_hours,
    histnorm = "probability density",
    nbinsx = 50,
    marker = list(color = "#599191"),
    opacity = 0.7,
    name = "Histogram"
  ) %>%
  add_lines(
    x = dens$x,
    y = dens$y,
    line = list(color = "#1f3a3a", width = 2),
    name = "Density"
  ) %>%
  layout(
    title = "Distribution of Fire Duration",
    showlegend = FALSE,
    xaxis = list(title = "Duration (hours)"),
    yaxis = list(title = "Density")
  )

#### rozkład wielkości pożarów
size_distribution_plot <- ggplot(
  fires_filtered,
  aes(x = FIRE_SIZE)
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
    title = "Distribution of Fire Size",
    x = "Size",
    y = "Density"
  )

size_distribution_plot <- ggplotly(size_distribution_plot, tooltip = "x")


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

### Map data ---------------------------------------------------------------
# Lightweight query: only columns needed for maps
fires_geo <- dbGetQuery(
  con,
  "SELECT FIRE_YEAR, STATE, STAT_CAUSE_DESCR, LATITUDE, LONGITUDE
   FROM Fires
   WHERE LATITUDE IS NOT NULL AND LONGITUDE IS NOT NULL"
)

# State name lookup (maps package uses lower-case full names)
state_abb_to_name <- tibble(
  STATE = state.abb,
  region = tolower(state.name)
)

heatmap_data <- fires_geo %>%
  filter(
    !is.na(LATITUDE),
    !is.na(LONGITUDE),
    LATITUDE >= 24, LATITUDE <= 50,
    LONGITUDE >= -125, LONGITUDE <= -66
  ) %>%
  mutate(
    lon_bin = round(LONGITUDE, 2),
    lat_bin = round(LATITUDE, 2)
  ) %>%
  count(lat_bin, lon_bin)

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
        "Project 2",
        tabName = "project1",
        icon = icon("fire"),
        menuSubItem("Fire duration analisys", tabName = "subitemP1"),
        menuSubItem("Fire size analisys", tabName = "subitemP2"),
        menuSubItem("Number of occurrences", tabName = "subitemP3"),
        menuSubItem("Maps 1", tabName = "subitemP4"),
        menuSubItem("Maps 2", tabName = "subitemP5")
      )
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
      # ==================== PROJECT 2
      tabItem(
        tabName = "subitemP1",
        fluidRow(
          tabBox(
            width = 12,
            tabPanel(
              "General Analysis",
              fluidRow(
                box(plotlyOutput("duration_year_plot"), width = 6),
                box(plotlyOutput("duration_distribution_plot"), width = 6)
              ),
              fluidRow(
                box(
                  title = "Select fire causes",
                  width = 4,
                  actionButton("duration_toggle_causes", "Select All",
                                class = "btn-sm btn-default",
                                style = "margin-bottom: 6px; width: 100%;"),
                  checkboxGroupInput(
                    inputId = "selected_causes",
                    label = NULL,
                    choices = unique(fires$STAT_CAUSE_DESCR),
                    selected = unique(fires$STAT_CAUSE_DESCR)[1]
                   ),
                   selectInput(
                     inputId = "duration_plot_type",
                     label = "Plot Type:",
                     choices = c(
                       "Raincloud" = "raincloud",
                       "Boxplot"   = "boxplot",
                       "Violin"    = "violin",
                       "Ridgeline" = "ridgeline"
                     ),
                     selected = "raincloud"
                   ),
                   actionButton("duration_apply", "Apply Filters",
                                class = "btn-primary",
                                style = "margin-top: 10px; width: 100%;")
                ),
                box(
                  title = "Fire Duration by Cause",
                  width = 8,
                  withSpinner(plotOutput("cause_duration_plot"),
                              type = 8, color = "#599191")
                )
              )
            ),
            tabPanel(
              "Size vs Duration Relation",
              fluidRow(
                box(
                  title = "Filters",
                  width = 3,
                  radioButtons(
                    inputId = "bubble_mode",
                    label = "Plot Mode:",
                    choices = c(
                      "All points" = "all",
                      "General" = "general"
                    ),
                    selected = "general"
                  ),
                  radioButtons(
                    inputId = "bubble_scale",
                    label = "Axis Scale:",
                    choices = c(
                      "Log" = "log",
                      "Normal" = "linear"
                    ),
                    selected = "log"
                  ),
                  tags$label("Select Causes:"),
                  actionButton("bubble_toggle_causes", "Deselect All",
                               class = "btn-sm btn-warning",
                               style = "margin-bottom: 6px; width: 100%;"),
                  checkboxGroupInput(
                    inputId = "bubble_causes",
                    label = NULL,
                    choices = unique(fires$STAT_CAUSE_DESCR),
                    selected = unique(fires$STAT_CAUSE_DESCR)[1]
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
                  ),
                  actionButton("bubble_apply", "Apply Filters",
                              class = "btn-primary",
                              style = "margin-top: 10px; width: 100%;")
                ),
                box(
                  withSpinner(plotlyOutput("size_duration_bubble_plot", height = "600px"),
                              type = 8, color = "#599191"),
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
            tabPanel(
              "Size vs Cause",
              fluidRow(
                box(
                  title = "Filters",
                  width = 3,
                  radioButtons(
                    inputId = "size_cause_scale",
                    label = "X-axis Scale:",
                    choices = c("Normal" = "linear", "Log" = "log"),
                    selected = "linear",
                    inline = TRUE
                  ),
                  sliderInput(
                    inputId = "size_cause_years",
                    label = "Select Timeline (Years):",
                    min = min(fires$FIRE_YEAR, na.rm = TRUE),
                    max = max(fires$FIRE_YEAR, na.rm = TRUE),
                    value = c(min(fires$FIRE_YEAR, na.rm = TRUE),
                              max(fires$FIRE_YEAR, na.rm = TRUE)),
                    step = 1,
                    sep = ""
                  ),
                  tags$label("Select Causes:"),
                  actionButton(
                    "size_cause_toggle_causes", "Deselect All",
                    class = "btn-sm btn-warning",
                    style = "margin-bottom: 6px; width: 100%;"
                  ),
                  checkboxGroupInput(
                    inputId = "size_cause_causes",
                    label = NULL,
                    choices = unique(fires$STAT_CAUSE_DESCR),
                    selected = unique(fires$STAT_CAUSE_DESCR)
                  ),
                  actionButton(
                    "size_cause_apply", "Apply Filters",
                    class = "btn-primary",
                    style = "margin-top: 10px; width: 100%;"
                  )
                ),
                box(
                  width = 9,
                  withSpinner(plotOutput("fire_size_cause_plot"),
                              type = 8, color = "#599191")
                )
              )
            ),
            tabPanel("Size Distribution", plotlyOutput("size_distribution_plot")),
            tabPanel(
              "Terrain owner Distribution",
              fluidRow(
                box(
                  title = "Filters",
                  width = 3,
                  tags$label("Select Owner Types:"),
                  actionButton(
                    "owner_toggle", "Deselect All",
                    class = "btn-sm btn-warning",
                    style = "margin-bottom: 6px; width: 100%;"
                  ),
                  checkboxGroupInput(
                    inputId = "owner_types",
                    label = NULL,
                    choices = unique(fires$OWNER_DESCR),
                    selected = unique(fires$OWNER_DESCR)
                  ),
                  actionButton(
                    "owner_apply", "Apply Filters",
                    class = "btn-primary",
                    style = "margin-top: 10px; width: 100%;"
                  )
                ),
                box(
                  width = 9,
                  withSpinner(plotOutput("teren_owner_plot", height = "75vh"),
                              type = 8, color = "#599191"),
                
                  br(),

                  tableOutput("owner_table")
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "subitemP3",
        tabBox(
          width = 12,
          tabPanel(
              "By Day",
              fluidRow(
                box(
                  title = "Filters",
                  width = 3,
                  sliderInput(
                    inputId = "doy_years",
                    label = "Select Timeline (Years):",
                    min = min(fires$FIRE_YEAR, na.rm = TRUE),
                    max = max(fires$FIRE_YEAR, na.rm = TRUE),
                    value = c(min(fires$FIRE_YEAR, na.rm = TRUE),
                              max(fires$FIRE_YEAR, na.rm = TRUE)),
                    step = 1,
                    sep = ""
                  ),
                  actionButton(
                    "doy_apply", "Apply Filters",
                    class = "btn-primary",
                    style = "margin-top: 10px; width: 100%;"
                  )
                ),
                box(
                  width = 9,
                  withSpinner(plotOutput("doy_plot"),
                              type = 8, color = "#599191")
                )
              )
            ),
            tabPanel(
              "By Month",
              fluidRow(
                box(
                  title = "Filters",
                  width = 3,
                  sliderInput(
                    inputId = "month_years",
                    label = "Select Timeline (Years):",
                    min = min(fires$FIRE_YEAR, na.rm = TRUE),
                    max = max(fires$FIRE_YEAR, na.rm = TRUE),
                    value = c(min(fires$FIRE_YEAR, na.rm = TRUE),
                              max(fires$FIRE_YEAR, na.rm = TRUE)),
                    step = 1,
                    sep = ""
                  ),
                  actionButton(
                    "month_apply", "Apply Filters",
                    class = "btn-primary",
                    style = "margin-top: 10px; width: 100%;"
                  )
                ),
                box(
                  width = 9,
                  withSpinner(plotOutput("month_plot"),
                              type = 8, color = "#599191")
                )
              )
            ),
          tabPanel("By Year", plotOutput("year_plot"))
        )
      ),
      tabItem(
        tabName = "subitemP4",
        fluidRow(
          box(
            title = "Filters",
            width = 3,
            status = "primary",
            sliderInput(
              inputId = "state_map_years",
              label = "Select Timeline (Years):",
              min = min(fires$FIRE_YEAR, na.rm = TRUE),
              max = max(fires$FIRE_YEAR, na.rm = TRUE),
              value = c(min(fires$FIRE_YEAR, na.rm = TRUE),
                        max(fires$FIRE_YEAR, na.rm = TRUE)),
              step = 1,
              sep = ""
            ),
            tags$label("Select Causes:"),
            actionButton(
              "state_map_toggle_causes", "Deselect All",
              class = "btn-sm btn-warning",
              style = "margin-bottom: 6px; width: 100%;"
            ),
            checkboxGroupInput(
              inputId = "state_map_causes",
              label = NULL,
              choices = unique(fires$STAT_CAUSE_DESCR),
              selected = unique(fires$STAT_CAUSE_DESCR)
            ),
            actionButton(
              "state_map_apply", "Apply Filters",
              class = "btn-primary",
              style = "margin-top: 10px; width: 100%;"
            )
          ),
          box(
            title = "Wildfire Occurrences by State",
            width = 9,
            status = "primary",
            withSpinner(plotlyOutput("state_map", height = "600px"),
                        type = 8, color = "#599191")
          )
        )
      ),
      tabItem(
        tabName = "subitemP5",
        fluidRow(
          # box(
          #   title = "Filters",
          #   width = 3,
          #   status = "primary",
          #   sliderInput(
          #     inputId = "heatmap_years",
          #     label = "Select Timeline (Years):",
          #     min = min(fires$FIRE_YEAR, na.rm = TRUE),
          #     max = max(fires$FIRE_YEAR, na.rm = TRUE),
          #     value = c(min(fires$FIRE_YEAR, na.rm = TRUE),
          #               max(fires$FIRE_YEAR, na.rm = TRUE)),
          #     step = 1,
          #     sep = ""
          #   ),
          #   tags$label("Select Causes:"),
          #   actionButton(
          #     "heatmap_toggle_causes", "Deselect All",
          #     class = "btn-sm btn-warning",
          #     style = "margin-bottom: 6px; width: 100%;"
          #   ),
          #   checkboxGroupInput(
          #     inputId = "heatmap_causes",
          #     label = NULL,
          #     choices = unique(fires$STAT_CAUSE_DESCR),
          #     selected = unique(fires$STAT_CAUSE_DESCR)
          #   ),
          #   actionButton(
          #     "heatmap_apply", "Apply Filters",
          #     class = "btn-primary",
          #     style = "margin-top: 10px; width: 100%;"
          #   )
          # ),
          box(
            title = "Wildfire Heatmap — Continental US",
            width = 9,
            status = "primary",
            withSpinner(plotlyOutput("us_heatmap", height = "600px"),
                        type = 8, color = "#599191")
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  doy_data <- eventReactive(input$doy_apply, {
    req(input$doy_years)
    fires %>% filter(FIRE_YEAR >= input$doy_years[1], FIRE_YEAR <= input$doy_years[2])
  }, ignoreNULL = FALSE)

  output$doy_plot <- renderPlot({
    df <- doy_data()
    ggplot(df, aes(x = DISCOVERY_DOY)) +
      geom_histogram(binwidth = 1, fill = "#599191") +
      scale_x_continuous(breaks = dynamic_breaks, labels = dynamic_labels) +
      theme_minimal() +
      labs(title = "Fires by Day of Year",
           x = "Day of Year", y = "Number of Fires")
  })

  month_data <- eventReactive(input$month_apply, {
    req(input$month_years)
    fires %>%
      filter(FIRE_YEAR >= input$month_years[1], FIRE_YEAR <= input$month_years[2]) %>%
      mutate(
        temp_date = as.Date(paste(FIRE_YEAR, DISCOVERY_DOY), format = "%Y %j"),
        Month_Num = month(temp_date)
      ) %>%
      group_by(Month_Num) %>%
      summarise(Count = n())
  }, ignoreNULL = FALSE)

  output$month_plot <- renderPlot({
    ms <- month_data()
    ggplot(ms, aes(x = factor(Month_Num), y = Count)) +
      geom_col(fill = "#599191") +
      scale_x_discrete(labels = month.abb) +
      theme_minimal() +
      labs(title = "Fires by Month", x = "Month", y = "Number of Fires")
  }) %>%
  bindCache(input$month_years)

  output$year_plot <- renderPlot({
    year_plot
  })

  size_cause_filtered <- eventReactive(input$size_cause_apply, {
    req(input$size_cause_years, input$size_cause_causes)
    list(
      data = fires %>%
        filter(
          FIRE_YEAR >= input$size_cause_years[1],
          FIRE_YEAR <= input$size_cause_years[2],
          STAT_CAUSE_DESCR %in% input$size_cause_causes,
          FIRE_SIZE <= quantile(fires$FIRE_SIZE, 0.85, na.rm = TRUE)
        ),
      scale = input$size_cause_scale
    )
  }, ignoreNULL = FALSE)

  observeEvent(input$size_cause_toggle_causes, {
    all_causes <- unique(fires$STAT_CAUSE_DESCR)
    if (length(input$size_cause_causes) == length(all_causes)) {
      updateCheckboxGroupInput(session, "size_cause_causes", selected = character(0))
      updateActionButton(session, "size_cause_toggle_causes", label = "Select All")
    } else {
      updateCheckboxGroupInput(session, "size_cause_causes", selected = all_causes)
      updateActionButton(session, "size_cause_toggle_causes", label = "Deselect All")
    }
  })

  output$fire_size_cause_plot <- renderPlot({
    res <- size_cause_filtered()
    df <- res$data

    p <- ggplot(df, aes(x = FIRE_SIZE, y = STAT_CAUSE_DESCR,
                        fill = STAT_CAUSE_DESCR)) +
    geom_density_ridges() +
    theme_ridges() +
    theme(legend.position = "none") +
    labs(
      title = "Fire cause vs size",
      x = "Fire size",
      y = "Fire cause"
    )

    if (res$scale == "log") {
      p <- p + scale_x_log10()
    }

    p
  })

  observeEvent(input$owner_toggle, {
    all_owners <- unique(fires$OWNER_DESCR)
    if (length(input$owner_types) == length(all_owners)) {
      updateCheckboxGroupInput(session, "owner_types", selected = character(0))
      updateActionButton(session, "owner_toggle", label = "Select All")
    } else {
      updateCheckboxGroupInput(session, "owner_types", selected = all_owners)
      updateActionButton(session, "owner_toggle", label = "Deselect All")
    }
  })

  owner_filtered <- eventReactive(input$owner_apply, {
    req(input$owner_types)
    fires %>%
      filter(OWNER_DESCR %in% input$owner_types) %>%
      group_by(OWNER_DESCR) %>%
      summarise(count_FOD_ID = n(), .groups = "drop") %>%
      arrange(desc(count_FOD_ID))
  }, ignoreNULL = FALSE)

  output$teren_owner_plot <- renderPlot({
    owner_counts <- owner_filtered()
    ggplot(
      owner_counts,
      aes(x = reorder(str_wrap(OWNER_DESCR, 10), count_FOD_ID),
          y = count_FOD_ID)
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
  })

  output$duration_year_plot <- renderPlotly({
    duration_year_plot
  })

  output$duration_distribution_plot <- renderPlotly({
    duration_distribution_plot
  })

  duration_filtered <- eventReactive(input$duration_apply,
    {
      causes <- if (is.null(input$selected_causes) || length(input$selected_causes) == 0) {
        unique(fires$STAT_CAUSE_DESCR)[1]
      } else {
        input$selected_causes
      }
      list(
        data = fires_duration_small %>% filter(STAT_CAUSE_DESCR %in% causes),
        plot_type = input$duration_plot_type
      )
    },
    ignoreNULL = FALSE
  )

  output$cause_duration_plot <- renderPlot({
    result        <- duration_filtered()
    filtered_data <- result$data
    plot_type     <- result$plot_type

    base <- ggplot(filtered_data, aes(x = STAT_CAUSE_DESCR, y = duration_hours))

    p <- if (plot_type == "raincloud") {
      base +
        ggdist::stat_halfeye(adjust = 0.5, width = 0.6, .width = c(0.5, 1)) +
        ggdist::stat_dots(side = "left", dotsize = 0.4, justification = 1.1)
    } else if (plot_type == "boxplot") {
      base +
        geom_boxplot(fill = "#599191", alpha = 0.6, outlier.size = 0.8)
    } else if (plot_type == "violin") {
      base +
        geom_violin(fill = "#599191", alpha = 0.6, trim = FALSE) +
        geom_boxplot(width = 0.08, fill = "white", outlier.size = 0.6)
    } else {
      ggplot(filtered_data, aes(x = duration_hours,
                                y = STAT_CAUSE_DESCR,
                                fill = STAT_CAUSE_DESCR)) +
        ggridges::geom_density_ridges(alpha = 0.7, show.legend = FALSE) +
        theme_minimal() +
        labs(title = "Fire Duration by Cause",
             x = "Duration (hours)",
             y = "Cause") +
        theme(legend.position = "none")
    }

    if (plot_type != "ridgeline") {
      p <- p +
        theme_minimal() +
        labs(title = "Fire Duration by Cause",
             x = "Cause",
             y = "Duration (hours)")
    }

    p
  })

  owner_table_data <- reactive({
    df <- owner_filtered()

    total_fires <- sum(df$count_FOD_ID, na.rm = TRUE)

    df %>%
      mutate(
        percent = round(100 * count_FOD_ID / total_fires, 2)
      ) %>%
      arrange(desc(count_FOD_ID))
  })

  output$size_distribution_plot <- renderPlotly({
    size_distribution_plot
  })

  output$owner_table <- renderTable({
    owner_table_data() %>%
      select(
        'Owner' = OWNER_DESCR,
        'Total Fires' = count_FOD_ID,
        'Share (%)' = percent
      )
  }, striped = TRUE, hover = TRUE, spacing = "s")

  output$time_size_cause_plot <- renderPlot({
    time_size_cause_plot
  })

  observeEvent(input$duration_toggle_causes, {
    all_causes <- unique(fires$STAT_CAUSE_DESCR)
    if (length(input$selected_causes) == length(all_causes)) {
      updateCheckboxGroupInput(session, "selected_causes", selected = character(0))
      updateActionButton(session, "duration_toggle_causes", label = "Select All")
    } else {
      updateCheckboxGroupInput(session, "selected_causes", selected = all_causes)
      updateActionButton(session, "duration_toggle_causes", label = "Deselect All")
    }
  })

  observeEvent(input$bubble_toggle_causes, {
    all_causes <- unique(fires$STAT_CAUSE_DESCR)
    if (length(input$bubble_causes) == length(all_causes)) {
      updateCheckboxGroupInput(session, "bubble_causes", selected = character(0))
      updateActionButton(session, "bubble_toggle_causes", label = "Select All")
    } else {
      updateCheckboxGroupInput(session, "bubble_causes", selected = all_causes)
      updateActionButton(session, "bubble_toggle_causes", label = "Deselect All")
    }
  })

  bubble_filtered <- eventReactive(input$bubble_apply,
  {
    req(input$bubble_causes, input$bubble_years)

    df <- fires_bubble %>%
      filter(
        STAT_CAUSE_DESCR %in% input$bubble_causes,
        FIRE_YEAR >= input$bubble_years[1],
        FIRE_YEAR <= input$bubble_years[2]
      )

    list(
      data = df,
      mode = input$bubble_mode,
      scale = input$bubble_scale,
      year_min = input$bubble_years[1],
      year_max = input$bubble_years[2]
    )
  }, ignoreNULL = FALSE)

  output$size_duration_bubble_plot <- renderPlotly({

    res <- bubble_filtered()
    df <- res$data
    scale_type <- ifelse(res$scale == "log", "log", "-")
    filtered_bubble_data <- bubble_filtered()$data

    if (res$mode == "all") {

      p <- plot_ly(
        data = df,
        x = ~duration_hours,
        y = ~FIRE_SIZE,
        type = "scatter",
        mode = "markers",
        color = ~STAT_CAUSE_DESCR,
        size = ~FIRE_SIZE,
        sizes = c(5, 40),
        opacity = 0.5,
        text = ~paste(
          "<b>Cause:</b>", STAT_CAUSE_DESCR,
          "<br><b>Duration:</b>", round(duration_hours, 2), " h",
          "<br><b>Fire Size:</b>", round(FIRE_SIZE, 2)
        ),
        hoverinfo = "text"
      ) %>%
        layout(
          title = paste0(
            "Fire Size vs Duration by Cause (",
            res$year_min,
            " - ",
            res$year_max,
            ")"
          ),
          xaxis = list(
            title = paste0(
              "Duration (hours",
              ifelse(res$scale == "log", ", log scale", ""),
              ")"
            ),
            type = scale_type
          ),
          yaxis = list(
            title = paste0(
              "Fire Size",
              ifelse(res$scale == "log", " (log scale)", "")
            ),
            type = scale_type
          )
        )

    } else {

      summary_df <- filtered_bubble_data %>%
        group_by(STAT_CAUSE_DESCR) %>%
        summarise(
          mean_duration = mean(duration_hours, na.rm = TRUE),
          mean_size = mean(FIRE_SIZE, na.rm = TRUE),
          sd_duration = sd(duration_hours, na.rm = TRUE) / 2,
          sd_size = sd(FIRE_SIZE, na.rm = TRUE) / 2,
          .groups = "drop"
        )

      p <- plot_ly()
      colors <- scales::hue_pal()(nrow(summary_df))

      shape_list <- list()

      for (i in seq_len(nrow(summary_df))) {

        row <- summary_df[i, ]

        shape_list[[i]] <- list(
          type = "circle",
          xref = "x",
          yref = "y",

          x0 = row$mean_duration - row$sd_duration,
          x1 = row$mean_duration + row$sd_duration,

          y0 = row$mean_size - row$sd_size,
          y1 = row$mean_size + row$sd_size,

          fillcolor = adjustcolor(colors[i], alpha.f = 0.25),
          line = list(color = colors[i]),
          opacity = 0.3
        )
      }

      p <- plot_ly(
        summary_df,
        x = ~mean_duration,
        y = ~mean_size,
        type = "scatter",
        mode = "markers",
        color = ~STAT_CAUSE_DESCR,
        colors = colors,

        marker = list(size = 14),

        text = ~paste0(
          "<b>", STAT_CAUSE_DESCR, "</b><br>",
          "Mean Duration: ", round(mean_duration, 2), " h<br>",
          "Mean Size: ", round(mean_size, 2), "<br>",
          "SD Duration/2: ", round(sd_duration, 2), "<br>",
          "SD Size/2: ", round(sd_size, 2)
        ),

        hoverinfo = "text"
      ) %>%
        layout(
          title = "General Fire Size vs Duration by Cause",

          shapes = shape_list,

          paper_bgcolor = "white",
          plot_bgcolor = "white",

          xaxis = list(
            title = ifelse(
              input$bubble_scale == "log",
              "Mean Duration (log scale)",
              "Mean Duration"
            ),
            type = ifelse(input$bubble_scale == "log", "log", "linear"),
            gridcolor = "#E5E5E5"
          ),

          yaxis = list(
            title = ifelse(
              input$bubble_scale == "log",
              "Mean Fire Size (log scale)",
              "Mean Fire Size"
            ),
            type = ifelse(input$bubble_scale == "log", "log", "linear"),
            gridcolor = "#E5E5E5"
          )
        )
    }

    p
  })
  # ==================== Project 2 — State choropleth map
  observeEvent(input$state_map_toggle_causes, {
    all_causes <- unique(fires$STAT_CAUSE_DESCR)
    if (length(input$state_map_causes) == length(all_causes)) {
      updateCheckboxGroupInput(session, "state_map_causes", selected = character(0))
      updateActionButton(session, "state_map_toggle_causes", label = "Select All")
    } else {
      updateCheckboxGroupInput(session, "state_map_causes", selected = all_causes)
      updateActionButton(session, "state_map_toggle_causes", label = "Deselect All")
    }
  })

  state_map_data <- eventReactive(input$state_map_apply, {
    req(input$state_map_years, input$state_map_causes)
    fires_geo %>%
      filter(
        FIRE_YEAR >= input$state_map_years[1],
        FIRE_YEAR <= input$state_map_years[2],
        STAT_CAUSE_DESCR %in% input$state_map_causes
      ) %>%
      group_by(STATE) %>%
      summarise(count = n(), .groups = "drop") %>%
      left_join(state_abb_to_name, by = "STATE")
  }, ignoreNULL = FALSE)

  output$state_map <- renderPlotly({
    state_counts <- state_map_data() 

    map_joined <- us_map %>%
      left_join(state_counts, by = "region")
    map_joined$count[is.na(map_joined$count)] <- 0

    p <- ggplot(map_joined, aes(
      x = long, y = lat, group = group,
      fill = count,
      text = paste0(tools::toTitleCase(region), ": ", scales::comma(count), " fires")
    )) +
      geom_polygon(color = "white", linewidth = 0.3) +
      scale_fill_gradient(
        low = "#ffffcc", high = "#800026",
        name = "# Fires",
        labels = scales::comma
      ) +
      coord_fixed(1.3) +
      theme_void() +
      labs(title = "Wildfire Occurrences by State")

    ggplotly(p, tooltip = "text") %>%
      layout(
        geo = list(scope = "usa"),
        margin = list(l = 0, r = 0, t = 40, b = 0)
      )
  })

  # ==================== Project 3 — Heatmap
  # observeEvent(input$heatmap_toggle_causes, {
  #   all_causes <- unique(fires$STAT_CAUSE_DESCR)
  #   if (length(input$heatmap_causes) == length(all_causes)) {
  #     updateCheckboxGroupInput(session, "heatmap_causes", selected = character(0))
  #     updateActionButton(session, "heatmap_toggle_causes", label = "Select All")
  #   } else {
  #     updateCheckboxGroupInput(session, "heatmap_causes", selected = all_causes)
  #     updateActionButton(session, "heatmap_toggle_causes", label = "Deselect All")
  #   }
  # })

  # heatmap_data <- eventReactive(input$heatmap_apply, {
  #   req(input$heatmap_years, input$heatmap_causes)
  #   df <- fires_geo %>%
  #     filter(
  #       FIRE_YEAR >= input$heatmap_years[1],
  #       FIRE_YEAR <= input$heatmap_years[2],
  #       STAT_CAUSE_DESCR %in% input$heatmap_causes,
  #       LATITUDE >= 24, LATITUDE <= 50,
  #       LONGITUDE >= -125, LONGITUDE <= -66
  #     )
  #   df
  # }, ignoreNULL = FALSE)

  

  output$us_heatmap <- renderPlotly({

    df <- heatmap_data
    req(nrow(df) > 0)

    plot_ly(
      df,
      type = "densitymapbox",
      lat = ~lat_bin,
      lon = ~lon_bin,
      z = ~n,
      radius = 12,
      coloraxis = "coloraxis"
    ) %>%
      layout(
        mapbox = list(
          style = "carto-positron",
          center = list(lon = -96, lat = 37),
          zoom = 3.2
        ),
        coloraxis = list(colorscale = "Hot", reversescale = TRUE),
        margin = list(l = 0, r = 0, t = 40, b = 0),
        title = "Wildfire Density Heatmap — Continental US"
      )
  })
}

shinyApp(ui, server)
