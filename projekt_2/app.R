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

total_fires <- nrow(fires)

### ============= DATA PREPARATION =====================================================================================
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

doy_summary <- fires %>%
  group_by(DISCOVERY_DOY) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(
    percent = 100 * Count / sum(Count),
    tooltip = paste0(
      "Day: ", DISCOVERY_DOY,
      "<br>Count: ", Count,
      "<br>%: ", round(percent,2)
    )
  )

month_summary <- fires %>%
  mutate(
    temp_date = as.Date(paste(FIRE_YEAR, DISCOVERY_DOY), format = "%Y %j"),
    Month_Num = month(temp_date)
  ) %>%
  group_by(Month_Num) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(
    percent = 100 * Count / sum(Count),
    tooltip = paste0(
      "Month: ", month.abb[Month_Num],
      "<br>Count: ", Count,
      "<br>%: ", round(percent,2)
    )
  )

year_summary <- fires %>%
  group_by(FIRE_YEAR) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(
    percent = 100 * Count / sum(Count),
    tooltip = paste0(
      "Year: ", FIRE_YEAR,
      "<br>Count: ", Count,
      "<br>%: ", round(percent,2)
    )
  )

max_fire <- quantile(fires$FIRE_SIZE, 0.85, na.rm = TRUE)
fires_filtered <- fires %>% filter(FIRE_SIZE <= max_fire)

fires_duration_small <- fires_duration %>%
  filter(duration_hours < quantile(duration_hours, 0.85))

fires_bubble <- fires_duration %>%
  filter(FIRE_SIZE > 0, duration_hours > 0)

# =================== Plots =============================


doy_plot <- ggplot(doy_summary,aes(DISCOVERY_DOY,Count,text=tooltip))+geom_col(fill="#599191")+theme_minimal()
month_plot <- ggplot(month_summary,aes(factor(Month_Num),Count,text=tooltip))+geom_col(fill="#599191")+theme_minimal()
year_plot <- ggplot(year_summary,aes(FIRE_YEAR,Count,text=tooltip))+geom_col(fill="#599191")+theme_minimal()


### Fire duration over years ---------------------------------

duration_year_summary <- fires_duration %>%
  group_by(FIRE_YEAR) %>%
  summarise(
    mean_duration = mean(duration_hours),
    sd_duration = sd(duration_hours),
    .groups = "drop"
  ) %>%
  mutate(
    tooltip = paste0(
      "Year: ", FIRE_YEAR,
      "<br>Avg: ", round(mean_duration,2),
      "<br>SD: ", round(sd_duration,2)
    )
  )

p_duration_year <- ggplot(
  duration_year_summary,
  aes(x = FIRE_YEAR, y = mean_duration, text = tooltip)
) +
  geom_line(color = "#599191") +
  geom_point() +
  geom_ribbon(aes(ymin = 0, ymax = mean_duration + sd_duration), alpha = 0.2) +
  theme_minimal() 

### Duration distribution ---------------------------------

p_duration_dist <- plot_ly(
    data = fires_duration_small,
    x = ~duration_hours,
    type = "histogram",
    histnorm = "density",
    nbinsx = 50,
    marker = list(color = "#599191"),
    hovertemplate = paste(
      "Duration: %{x}<br>",
      "Density: %{y}<br>",
      "<extra></extra>"
    )
  ) %>%
    add_trace(
      type = "density",
      x = ~duration_hours,
      name = "Density"
    )

### Dire size vs cause ---------------------------------

p_fire_size_cause <- plot_ly(
    data = fires_filtered,
    x = ~FIRE_SIZE,
    y = ~STAT_CAUSE_DESCR,
    type = "violin",
    orientation = "h",
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    fillcolor = "#599191",
    line = list(color = "#2f6f6f"),
    opacity = 0.6,
    hovertemplate = paste(
      "Cause: %{y}<br>",
      "Size: %{x}<br>",
      "<extra></extra>"
    )
  )

### Land owner ---------------------------------

owner_counts <- fires %>%
  group_by(OWNER_DESCR) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    percent = 100 * count / sum(count),
    tooltip = paste0(
      "Owner: ", OWNER_DESCR,
      "<br>Count: ", count,
      "<br>%: ", round(percent,2)
    )
  )

owner_counts <- owner_counts %>%
  mutate(
    owner_label = str_wrap(OWNER_DESCR, 10),
    tooltip = paste0(
      "Owner: ", OWNER_DESCR,
      "<br>Count: ", count,
      "<br>%: ", round(percent, 2)
    )
  )

teren_owner_plot <- ggplot(
    owner_counts,
    aes(
      x = reorder(owner_label, count),
      y = count,
      text = tooltip
    )
  ) +
    geom_col(fill = "#599191", alpha = 0.8) +
    geom_point(color = "#599191", size = 3) +
    geom_segment(
      aes(
        xend = reorder(owner_label, count),
        y = 0,
        yend = count
      ),
      color = "#599191"
    ) +
    coord_polar() +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8)) +
    labs(x = "", y = "Fires", title = "Count of fires by land owner")



### przyczyna - czas trwania ---------------------------------
fires_bubble <- fires_bubble %>%
  group_by(STAT_CAUSE_DESCR) %>%
  mutate(num_occurrences = n()) %>%
  ungroup()

p_bubble_all <- ggplot(
  fires_bubble,
  aes(
    x = duration_hours,
    y = FIRE_SIZE,
    color = STAT_CAUSE_DESCR,
    text = paste0(
      "Cause: ", STAT_CAUSE_DESCR,
      "<br>Size: ", FIRE_SIZE,
      "<br>Duration: ", duration_hours,
      "<br>Occ: ", num_occurrences
    )
  )
) +
  geom_point(alpha = 0.4) +
  theme_minimal()

bubble_general <- fires_bubble %>%
  group_by(STAT_CAUSE_DESCR) %>%
  summarise(
    avg_size = mean(FIRE_SIZE),
    sd_size = sd(FIRE_SIZE),
    avg_duration = mean(duration_hours),
    sd_duration = sd(duration_hours),
    num_occurrences = n(),
    .groups = "drop"
  ) %>%
  mutate(
    tooltip = paste0(
      "Cause: ", STAT_CAUSE_DESCR,
      "<br>Avg size: ", round(avg_size,2), " ± ", round(sd_size,2),
      "<br>Avg duration: ", round(avg_duration,2), " ± ", round(sd_duration,2),
      "<br>Occ: ", num_occurrences
    )
  )

p_bubble_general <- ggplot(
  bubble_general,
  aes(x = avg_duration, y = avg_size, text = tooltip)
) +
  geom_point(size = 5) +
  theme_minimal()


p_size_dist <- plot_ly(
    data = fires_filtered,
    x = ~FIRE_SIZE,
    type = "histogram",
    histnorm = "density",
    nbinsx = 50,
    marker = list(color = "#599191"),
    hovertemplate = paste(
      "Size: %{x}<br>",
      "Density: %{y}<br>",
      "<extra></extra>"
    )
  ) %>%
    add_trace(
      data = fires_filtered,
      x = ~FIRE_SIZE,
      type = "density",
      name = "Density",
      line = list(color = "#2f6f6f", width = 2)
    ) %>%
    layout(
      xaxis = list(title = "Fire Size"),
      yaxis = list(title = "Density")
    )

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
        tabName = "project2",
        icon = icon("fire"),
        menuSubItem("Fire duration analisys", tabName = "subitemP1"),
        menuSubItem("Fire size analisys", tabName = "subitemP2"),
        menuSubItem("Number of occurrences", tabName = "subitemP3")
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
      # ==================== PROJECT 2 ============================
      tabItem(
        tabName = "project2",
        fluidRow(
          tabBox(
            width = 12,
            tabPanel(
              "General Analysis",
              fluidRow(
                box(plotlyOutput("duration_year_plot")),
                box(plotlyOutput("duration_dist_plot"))
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
                  plotlyOutput("cause_duration_plot")
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
                  plotlyOutput("size_duration_bubble_plot", height = "600px"),
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

            tabPanel("Size vs Cause", plotlyOutput("fire_size_cause_plot")),
            tabPanel("Size Distribution", plotlyOutput("size_dist_plot")),
            tabPanel(
              "Terrain owner Distribution",
              selectInput(
                inputId = "top_owners",
                label = "Show Top N Owners",
                choices = c(5, 10, 15, 20, 30),
                selected = 10
              ),
              plotlyOutput("owner_plot")
            )
          )
        )
      ),
      tabItem(
        tabName = "subitemP3",

        tabBox(
          width = 12,

          tabPanel("By Day", plotlyOutput("doy_plot")),

          tabPanel("By Month", plotlyOutput("month_plot")),

          tabPanel("By Year", plotlyOutput("year_plot"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$doy_plot <- renderPlotly({
      ggplotly(doy_plot, tooltip = "text")
    
  })

  output$month_plot <- renderPlotly({
      ggplotly(month_plot, tooltip = "text")
    })


  output$year_plot <- renderPlotly({
      ggplotly(year_plot, tooltip = "text")
    })


  output$fire_size_cause_plot <- renderPlotly({

    p_fire_size_cause

})

  output$owner_plot <- renderPlotly({

    n <- as.numeric(input$top_owners)

    top_data <- owner_counts %>%
      slice_head(n = n) %>%
      mutate(
        owner_label = str_wrap(OWNER_DESCR, 10)
      )

    top_data <- top_data %>%
      mutate(
        owner_label = factor(owner_label, levels = rev(owner_label))
      )

    plot_ly(
      data = top_data,
      type = "barpolar",
      r = ~count,
      theta = ~owner_label,
      text = ~paste0(
        "Owner: ", OWNER_DESCR,
        "<br>Count: ", count
      ),
      hoverinfo = "text",
      marker = list(color = "#599191", opacity = 0.85)
    ) %>%
      layout(
        title = "Count of fires by land owner (Top N)",
        polar = list(
          radialaxis = list(showticklabels = TRUE)
        )
      )
  })

  output$duration_year_plot <- renderPlotly({
    ggplotly(p_duration_year, tooltip = "text")
  })

  output$duration_dist_plot <- renderPlotly({
    ggplotly(p_duration_dist, tooltip = "text")
  })

  output$cause_duration_plot <- renderPlotly({
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

  output$size_dist_plot <- renderPlotly(
    p_size_dist
  )

  ### TODO: make this plot too (u missed it)
  output$size_duration_bubble_plot <- renderPlotly({
    req(input$bubble_causes, input$bubble_years)

    filtered_bubble_data <- fires_bubble %>%
      filter(
        STAT_CAUSE_DESCR %in% input$bubble_causes,
        FIRE_YEAR >= input$bubble_years[1],
        FIRE_YEAR <= input$bubble_years[2]
      )

    bubble_general <- filtered_bubble_data %>%
      group_by(STAT_CAUSE_DESCR) %>%
      summarise(
        avg_duration = mean(duration_hours, na.rm = TRUE),
        sd_duration = sd(duration_hours, na.rm = TRUE),
        avg_size = mean(FIRE_SIZE, na.rm = TRUE),
        sd_size = sd(FIRE_SIZE, na.rm = TRUE),
        num_occurrences = n(),
        .groups = "drop"
      )
    p <- ggplot() +
      geom_point(
        data = filtered_bubble_data,
        aes(
          x = duration_hours,
          y = FIRE_SIZE,
          color = STAT_CAUSE_DESCR,
          text = paste0(
            "Cause: ", STAT_CAUSE_DESCR,
            "<br>Duration: ", round(duration_hours, 2),
            "<br>Size: ", round(FIRE_SIZE, 2)
          )
        ),
        alpha = 0.5
      ) +
      geom_point(
        data = bubble_general,
        aes(
          x = avg_duration,
          y = avg_size,
          color = STAT_CAUSE_DESCR,
          text = paste0(
            "CAUSE (AVG)",
            "<br>Cause: ", STAT_CAUSE_DESCR,
            "<br>Avg Duration: ", round(avg_duration, 2),
            " ± ", round(sd_duration, 2),
            "<br>Avg Size: ", round(avg_size, 2),
            " ± ", round(sd_size, 2),
            "<br>N: ", num_occurrences
          )
        ),
        size = 5,
        shape = 17
      ) +
      stat_ellipse(
        data = bubble_general,
        aes(
          x = avg_duration,
          y = avg_size,
          color = STAT_CAUSE_DESCR
        ),
        level = 0.68
      ) +

      scale_x_log10(labels = scales::comma) +
      scale_y_log10(labels = scales::comma) +
      theme_minimal() +
      labs(
        x = "Duration (log)",
        y = "Fire Size (log)",
        color = "Fire Cause"
      )

    ggplotly(p, tooltip = "text")

  })
}

shinyApp(ui, server)
