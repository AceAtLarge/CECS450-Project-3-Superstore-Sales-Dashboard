
#Imports
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(lubridate)
library(scales)
library(DT)

#1 Load & Clean Data
superstore_raw <- read_csv("superstore.csv", show_col_types = FALSE)

#1a Convert date columns into the proper date format
superstore <- superstore_raw %>%
  rename_with(~ gsub(" ", "_", .x)) %>%
  rename_with(tolower) %>%
  mutate(order_date = mdy(order_date), 
         ship_date  = mdy(ship_date), 
         order_month = floor_date(order_date, "month"), 
         # (OLD) profit_margin = profit / sales)
         profit_margin = ifelse(sales != 0, profit / sales, NA_real_))

#1b create state abbreviations
state_abb_df <- data.frame(state = state.name, 
                           state_abbr = state.abb, 
                           stringsAsFactors = FALSE)

superstore <- superstore %>%
  left_join(state_abb_df, by = c("state" = "state"))

#2 vectors for filter dropdowns
all_regions <- sort(unique(superstore$region))
all_categories <- sort(unique(superstore$category))
all_segments <- sort(unique(superstore$segment))
all_shipmodes <- sort(unique(superstore$ship_mode))
date_range <- range(superstore$order_date, na.rm = TRUE)
disc_range <- range(superstore$discount, na.rm = TRUE)

#3 KPI Summary Cards
kpi_card <- function(id, label, icon_html){
  div(class="kpi-card", div(class = "kpi-icon",  
                            HTML(icon_html)), 
      div(class = "kpi-label", label), 
      div(class = "kpi-value", uiOutput(id))
  )
}

#4 UI Section
ui <- fluidPage(tags$head(tags$style(HTML(
  "@import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600&family=DM+Serif+Display&display=swap');
  :root {
        --bg:        #0f1117;
        --surface:   #1a1d27;
        --surface2:  #242736;
        --border:    #2e3248;
        --accent:    #6c63ff;
        --accent2:   #ff6584;
        --text:      #e8eaf0;
        --muted:     #7b7f9e;
        --success:   #43d9ad;
        --warning:   #ffc85c;
        --radius:    12px;
      }
 
      /* ── Base ── */
      body, .container-fluid {
        background: var(--bg) !important;
        color: var(--text) !important;
        font-family: 'DM Sans', sans-serif !important;
        margin: 0; padding: 0;
      }
 
      /* ── Top Banner ── */
      .dash-header {
        background: linear-gradient(135deg, #1a1d27 0%, #242736 100%);
        border-bottom: 1px solid var(--border);
        padding: 18px 32px;
        display: flex;
        align-items: center;
        gap: 14px;
      }
      .dash-header h1 {
        font-family: 'DM Serif Display', serif;
        font-size: 1.6rem;
        color: var(--text);
        margin: 0;
      }
      .dash-header .badge-live {
        background: var(--accent);
        color: #fff;
        font-size: 0.65rem;
        font-weight: 600;
        letter-spacing: 0.08em;
        padding: 3px 9px;
        border-radius: 20px;
        text-transform: uppercase;
      }
 
      .dash-body {
        display: flex;
        min-height: calc(100vh - 65px);
      }
 
      .dash-sidebar {
        width: 260px;
        min-width: 260px;
        background: var(--surface);
        border-right: 1px solid var(--border);
        padding: 20px 16px;
        overflow-y: auto;
      }
      .sidebar-section-title {
        font-size: 0.68rem;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.12em;
        color: var(--muted);
        margin: 18px 0 8px;
      }
      .dash-sidebar label, .dash-sidebar .control-label {
        color: var(--text) !important;
        font-size: 0.82rem !important;
        font-weight: 500 !important;
        margin-bottom: 4px !important;
      }
      .dash-sidebar .selectize-input,
      .dash-sidebar .selectize-dropdown {
        background: var(--surface2) !important;
        border: 1px solid var(--border) !important;
        color: var(--text) !important;
        border-radius: 8px !important;
        font-size: 0.83rem !important;
      }
      .dash-sidebar .selectize-input.focus { border-color: var(--accent) !important; }
      .dash-sidebar .irs--shiny .irs-bar { background: var(--accent) !important; 
      border-color:var(--accent) !important; }
      .dash-sidebar .irs--shiny .irs-handle { border-color: 
      var(--accent)!important; }
      .dash-sidebar .irs--shiny .irs-from,
      .dash-sidebar .irs--shiny .irs-to,
      .dash-sidebar .irs--shiny .irs-single { background: var(--accent) !important; }
      .btn-reset {
        width: 100%;
        margin-top: 18px;
        background: transparent;
        border: 1px solid var(--border);
        color: var(--muted);
        border-radius: 8px;
        padding: 7px 0;
        font-size: 0.82rem;
        cursor: pointer;
        transition: all 0.2s;
      }
      .btn-reset:hover { border-color: var(--accent); color: var(--accent); }
 
      .dash-main {
        flex: 1;
        padding: 24px 28px;
        overflow-x: hidden;
      }
 
      .kpi-row {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 16px;
        margin-bottom: 24px;
      }
      .kpi-card {
        background: var(--surface);
        border: 1px solid var(--border);
        border-radius: var(--radius);
        padding: 18px 20px;
        display: flex;
        flex-direction: column;
        gap: 6px;
        transition: border-color 0.2s;
      }
      .kpi-card:hover { border-color: var(--accent); }
      .kpi-icon  { font-size: 1.4rem; }
      .kpi-label { font-size: 0.75rem; color: var(--muted); font-weight: 500; text-transform: uppercase; letter-spacing: 0.06em; }
      .kpi-value { font-size: 1.5rem; font-weight: 600; color: var(--text); }
 
      .nav-tabs { border-bottom: 1px solid var(--border) !important; margin-bottom: 20px; }
      .nav-tabs > li > a {
        color: var(--muted) !important;
        background: transparent !important;
        border: none !important;
        border-bottom: 2px solid transparent !important;
        padding: 8px 18px !important;
        font-size: 0.85rem !important;
        font-weight: 500 !important;
        border-radius: 0 !important;
        transition: all 0.2s !important;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li > a:hover {
        color: var(--text) !important;
        border-bottom: 2px solid var(--accent) !important;
        background: transparent !important;
      }
      .tab-content { background: transparent !important; }
 
      .chart-card {
        background: var(--surface);
        border: 1px solid var(--border);
        border-radius: var(--radius);
        padding: 20px;
        margin-bottom: 20px;
      }
      .chart-title {
        font-size: 0.9rem;
        font-weight: 600;
        color: var(--text);
        margin-bottom: 14px;
        display: flex;
        align-items: center;
        gap: 8px;
      }
 
      .dataTables_wrapper { color: var(--text) !important; font-size: 0.83rem; }
      table.dataTable thead th { background: var(--surface2) !important; color: var(--muted) !important; border-bottom: 1px solid var(--border) !important; }
      table.dataTable tbody tr { background: var(--surface) !important; }
      table.dataTable tbody tr:hover td { background: var(--surface2) !important; }
      table.dataTable tbody td { border-color: var(--border) !important; color: var(--text) !important; }
      .dataTables_filter input, .dataTables_length select {
        background: var(--surface2) !important;
        border: 1px solid var(--border) !important;
        color: var(--text) !important;
        border-radius: 6px;
        padding: 3px 8px;
      }
      .dataTables_paginate .paginate_button { color: var(--muted) !important; }
      .dataTables_paginate .paginate_button.current { background: var(--accent) !important; color: #fff !important; border-radius: 6px; }
      .dataTables_info { color: var(--muted) !important; }
 
      
      .plotly .modebar { background: transparent !important; }
      .plotly .modebar-btn path { fill: var(--muted) !important; }
 
      
      .dash-sidebar .form-control {
        background: var(--surface2) !important;
        border: 1px solid var(--border) !important;
        color: var(--text) !important;
        border-radius: 8px !important;
        font-size: 0.83rem !important;
      }
 
      @media (max-width: 900px) {
        .dash-body { flex-direction: column; }
        .dash-sidebar { width: 100%; min-width: unset; border-right: none; border-bottom: 1px solid var(--border); }
        .kpi-row { grid-template-columns: repeat(2, 1fr); }
      }
  "))
),
# Header
div(
  class = "dash-header",
  span("🛒"),
  h1("Superstore Sales Dashboard"),
  span(class = "badge-live", "Interactive")),

# Body
div(
  class = "dash-body",
  
  div(class = "dash-sidebar",
      div(class = "sidebar-section-title", "📅 Date"),
      dateRangeInput("date_range", NULL, start = date_range[1], 
                     end = date_range[2], min = date_range[1],
                     max = date_range[2], format = "mm/dd/yyyy"),
      
      div(class = "sidebar-section-title", "🌎 Region"), 
      selectInput("region", NULL, choices = c("All", all_regions), 
                  selected = "All"), 
      div(class = "sidebar-section-title", "📦 Category"),
      selectInput("category", NULL, choices  = c("All", all_categories),
                  selected = "All"), 
      div(class = "sidebar-section-title", "👤 Segment"),
      selectInput("segment", NULL, choices  = c("All", all_segments), 
                  selected = "All"),
      div(class = "sidebar-section-title", "🚚 Ship Mode"),
      selectInput("ship_mode", NULL, choices  = c("All", all_shipmodes),
                  selected = "All"),
      div(class = "sidebar-section-title", "💲 Discount Range"),
      sliderInput("discount", NULL, min   = 0, max = round(disc_range[2], 2),
                  value = c(0, round(disc_range[2], 2)),
                  step  = 0.01, post = "%", ticks = FALSE),
      tags$button("↺ Reset Filters", class = "btn-reset",
                  onclick = "Shiny.setInputValue('reset', Math.random())")
  ),
  
  # Main
  div(
    class = "dash-main", 
    div(
      class = "kpi-row",
      kpi_card("kpi_sales", "Total Sales", "💰"),
      kpi_card("kpi_profit", "Total Profit", "📈"),
      kpi_card("kpi_orders",  "Orders", "📦"),
      kpi_card("kpi_discount", "Avg. Discount", "💲")
    ),
    
    # Tabs
    tabsetPanel(
      id = "main_tabs",
      tabPanel(
        "🗺 Map",
        div(class = "chart-card",
            div(class = "chart-title", "Sales by State"),
            plotlyOutput("map_plot", height = "440px"))
      ), 
      tabPanel(
        "📊 Category",
        div(class = "chart-card",
            div(class = "chart-title", "Sales by Category & Sub-Category"),
            plotlyOutput("category_plot", height = "440px"))
      ), 
      tabPanel(
        "💹 Profit vs Discount",
        div(class = "chart-card",
            div(class = "chart-title", "Profit vs. Discount (with Trend)"),
            plotlyOutput("scatter_plot", height = "440px"))
      ),
      tabPanel(
        "📅 Monthly Trend",
        div(class = "chart-card",
            div(class = "chart-title", "Monthly Sales Trend"),
            plotlyOutput("trend_plot", height = "440px"))
      ),
      tabPanel(
        "📋 Top States",
        div(class = "chart-card",
            div(class = "chart-title", "State-Level Summary"),
            DTOutput("states_table"))
      )
    )
  )
)
)

# Server
server <- function(input, output, session){
  # Plotly theme helpers
  bg <- "#0f1117"
  surf <-"#1a1d27"
  bord <- "#2e3248"
  txt <- "#e8eaf0"
  mut <- "#7b7f9e"
  acc <- "#6c63ff"
  acc2 <- "#ff6584"
  suc <- "#43d9ad"
  
  base_layout <- list(
    paper_bgcolor = surf, plot_bgcolor = surf, 
    font = list(family = "DM Sans", color = txt, size = 12),
    margin = list(l = 50, r = 20, t = 20, b = 50), 
    legend = list(bgcolor = "transparent", font = list(color = txt)),
    hoverlabel = list(bgcolor = "#242736", 
                      bordercolor = bord,
                      font = list(color = txt, size = 12))
  )
  axis_style <- list(
    gridcolor = bord, zerolinecolor = bord, 
    tickfont = list(color = mut, size = 11), 
    titlefont = list(color = mut, size = 12)
  )
  
  # reset filters
  observeEvent(input$reset, {
    updateSelectInput(session, "region", selected = "All")
    updateSelectInput(session, "category", selected = "All")
    updateSelectInput(session, "segment", selected = "All")
    updateSelectInput(session, "ship_mode", selected = "All")
    updateDateRangeInput(session, "date_range", 
                         start = date_range[1], end = date_range[2])
    updateSliderInput(session, "discount", value = c(0, round(disc_range[2], 2))
    )
  })
  
  # reactive filtered data
  filtered <- reactive({
    df <- superstore %>% filter(order_date >= input$date_range[1],
                                order_date <= input$date_range[2],
                                discount >= input$discount[1],
                                discount <= input$discount[2])
    
    if(input$region != "All") df <- df %>% filter(region == input$region)
    if(input$category != "All") df <- df %>% filter(category == input$category)
    if(input$segment != "All") df <- df %>% filter(segment == input$segment)
    if(input$ship_mode != "All") df <- df %>% filter(ship_mode == input$ship_mode)
    
    df
  })
  
  # KPIs
  output$kpi_sales <- renderUI({dollar(sum(filtered()$sales, na.rm = TRUE))
  })
  output$kpi_profit <- renderUI({v <- sum(filtered()$profit, na.rm = TRUE)
  col <- if (v >= 0) "#43d9ad" else "#ff6584"
  tags$span(style = paste0("color:", col), dollar(v))
  })
  output$kpi_orders <- renderUI({
    comma(nrow(filtered()))
  })
  output$kpi_discount <- renderUI({
    percent(mean(filtered()$discount, na.rm = TRUE), accuracy = 0.1)
  })
  
  # Map
  output$map_plot <- renderPlotly({
    validate(need(nrow(filtered()) > 0, "No data matches the selected filters."))
    state_summary <- filtered() %>%
      group_by(state_abbr, state) %>%
      summarise(sales = sum(sales, na.rm = TRUE), .groups = "drop")
    
    plot_geo(state_summary, locationmode = "USA-states") %>%
      add_trace(z = ~sales, locations = ~state_abbr, color = ~sales,
                colorscale = list(c(0, "#1a1d27"), c(0.5, acc), c(1, suc)), 
                text = ~paste0("<b>", state, "</b><br>Sales: ", dollar(sales)), 
                hoverinfo = "text", 
                marker = list(line = list(color = bord, width = 0.5))
      ) %>%
      colorbar(title = "Sales ($)", tickfont = list(color = mut)) %>%
      layout(
        geo = list(scope = "usa", bgcolor = surf, lakecolor = surf, 
                   landcolor = surf, subunitcolor = bord, showlakes = TRUE
        ), 
        paper_bgcolor = surf,
        font = list(color = txt), 
        margin = list(l = 0, r = 0, t = 0, b = 0)
      )
  })
  
  # Category bar chart
  output$category_plot <- renderPlotly({
    validate(need(nrow(filtered()) > 0, "No data matches the selected filters."))
    cat_df <- filtered() %>%
      group_by(category, sub_category) %>%
      summarise(sales = sum(sales, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(sales))
    
    pal <- c("#6c63ff", "#ff6584", "#43d9ad", "#ffc85c", "#a78bfa", "#f9a8d4", 
             "#6ee7b7", "#fde68a", "#c4b5fd", "#fbcfe8", "#34d399")
    
    p <- ggplot(cat_df, aes(x = reorder(sub_category, sales), y = sales,
                            fill = category, 
                            text = paste0("<b>", sub_category, "</b><br>", 
                                          "Category: ", category, "<br>",
                                          "Sales: ", dollar(sales)))) + 
      geom_col(width = 0.7) + coord_flip() + 
      scale_fill_manual(values = pal) +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = NULL, y = "Total Sales", fill = "Category") + 
      theme(plot.background  = element_rect(fill = surf, color = NA),
            panel.background = element_rect(fill = surf, color = NA),
            panel.grid.major.x = element_line(color = bord, linewidth = 0.4),
            panel.grid.major.y = element_blank(),
            axis.text = element_text(color = mut, size = 9),
            axis.title = element_text(color = mut, size = 10), 
            legend.background = element_rect(fill = surf, color = NA),
            legend.text = element_text(color = txt),
            legend.title = element_text(color = mut)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = surf,  plot_bgcolor = surf, 
             font = list(color = txt), 
             legend = list(bgcolor = "transparent")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # scatter plot profit vs discount
  output$scatter_plot <- renderPlotly({
    validate(need(nrow(filtered()) > 0, "No data matches the selected filters."))
    df <- filtered() %>%
      group_by(order_id, category) %>%
      summarise(discount = mean(discount, na.rm = TRUE), 
                profit = sum(profit, na.rm = TRUE),
                sales = sum(sales, na.rm = TRUE),
                .groups = "drop"
      )
    
    pal <- c(acc, acc2, suc)
    cats <- unique(df$category)
    
    fig <- plot_ly()
    
    for(i in seq_along(cats)){
      sub <- df %>% filter(category == cats[i])
      fig <- fig %>%
        add_trace(data = sub, x = ~discount, y = ~profit, type = "scatter", 
                  mode = "markers", name = cats[i], 
                  marker = list(color = pal[i], size = 6, opacity = 0.65, 
                                line = list(width = 0)),
                  text = ~paste0("<b>Order:</b> ", order_id,
                                 "<br>Discount: ", percent(discount, 0.1),
                                 "<br>Profit: ",   dollar(profit)),
                  hoverinfo = "text"
        )
    }
    
    # trend line across all data
    trend_df <- df %>%
      filter(is.finite(discount), is.finite(profit))
    
    if (nrow(trend_df) >= 2 && dplyr::n_distinct(trend_df$discount) >= 2) {
      fit  <- lm(profit ~ discount, data = trend_df)
      xseq <- seq(min(trend_df$discount), max(trend_df$discount), length.out = 80)
      ypred <- predict(fit, newdata = data.frame(discount = xseq))
      
      fig <- fig %>%
        add_trace(x = xseq, y = ypred, type = "scatter", mode = "lines", 
                  name = "Trend", 
                  line = list(color = "#ffc85c", width = 2, dash = "dot"),
                  hoverinfo = "skip", showlegend = TRUE
        )
    }
    
    fig <- fig %>%
      layout(xaxis= c(list(title = "Discount"), axis_style, 
                      list(tickformat = ".0%")), 
             yaxis = c(list(title = "Profit ($)"), axis_style,
                       list(tickprefix = "$", separatethousands = TRUE)), 
             base_layout
      ) %>%
      config(displayModeBar = FALSE)
    fig
  })
  
  # monthly trend
  output$trend_plot <- renderPlotly({
    validate(need(nrow(filtered()) > 0, "No data matches the selected filters."))
    trend_df <- filtered() %>%
      group_by(order_month) %>%
      summarise(sales = sum(sales, na.rm = TRUE), .groups = "drop") %>%
      arrange(order_month)
    
    plot_ly(trend_df, x = ~order_month, y = ~sales, type = "scatter", 
            mode = "lines+markers", line = list(color = acc, width = 2.5), 
            marker = list(color = acc, size = 6), 
            fill = "tozeroy", fillcolor = "rgba(108,99,255,0.1)",
            text = ~paste0("<b>", format(order_month, "%b %Y"), 
                           "</b><br>Sales: ", dollar(sales)), 
            hoverinfo = "text") %>%
      layout(xaxis = c(list(title = "Month"), axis_style),
             yaxis = c(list(title = "Sales ($)", 
                            tickprefix = "$"), axis_style), base_layout
      ) %>%
      config(displayModeBar = FALSE)
  })
  # Top states table
  output$states_table <- renderDT({
    validate(need(nrow(filtered()) > 0, "No data matches the selected filters."))
    tbl <- filtered() %>%
      group_by(State = state, Region = region) %>%
      summarize(`Total Sales` = sum(sales, na.rm = TRUE), 
                `Total Profit` = sum(profit, na.rm = TRUE),
                `Avg. Discount`  = mean(discount, na.rm = TRUE),
                Orders = n(), .groups = "drop"
      ) %>%
      arrange(desc(`Total Sales`)) %>%
      mutate(`Total Sales` = dollar(`Total Sales`,accuracy = 1),
             `Total Profit` = dollar(`Total Profit`, accuracy = 1),
             `Avg. Discount` = percent(`Avg. Discount`, accuracy = 0.1)
      )
    
    datatable(tbl, rownames = FALSE, 
              options  = list(pageLength = 10, dom = "ftip", ordering = TRUE, 
                              autoWidth = TRUE, scrollX = TRUE, 
                              columnDefs = list(list(className = "dt-right", 
                                                     targets = c(2, 3, 4, 5)))
              ), 
              class = "display compact"
    )
  })
}

# Launch
shinyApp(ui = ui, server = server)


















