tabPanel(
  title = 'Usage by Geography',
  value = 'usage_by_geography',
  fluidPage(
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4('Data Filter'),
          hr(style="border-top: 1px solid #000000;"),
          selectInput(inputId = 'ubg_usage_type', label = 'Usage Type:',
                      choices = c('Overall', 'Subject', 'Grade Band', 'Product Group'),
                      selected = 'Overall'),
          uiOutput(outputId = 'ubg_usage_type_options_ui'),
          selectInput(inputId = 'ubg_usage_metric', label = 'Usage Metric:',
                      choices = c('# Students', '# Teachers', '# Sessions', '# Events per Student', '# Events per Teacher'),
                      selected = '# Sessions')

        ),

        mainPanel(
          tabsetPanel(id = 'ubg_map_tabs',
                      tabPanel(title = 'Heat Map', value = 'heatmap',
                               leafletOutput("ubg_heatmap", width = "100%", height = "700") %>%
                                 withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')),
                      tabPanel(title = 'Dot Chart', value = 'dotchart',
                               leafletOutput("ubg_dotchart", width = "100%", height = "700") %>%
                                 withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')))
        )

      )

    )
  )
  
)


