tabPanel(
  title = 'Content Usage',
  value = 'content_usage',
  
  fluidPage(
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4('Data Filter'),
          hr(style="border-top: 1px solid #000000;"),
          selectInput(inputId = 'cu_usage_type', label = 'Usage Type:',
                      choices = c('Overall', 'By Product'),
                      selected = 'Overall'),
          uiOutput(outputId = 'cu_usage_type_options_ui')
        ),
        mainPanel(
          h4('Content Usage Summary'),
          hr(style="border-top: 1px solid #000000;"),
          uiOutput('cu_content_summary_title_ui'),
          br(),
          uiOutput('cu_content_summary_ui') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),
          h4('Content Popularity'),
          hr(style="border-top: 1px solid #000000;"),
          plotOutput(outputId = 'cu_content_pop_plot', height = '700px') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br()
        )
      )
    ),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4('Data Filter'),
          hr(style="border-top: 1px solid #000000;"),
          radioButtons(inputId = 'cu_usage_metric', label = 'Select Usage Metric:',
                       choices = c('# of users','# events per user','Unique content items'))
        ),
        mainPanel(
          h4('Content Type Usage'),
          hr(style="border-top: 1px solid #000000;"),
          plotOutput(outputId = 'cu_content_type_usage_plot', height = '500px') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br()
        )
      )
    ),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4('Data Filter'),
          hr(style="border-top: 1px solid #000000;"),
          radioButtons(inputId = 'cu_fill_by', label = 'Fill by:',
                       choices = c('% Users','Total Users'))
        ),
        mainPanel(
          plotOutput(outputId = 'cu_content_type_usage_heatmap', height = '450px', width = '100%') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),
          h4('Content Usage Metrics'),
          hr(style="border-top: 1px solid #000000;"),
          DT::dataTableOutput(outputId = 'cu_table') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),br()
        )
      )
    )
  )
)