tabPanel(
  title = 'Usage by Content Type',
  value = 'usage_by_content_type',
  fluidPage(
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4('Data Filter'),
          hr(style="border-top: 1px solid #000000;"),
          selectInput(inputId = 'ubct_usage_type', label = 'Usage Type:',
                      choices = c('Subject', 'Grade Band', 'Product Group'), selected = 'Subject'),
          uiOutput(outputId = 'ubct_usage_type_options_ui')
        ),
        mainPanel(
          h4('Usage by Content Type'),
          hr(style="border-top: 1px solid #000000;"),
          plotOutput(outputId = 'ubct_plot_1', height = '800px') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),br(),hr()
        )
      )
    ),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          radioButtons(inputId = 'ubct_usage_metric', label = 'Usage Metric:',
                       choices =  c('# Students','# Teachers','# Sessions'), selected = '# Sessions')
        ),
        mainPanel(
          plotOutput(outputId = 'ubct_plot_2', height = '600px') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(), br(),
          h4('Usage by Content Type Summary'),
          hr(style="border-top: 1px solid #000000;"),
          DT::dataTableOutput(outputId = 'ubct_table') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),br()
        )
      )
    )
  )
  
)