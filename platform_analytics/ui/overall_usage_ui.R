tabPanel(
  title = 'Overall Usage',
  value = 'overall_usage',
  fluidPage(
    fluidRow(
     sidebarLayout(
       sidebarPanel(
         width = 3,
         h4('Global Filter'),
         hr(style="border-top: 1px solid #000000;"),
         selectInput(inputId = 'ou_select_year',label = 'Select School Year:', choices = c('SY1819'))
       ),
       mainPanel(
         h4('Overall Usage Summary'),
         hr(style="border-top: 1px solid #000000;"),
         uiOutput(outputId = 'ou_info_boxes_ui') %>%
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
          selectInput(inputId = 'ou_usage_type', label = 'Usage Type:',
                      choices = c('Subject Usage', 'Grade Band Usage', 'Product Group Usage')),
          uiOutput(outputId = 'ou_usage_type_filter_ui'),
          uiOutput(outputId = 'ou_usage_type_filter_options_ui'),
          radioButtons(inputId = 'ou_usage_metric', label = 'Usage Metric:', choices = c('# Users', '# Sessions'))
        ),
        mainPanel(
          h4('Overall Usage Summary Plot'),
          hr(style="border-top: 1px solid #000000;"),
          uiOutput(outputId = 'ou_plot_ui') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),
          h4('Overall Usage Summary Table'),
          hr(style="border-top: 1px solid #000000;"),
          DT::dataTableOutput("ou_table") %>%
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
          helpText(strong("Active Week Defination: "),
                   "Atleast 1 active day on the platform in a week.",
                   br(),
                   br(),
                   strong("Active Day Defination: "),
                   "Greater than or equal to 1 events",
                   "in a day."),
          hr(style="border-top: 1px solid #000000;"),
          radioButtons(inputId = 'ou_active_weeks_filter', label = 'Usage Metric:',
                       choices = c('% of Active Weeks', 'Total Active Weeks'),
                       selected = 'Total Active Weeks')
          
        ),
        
        mainPanel(
          h4('User Active Weeks Plot'),
          hr(style="border-top: 1px solid #000000;"),
          uiOutput(outputId = 'ou_plot_2_ui') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),br()
        )
      )
    )
  )
)