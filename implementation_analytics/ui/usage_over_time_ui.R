tabPanel(
  title = 'Usage Over Time',
  value = 'usage_over_time',
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4('Data Filter:'),
        hr(style="border-top: 1px solid #000000;"),
        radioButtons(inputId = 'uot_res', label = 'Resolution:',
                     choices = c('Day', 'Week', 'Month'),
                     selected = 'Week'),
        uiOutput(outputId = 'uot_date_range_ui'),
        selectInput(inputId = 'uot_usage_type', label = 'Usage Type',
                    choices = c('Overall', 'Subject', 'Grade Band', 'Most Used Product Groups'),
                    selected = 'Overall'),
        uiOutput(outputId = 'uot_usage_type_options_ui'),
        uiOutput(outputId = 'uot_add_feature_ui'),
        uiOutput(outputId = 'uot_facet_ui'),
        uiOutput(outputId = 'uot_userrole_ui'),
        uiOutput(outputId = 'uot_fix_y_ui')
      ),
      mainPanel(
        h4('Usage Over Time Plot'),
        hr(style="border-top: 1px solid #000000;"),
        tabsetPanel(id = 'uot_plots',
                    tabPanel(title = 'New vs Returning Users', value = 'uot_nvr',
                             plotOutput("uot_plot", height = 'auto', width = "100%") %>%
                               withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')),
                    tabPanel(title = 'Avg Session Duration', value = 'uot_asd',
                             plotOutput("uot_asd_plot", height = 'auto', width = "100%") %>%
                               withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')),
                    tabPanel(title = 'Avg Events per User', value = 'uot_aeu',
                             plotOutput("uot_aeu_plot", height = 'auto', width = "100%") %>%
                               withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'))),
        br(),br(),
        h4('Usage Over Time Table'),
        hr(style="border-top: 1px solid #000000;"),
        DT::dataTableOutput("uot_table") %>%
          withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
        br(),br()
      )
    )
  )
)