tabPanel(
  title = 'Leaderboard',
  value = 'leaderboard',
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4('Data Filter:'),
        hr(style="border-top: 1px solid #000000;"),
        radioButtons(inputId = 'lb_leaderboard_type', label = 'Leaderboard Type:',
                     choices = c('School','Class'), selected = 'Class'),
        radioButtons(inputId = 'lb_usage_type', label = 'Usage Type:',
                     choices = c('Overall','By Product')),
        uiOutput(outputId = 'lb_sel_product_ui')
      ),
      mainPanel(
        uiOutput(outputId = 'lb_plot_ui'),
        uiOutput('lb_heading_ui'),
        DT::dataTableOutput(outputId = 'lb_leaderboard') %>%
          withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
        br(),br()
      )
    )
  )
)