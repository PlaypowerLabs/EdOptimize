tabPanel(
  title = 'Leaderboard',
  value = 'leaderboard',
  fluidPage(
    # sidebarLayout(
    #   sidebarPanel(
    #     width = 3,
    #     style = "background-color: white;",
    #     h4('Data Filter:'),
    #     hr(style="border-top: 1px solid #000000;"),
    #     selectInput(inputId = 'dl_usage_type', label = 'Usage Type:',
    #                 choices = c('Overall', 'Subject', 'Grade Band', 'Product Group'), selected = 'Overall'),
    #     uiOutput(outputId = 'dl_usage_type_options_ui')
    #   ),
    #   mainPanel(
    #     h4('District Leaderboard'),
    #     hr(style="border-top: 1px solid #000000;"),
    #     DT::dataTableOutput(outputId = 'dl_leaderboard')
    #   )
    # )
    uiOutput(outputId = 'dl_plot_ui'),
    h4('District Leaderboard'),
    hr(style="border-top: 1px solid #000000;"),
    DT::dataTableOutput(outputId = 'dl_leaderboard')
  )
)