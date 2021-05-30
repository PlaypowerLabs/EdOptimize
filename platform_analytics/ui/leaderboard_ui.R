tabPanel(
  title = 'Leaderboard',
  value = 'leaderboard',
  fluidPage(
    uiOutput(outputId = 'dl_plot_ui'),
    h4('District Leaderboard'),
    hr(style="border-top: 1px solid #000000;"),
    DT::dataTableOutput(outputId = 'dl_leaderboard')
  )
)