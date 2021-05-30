tabPanel(
  title = 'Leaderboard',
  value = 'leaderboard',
  
  fluidPage(
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4('Data Filter'),
          hr(style="border-top: 1px solid #000000;"),
          uiOutput(outputId = 'dl_sel_product_ui')
        ),
        mainPanel(
          uiOutput(outputId = 'dl_plot_ui'),
          h4('District Leaderboard'),
          hr(style="border-top: 1px solid #000000;"),
          DT::dataTableOutput(outputId = 'dl_leaderboard') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),br()
        )
      )
    )
  )
)