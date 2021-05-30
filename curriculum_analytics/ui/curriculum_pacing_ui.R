tabPanel(
  title = 'Curriculum Pacing',
  value = 'curriculum_pacing',
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4('Data Filter'),
        hr(style="border-top: 1px solid #000000;"),
        uiOutput('cp_sel_product_ui'),
        radioButtons(inputId = 'cp_usertype', label = 'User Type:', choices = c('Student','Teacher')),
        uiOutput('cp_fill_by_ui')
      ),
      mainPanel(
        h4('Pacing Chart'),
        hr(style="border-top: 1px solid #000000;"),
        uiOutput('cp_pacing_plot_ui'),
        br(),
        h4('Pacing Summary Table'),
        hr(style="border-top: 1px solid #000000;"),
        DT::dataTableOutput('cp_pacing_table'),
        br(),br()
      )
    )
  )
)