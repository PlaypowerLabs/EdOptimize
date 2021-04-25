tabPanel(
  title = 'Curriculum Insights',
  value = 'curriculum_insights',
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4('Data Filter:'),
        hr(style="border-top: 1px solid #000000;"),
        radioButtons(inputId = 'ci_plot_type', label = 'Plot Type:',
                     choices = c('Usage Over Time', 'Curriculum Pacing')),
        uiOutput(outputId = 'ci_sel_product_ui'),
        uiOutput(outputId = 'ci_add_feature_ui'),
        uiOutput(outputId = 'ci_user_type_ui'),
        uiOutput(outputId = 'ci_fill_by_ui')
      ),
      mainPanel(
        uiOutput(outputId = 'ci_plot_ui'),
        uiOutput(outputId = 'ci_table_ui')
      )
    )
  )
)