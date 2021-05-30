tabPanel(
  title = 'Assessment Analysis',
  value = 'assessment_analysis',
  
  fluidPage(
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4('Data Filter'),
          hr(style="border-top: 1px solid #000000;"),
          uiOutput(outputId = 'aa_sel_product_ui')
        ),
        mainPanel(
          h4('Assessment Summary'),
          hr(style="border-top: 1px solid #000000;"),
          uiOutput(outputId = 'aa_infobox_ui') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),
          DT::dataTableOutput(outputId = 'aa_assess_table') %>%
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
          uiOutput(outputId = 'aa_sel_assess_ui')
        ),
        mainPanel(
          h4('Assessment Score Distribution'),
          hr(style="border-top: 1px solid #000000;"),
          plotOutput(outputId = 'aa_assess_score_plot', height = '500px') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),
          h4('Assessment Item Summary'),
          hr(style="border-top: 1px solid #000000;"),
          DT::dataTableOutput(outputId = 'aa_assess_item_table') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),br()
        )
      )
    )
  )
)