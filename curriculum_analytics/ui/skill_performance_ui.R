tabPanel(
  title = 'Skill Performance',
  value = 'skill_performance',
  
  fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4('Data Filter'),
          hr(style="border-top: 1px solid #000000;"),
          uiOutput(outputId = 'sp_sel_product_ui'),
          uiOutput(outputId = 'sp_skills_slider_ui')
        ),
        mainPanel(
          h4('Skills Usage Summary'),
          hr(style="border-top: 1px solid #000000;"),
          uiOutput(outputId = 'sp_infobox_ui') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),
          h4('Skill Pacing'),
          hr(style="border-top: 1px solid #000000;"),
          plotOutput(outputId = 'sp_pacing_plot', height = '700px') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),
          h4('Skill Performance'),
          hr(style="border-top: 1px solid #000000;"),
          DT::dataTableOutput(outputId = 'sp_table') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),br()
        )
      )
  )
)