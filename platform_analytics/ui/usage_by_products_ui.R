tabPanel(
  title = 'Usage by Products',
  value = 'usage_by_products',
  fluidPage(
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4('Data Filter'),
          hr(style="border-top: 1px solid #000000;"),
          selectInput(inputId = 'ubp_product_group', label = 'Select Product Group:',
                      choices = product_groups)
        ),
        mainPanel(
          h4('Product Group Summary'),
          hr(style="border-top: 1px solid #000000;"),
          uiOutput(outputId = 'ubp_info_boxes_ui'),
          br(),br(),
          plotOutput(outputId = 'ubp_product_users_plot') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),br()
        )
      )
    ),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          radioButtons(inputId = 'ubp_uot_deeper_dive', label = 'Take a Deeper Dive:',
                       choices = c('Yes','No'), selected = 'No'),
          uiOutput(outputId = 'ubp_uot_product_ui')
        ),
        mainPanel(
          h4('Product Group Usage Over Time'),
          hr(style="border-top: 1px solid #000000;"),
          plotOutput(outputId = 'ubp_pg_usage_over_time_plot', height = '800px', width = '100%') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),br()
        )
      )
    ),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          radioButtons(inputId = 'ubp_user_behav', label = 'User Behavior Metric:',
                       choices = c('# Active Days', '# Active Weeks', 'Session Time'), selected = '# Active Days')
        ),
        mainPanel(
          h4('User Behavior'),
          hr(style="border-top: 1px solid #000000;"),
          plotOutput(outputId = 'ubp_user_behav_plot', height = '700px') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          h4('Product Comparison'),
          hr(style="border-top: 1px solid #000000;"),
          DT::dataTableOutput(outputId = 'ubp_product_comparison_table') %>%
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),
          plotOutput(outputId = 'ubp_product_comparison_plot', height = '700px', width = '100%') %>% 
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),br()
        )
      )
    ),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          radioButtons(inputId = 'ubp_cu_deeper_dive', label = 'Take a Deeper Dive:',
                       choices = c('Yes','No'), selected = 'No'),
          uiOutput(outputId = 'ubp_cu_product_ui')
        ),
        mainPanel(
          h4('Usage by Content Area'),
          hr(style="border-top: 1px solid #000000;"),
          plotOutput(outputId = 'ubp_content_area_usage_plot') %>% 
            withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff'),
          br(),br()
        )
      )
    )
  )
  
)