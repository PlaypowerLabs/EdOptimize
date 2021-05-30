#------UI ELEMENTS------
#----select product component-----
output$dl_sel_product_ui <- renderUI({
  req(!is.null(input$ou_select_pg))
  
  x <- product_group_products_rdf %>%
    filter(product_group %in% input$ou_select_pg) %>%
    .$product
  
  selectInput(inputId = 'dl_sel_product', label = 'Select Product:', choices = x)
})

#----district plot ui-------

output$dl_plot_ui <- renderUI({
  req(!is.null(input$dl_sel_product))
  
  s <- input$dl_leaderboard_rows_selected
  
  if(length(s)){
    fluidPage(
      h4('District Explorer'),
      hr(style="border-top: 1px solid #000000;"),
      tabsetPanel(id = 'dl_tab_panel',
                  tabPanel(
                    title = 'New vs Returning Users',
                    value = 'nvr',
                    plotOutput(outputId = 'dl_nvr_plot', height = input$dimension[2] * .4) %>%
                      withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')
                  ),
                  tabPanel(
                    title = 'Events per User Over Time',
                    value = 'eot',
                    plotOutput(outputId = 'dl_eot_plot', height = input$dimension[2] * .4) %>%
                      withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')
                  ))
    )
  }
})



#-----REACTIVE COMPONENTS-----
#----leaderboard data prep-----

dl_leaderboard_data <- reactive({
  req(!is.null(input$dl_sel_product))
  
  df <- dl_product_wise_rdf %>%
    filter(product %in% input$dl_sel_product) %>%
    select(-product)
  
  scale_cols <- names(df)[c(-1)]
  
  scaled_score_df <- map(scale_cols, function(x){
    scale(df[[x]])
  }) %>%
    bind_cols()
  
  df_2 <- df %>%
    mutate(scaled_score_sum = rowSums(scaled_score_df)) %>%
    arrange(desc(scaled_score_sum)) %>%
    mutate(rank = row_number()) %>%
    select('Rank' = rank, 'District Name' = districtname, 'Avg Active Days per User' = avg_active_days,
           'Avg Contents per User' = avg_contents, '# Students' = n_students, '# Teachers' = n_teachers,
           'Avg Student Events' = avg_student_events, 'Avg Teacher Events' = avg_teacher_events)
  
  df_2
})

#------new vs ret plot data----
dl_nvr_plot_data <- reactive({
  req(!is.null(input$dl_sel_product), !is.null(input$dl_leaderboard_rows_selected))
  
  district <- dl_leaderboard_data()[['District Name']][input$dl_leaderboard_rows_selected]
  
  df <- dl_product_wise_nvr_rdf %>%
    filter(product %in% input$dl_sel_product,
           districtname %in% district) %>%
    select(-product, -districtname) %>%
    pivot_longer(cols = c(new,returning), names_to = 'feature', values_to = 'value') %>%
    mutate(feature = factor(feature, levels = c('returning','new'), labels = c('Returning User', 'New User')))
  
  label <- labs(
    title = paste0('Product: ',input$dl_sel_product),
    subtitle = paste0('District: ', district)
  )
  
  list(data = df, label = label)
})

#-----events per user plot data----

dl_eot_plot_data <- reactive({
  req(!is.null(input$dl_sel_product), !is.null(input$dl_leaderboard_rows_selected))
  
  district <- dl_leaderboard_data()[['District Name']][input$dl_leaderboard_rows_selected]
  
  df <- dl_product_wise_eot_rdf %>%
    filter(product %in% input$dl_sel_product,
           districtname %in% district) %>%
    select(-product, -districtname)
  
  label <- labs(
    title = paste0('Product: ',input$dl_sel_product),
    subtitle = paste0('District: ', district)
  )
  
  list(data = df, label = label)
})



#-----OUTPUT ELEMENTS------
#------leaderboard table------

options(DT.options = list(pageLength = 15))
output$dl_leaderboard <- DT::renderDataTable({
  req(!is.null(input$dl_sel_product))
  
  DT::datatable(dl_leaderboard_data(), filter = 'none', class = 'cell-border stripe', 
                options(list(scrollX = TRUE, sDom  = '<"top">lrt<"bottom">ip',
                             columnDefs = list(list(className = 'dt-center', targets = 0)))), rownames= FALSE,
                selection = 'single', escape = FALSE)
  
  
}, server = TRUE)

#----new vs ret plot-----
output$dl_nvr_plot <- renderPlot({
  req(!is.null(input$dl_sel_product), !is.null(input$dl_leaderboard_rows_selected))
  
  district_new_vs_returning_users_plot(dl_nvr_plot_data()$data) +
    dl_nvr_plot_data()$label
})

#----events per user plot-----
output$dl_eot_plot <- renderPlot({
  req(!is.null(input$dl_sel_product), !is.null(input$dl_leaderboard_rows_selected))
  
  district_events_per_user_over_time_plot(dl_eot_plot_data()$data) +
    dl_eot_plot_data()$label
})



