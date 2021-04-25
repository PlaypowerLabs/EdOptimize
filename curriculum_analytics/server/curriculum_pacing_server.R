#------UI ELEMENTS------
#----select product component-----
output$cp_sel_product_ui <- renderUI({
  req(!is.null(input$ou_select_pg))
  
  x <- product_group_products_rdf %>%
    filter(product_group %in% input$ou_select_pg) %>%
    .$product
  
  selectInput(inputId = 'cp_sel_product', label = 'Select Product:', choices = x)
})

#----pacing plots ui-------
output$cp_pacing_plot_ui <- renderUI({
  req(!is.null(input$cp_usertype))
  
  if(input$cp_usertype == 'Student'){
    tabsetPanel(id = 'cp_plots',
                tabPanel(title = 'Usage Heatmap', value = 'cp_usage_heatmap',
                         plotOutput("cp_plot_1", height = "700px", width = "100%") %>%
                           withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')),
                tabPanel(title = 'Performance Heatmap', value = 'cp_perf_heatmap',
                         plotOutput("cp_plot_2", height = "700px", width = "100%") %>%
                           withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')),
                tabPanel(title = 'Performance Dotchart', value = 'cp_perf_dotchart',
                         plotOutput("cp_plot_3", height = "700px", width = "100%") %>%
                           withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')))
  }
  
  else if(input$cp_usertype == 'Teacher'){
    tabsetPanel(id = 'cp_plots',
                tabPanel(title = 'Usage Heatmap', value = 'cp_usage_heatmap',
                         plotOutput("cp_plot_1", height = "700px", width = "100%") %>%
                           withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')))
  }
  
})

#-----fill by ui element-----

output$cp_fill_by_ui <- renderUI({
  req(!is.null(input$cp_plots))
  
  if(input$cp_plots == 'cp_usage_heatmap'){
    radioButtons(inputId = 'cp_fill_by', label = 'Fill by:',
                 choices = c('# Users', 'Avg Events per User', '# Events'))
  }
  
})

#------REACTIVE COMPONENTS-----
#----usage heatmap plot data----

cp_usage_heatmap_data <- reactive({
  req(!is.null(input$cp_sel_product), !is.null(input$cp_fill_by))
  
  filler_col <- c('# Users' = 'n_users', 'Avg Events per User' = 'n_events_per_user',
                  '# Events' = 'n_events')
  
  
  
  df <- cp_usage_heatmap_rdf %>%
    filter(product %in% input$cp_sel_product) %>%
    rename('fill_by' = unname(filler_col[input$cp_fill_by])) %>%
    select(userroletype, title, date_binned_by_week, fill_by)
  
  df_2 <- cp_topic_lesson_rdf %>%
    filter(product %in% input$cp_sel_product) %>%
    select(-product)
  
  label <- labs(
    title = paste0('Product: ', input$cp_sel_product)
  )
  
  list(data = df, y_label_data = df_2, label = label)
  
})

#-----performance heatmap plot data-----

cp_perf_heatmap_data <- reactive({
  req(!is.null(input$cp_sel_product))
  
  df <- cp_perf_heatmap_rdf %>%
    filter(product %in% input$cp_sel_product) %>%
    mutate(score_levels = case_when(between(avg_score,0,35.49) ~ '0-35',
                                    between(avg_score,35.5,70.49) ~ '36-70',
                                    between(avg_score,70.5,100) ~ '71-100') %>%
             factor(levels = c('0-35','36-70','71-100'))) %>%
    select(-product, -avg_score)
  
  df_2 <- cp_topic_lesson_rdf %>%
    filter(product %in% input$cp_sel_product) %>%
    select(-product)
  
  label <- labs(
    title = paste0('Product: ', input$cp_sel_product)
  )
  
  list(data = df, y_label_data = df_2, label = label)
  
})


#----performance dotchart plot data----

cp_perf_dotchart_data <- reactive({
  req(!is.null(input$cp_sel_product))
  
  df <- cp_perf_dotchart_rdf %>%
    filter(product %in% input$cp_sel_product) %>%
    mutate(score_levels = case_when(between(avg_score,0,35.49) ~ '0-35',
                                    between(avg_score,35.5,70.49) ~ '36-70',
                                    between(avg_score,70.5,100) ~ '71-100') %>%
             factor(levels = c('0-35','36-70','71-100'))) %>%
    select(-product, -title, -avg_score)
  
  df_2 <- cp_topic_content_rdf %>%
    filter(product %in% input$cp_sel_product) %>%
    select(-product, -title)
  
  label <- labs(
    title = paste0('Product: ', input$cp_sel_product)
  )
  
  list(data = df, y_label_data = df_2, label = label)
  
})

#-----pacing summary table data------

cp_pacing_table_data <- reactive({
  req(!is.null(input$cp_sel_product), !is.null(input$cp_plots), !is.null(input$cp_usertype))
  
  if(input$cp_plots == 'cp_usage_heatmap'){
    
    df <- cp_usage_heatmap_rdf %>%
      filter(product %in% input$cp_sel_product, userroletype %in% input$cp_usertype) %>%
      select(-product,-userroletype) %>%
      rename('Curriculum' = title, 'Date(binned by week)' = date_binned_by_week , '# Users' = n_users,
             '# Events' = n_events, '# Events per User' = n_events_per_user)
  }
  
  else if(input$cp_plots == 'cp_perf_heatmap'){
    
    df <- cp_perf_heatmap_rdf %>%
      filter(product %in% input$cp_sel_product) %>%
      select(-product) %>%
      rename('Curriculum' = title, 'Date(binned by week)' = date_binned_by_week,
             '# Students' = n_users, 'Avg Percent Score' = avg_score)
  }
  
  else if(input$cp_plots == 'cp_perf_dotchart'){
    
    df <- cp_perf_dotchart_rdf %>%
      filter(product %in% input$cp_sel_product) %>%
      select(-product, -title) %>%
      rename('Curriculum' = content, 'Date(binned by week)' = date_binned_by_week,
             '# Students' = n_users, 'Avg Percent Score' = avg_score)
  }
  
  df_2 <- df %>%
    mutate(`Date(binned by week)` = paste0(day(`Date(binned by week)`),' ',month.abb[month(`Date(binned by week)`)],', ',year(`Date(binned by week)`)))
  
  df_2
})

#-----OUTPUT ELEMENTS-----
#-----usage heatmap plot-------

output$cp_plot_1 <- renderPlot({
  req(!is.null(input$cp_usertype), !is.null(input$cp_fill_by))
  
  usage_heatmap_pacing_plot(cp_usage_heatmap_data()$data, cp_usage_heatmap_data()$y_label_data,
                            input$cp_usertype, input$cp_fill_by) + 
    cp_usage_heatmap_data()$label
})

#-----performance heatmap plot----

output$cp_plot_2 <- renderPlot({
  req(!is.null(input$cp_usertype))
  
  performance_heatmap_pacing_plot(cp_perf_heatmap_data()$data, cp_perf_heatmap_data()$y_label_data) +
    cp_perf_heatmap_data()$label
})

#-----performance dotchart plot----
output$cp_plot_3 <- renderPlot({
  req(!is.null(input$cp_usertype))
  
  performance_dotchart_pacing_plot(cp_perf_dotchart_data()$data, cp_perf_dotchart_data()$y_label_data) +
    cp_perf_dotchart_data()$label
})

#-----pacing summary table-----

output$cp_pacing_table <- DT::renderDataTable({
  req(!is.null(input$cp_sel_product), !is.null(input$cp_plots))
  
  sub_cols <- c('cp_usage_heatmap' = 2, 'cp_perf_heatmap' = 1, 'cp_perf_dotchart' = 1)
  
  ncols <- ncol(cp_pacing_table_data())
  columns <- colnames(cp_pacing_table_data())[(ncols-sub_cols[input$cp_plots]):ncols]
  break_col <- break_col_func(cp_pacing_table_data(), columns)
  
  brks <- break_col$brks
  clrs <- break_col$clrs
  
  init_tbl <- DT::datatable(cp_pacing_table_data(), filter = 'top', options = list(
    pageLength = 25, autoWidth = TRUE, sDom  = '<"top">lrt<"bottom">ip'
  ), class = 'cell-border stripe')
  
  purrr::reduce(columns, function(x, y) {
    DT::formatStyle(x, y, backgroundColor = DT::styleInterval(brks[[y]], clrs[[y]]))
    
  }, .init = init_tbl)
})

