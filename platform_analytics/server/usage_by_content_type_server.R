output$ubct_usage_type_options_ui <- renderUI({
  req(!is.null(input$ubct_usage_type))
  
  if(input$ubct_usage_type == 'Subject'){
    selectInput(inputId = 'ubct_usage_type_sub_options', label = 'Select Subject:', choices = subjects)
  }
  else if(input$ubct_usage_type == 'Grade Band'){
    selectInput(inputId = 'ubct_usage_type_gb_options', label = 'Select Grade Band:', choices = grade_bands)
  }
  else if(input$ubct_usage_type == 'Product Group'){
    selectInput(inputId = 'ubct_usage_type_pg_options', label = 'Select Product Group:', choices = product_groups)
  }
})

ubct_plot_1_data <- reactive({
  req(!is.null(input$ubct_usage_type))
  
  if(input$ubct_usage_type == 'Subject'){
    req(!is.null(input$ubct_usage_type_sub_options))
    
    df <- ubct_by_sub_rdf %>%
      filter(subject %in% input$ubct_usage_type_sub_options)
    
    label <- labs(
      title = paste0(input$ubct_usage_type,': ',input$ubct_usage_type_sub_options),
      subtitle = input$ou_select_year
    )
  }
  else if(input$ubct_usage_type == 'Grade Band'){
    req(!is.null(input$ubct_usage_type_gb_options))
    
    df <- ubct_by_gb_rdf %>%
      filter(grade_band %in% input$ubct_usage_type_gb_options)
    
    label <- labs(
      title = paste0(input$ubct_usage_type,': ',input$ubct_usage_type_gb_options),
      subtitle = input$ou_select_year
    )
  }
  else if(input$ubct_usage_type == 'Product Group'){
    req(!is.null(input$ubct_usage_type_pg_options))
    
    df <- ubct_by_pg_rdf %>%
      filter(product_group %in% input$ubct_usage_type_pg_options)
    
    label <- labs(
      title = paste0(input$ubct_usage_type,': ',input$ubct_usage_type_pg_options),
      subtitle = input$ou_select_year
    )
  }
  
  list(data = df, label = label)
})

ubct_plot_1_select_color <- reactive({
  req(!is.null(input$ubct_usage_type), !is.null(ubct_plot_1_data()$data))
  
  name <- unique(ubct_plot_1_data()$data$contenttype)
  pal <- c('#66C5CC','#F89C74','#87C55F','#9EB9F3','#FE88B1','#C9DB74','#8BE0A4')
  color_pal <- sample(pal)[1:length(name)]
  names(color_pal) <- name
  color_pal
})

ubct_plot_2_data <- reactive({
  req(!is.null(input$ubct_usage_type), !is.null(input$ubct_usage_metric))
  
  if(input$ubct_usage_type == 'Subject'){
    
    df <- ubct_plot_2_by_sub_rdf %>%
      select(subject, contenttype, input$ubct_usage_metric) %>%
      rename(y_axis_col = subject, value = input$ubct_usage_metric)
    
  }
  else if(input$ubct_usage_type == 'Grade Band'){
    
    df <- ubct_plot_2_by_gb_rdf %>%
      select(grade_band, contenttype, input$ubct_usage_metric) %>%
      rename(y_axis_col = grade_band, value = input$ubct_usage_metric)
  }
  else if(input$ubct_usage_type == 'Product Group'){
    
    df <- ubct_plot_2_by_pg_rdf %>%
      select(product_group, contenttype, input$ubct_usage_metric) %>%
      rename(y_axis_col = product_group, value = input$ubct_usage_metric)
  }
  
  df
})


ubct_table_data <- reactive({
  req(!is.null(input$ubct_usage_type))

  if(input$ubct_usage_type == 'Subject'){
    
    df <- ubct_table_by_sub_rdf %>%
      arrange(desc(n_sessions))
  }
  else if(input$ubct_usage_type == 'Grade Band'){
    df <- ubct_table_by_gb_rdf %>%
      arrange(desc(n_sessions))
  }
  else if(input$ubct_usage_type == 'Product Group'){
    df <- ubct_table_by_pg_rdf %>%
      arrange(desc(n_sessions))
  }
  
  col_mappings <- c(subject = 'Subject', grade_band = 'Grade Band', product_group = 'Product Group',
                    n_districts = '# Districts', n_schools = '# Schools', n_sessions = '# Sessions',
                    n_students = '# Students', n_teachers = '# Teachers', student_events = 'Student Events',
                    teacher_events = 'Teacher Events', contenttype = 'Content Type')
  
  new_col_names <- col_mappings[names(df)]
  names(df) <- new_col_names
  
  df
})

output$ubct_plot_1 <- renderPlot({
  
  content_type_usage_bar_chart(ubct_plot_1_data()$data, ubct_plot_1_select_color()) +
    ubct_plot_1_data()$label
})

output$ubct_plot_2 <- renderPlot({
  content_type_usage_tile_plot(ubct_plot_2_data(),input$ubct_usage_metric, input$ubct_usage_type)
})


output$ubct_table <- DT::renderDataTable({
  
  ncols <- ncol(ubct_table_data())
  columns <- colnames(ubct_table_data())[(ncols-6):ncols]
  break_col <- break_col_func(ubct_table_data(), columns)
  
  brks <- break_col$brks
  clrs <- break_col$clrs
  
  init_tbl <- DT::datatable(ubct_table_data(), filter = 'top', options = list(
    pageLength = 10, autoWidth = TRUE, sDom  = '<"top">lrt<"bottom">ip'
  ))
  
  purrr::reduce(columns, function(x, y) {
    DT::formatStyle(x, y, backgroundColor = DT::styleInterval(brks[[y]], clrs[[y]]))
    
  }, .init = init_tbl)
  
})