output$ubp_uot_product_ui <- renderUI({
  req(!is.null(input$ubp_uot_deeper_dive), !is.null(input$ubp_product_group))
  
  if(input$ubp_uot_deeper_dive == 'Yes'){
    x <- product_group_products_rdf %>%
      filter(product_group %in% c(input$ubp_product_group)) %>%
      .$product
    
    selectInput(inputId = 'ubp_uot_product', label = 'Select Product:', choices = x)
    
  }
  
})

output$ubp_cu_product_ui <- renderUI({
  req(!is.null(input$ubp_cu_deeper_dive), !is.null(input$ubp_product_group))
  
  if(input$ubp_cu_deeper_dive == 'Yes'){
    x <- product_group_products_rdf %>%
      filter(product_group %in% c(input$ubp_product_group)) %>%
      .$product
    
    selectInput(inputId = 'ubp_cu_product', label = 'Select Product:', choices = x)
    
  }
})

output$ubp_info_boxes_ui <- renderUI({
  fluidPage(
    fluidRow(infoBoxOutput('ubp_n_students'),infoBoxOutput('ubp_n_teachers'),infoBoxOutput('ubp_n_products'))
  )
})


product_wise_plot_data <- reactive({
  req(!is.null(input$ubp_product_group))
  
  df <- ubp_product_data_rdf %>%
    filter(product_group %in% input$ubp_product_group) %>%
    mutate(product = factor(product,
                            levels = product_group_products_rdf %>%
                              filter(product_group %in% input$ubp_product_group) %>%
                              .$product) ,
           product_labels = str_wrap(product, width = 15) %>%
             factor(levels = str_wrap(levels(product), width = 15)))
  
  df
})

select_color <- reactive({
  req(!is.null(input$ubp_product_group))
  
  name <- unique(product_wise_plot_data()$product)
  pal <- c('#7ed6df','#FFC312','#ff6b81','#badc58')
  color_pal <- sample(pal)[1:length(name)]
  names(color_pal) <- name
  color_pal
})


ubp_usage_over_time_plot_data <- reactive({
  req(!is.null(input$ubp_product_group), !is.null(input$ubp_uot_deeper_dive), !is.null(input$ou_select_year))
  
  if(input$ubp_uot_deeper_dive == 'Yes'){
    req(!is.null(input$ubp_uot_product))
    
    df <- ubp_products_usage_over_time_rdf %>%
      filter(product %in% input$ubp_uot_product) %>%
      rename('New' = new, 'Returning' = returning, dateid = date_binned_by_week) %>%
      pivot_longer(cols = c(New, Returning), names_to = 'feature', values_to = 'value') %>%
      mutate(usertype = paste0(feature,' ',userroletype))
    
    label <- labs(
      title = paste0('Product: ',input$ubp_uot_product),
      subtitle = input$ou_select_year
    )
  }
  
  else if(input$ubp_uot_deeper_dive == 'No'){
    
    df <- ubp_pg_usage_over_time_rdf %>%
      filter(product_group %in% input$ubp_product_group) %>%
      rename('New' = new, 'Returning' = returning, dateid = date_binned_by_week) %>%
      pivot_longer(cols = c(New, Returning), names_to = 'feature', values_to = 'value') %>%
      mutate(usertype = paste0(feature,' ',userroletype))
    
    label <- labs(
      title = paste0('Product Group: ',input$ubp_product_group),
      subtitle = input$ou_select_year
    )
  }
  
  list(data = df, label = label)
})

ubp_product_comparison_data <- reactive({
  req(!is.null(input$ubp_product_group))
  
  df <- ubp_compare_product_data_rdf %>%
    filter(product_group %in% input$ubp_product_group) %>%
    select(-product_group) %>%
    rename('# Districts' = n_districts, '# Schools' = n_schools, '# Sessions' = n_sessions,
           '# Students' = n_students, '# Teachers' = n_teachers, 'Student Events' = n_stud_events,
           'Teacher Events' = n_teacher_events, 'Total Events' = total_events) %>%
    pivot_longer(cols = -product, names_to = 'Feature', values_to = 'value')
  
  df_1 <- df %>%
    pivot_wider(names_from = product, values_from = value) 
  
  df_2 <- df %>%
    mutate(product = factor(product,
                            levels = product_group_products_rdf %>%
                              filter(product_group %in% input$ubp_product_group) %>%
                              .$product) ,
           product_labels = str_wrap(product, width = 25) %>%
             factor(levels = str_wrap(levels(product), width = 25)))
    
  list(table_data = df_1, plot_data = df_2)
})

select_comparison_color <- reactive({
  req(!is.null(input$ubp_product_group))
  
  name <- unique(ubp_product_comparison_data()$plot_data$product)
  pal <- colorRampPalette(c('#8dd3c7','#f1b6da','#fdb462','#bebada'))
  color_pal <- pal(length(name))
  names(color_pal) <- name
  color_pal
})


ubp_content_area_usage_plot_data <- reactive({
  
  req(!is.null(input$ubp_product_group),
      !is.null(input$ubp_cu_deeper_dive),
      !is.null(input$ou_select_year))
  
  if(input$ubp_cu_deeper_dive == 'Yes'){
    req(!is.null(input$ubp_cu_product))
    
    df <- ubp_content_area_usage_rdf %>%
      filter(product %in% input$ubp_cu_product) %>%
      mutate(content_area_labels = content_area %>%
               str_replace(pattern = 'content_area_',replacement = 'CA ') %>%
               factor(levels = unique(mixedsort(.))))
    
    label <- labs(
      title = paste0('Product: ',input$ubp_cu_product),
      subtitle = input$ou_select_year
    )
    
    height <- 400
  }
  
  else if(input$ubp_cu_deeper_dive == 'No'){
    
    df <- ubp_content_area_usage_rdf %>%
      filter(product_group %in% input$ubp_product_group) %>%
      mutate(content_area_labels = content_area %>%
               str_replace(pattern = 'content_area_',replacement = 'CA ') %>%
               factor(levels = unique(mixedsort(.))))
    
    label <- labs(
      title = paste0('Product Group: ',input$ubp_product_group),
      subtitle = input$ou_select_year
    )
    
    height <- 700
  }
  
  list(data = df, label = label, height = height)
})


ubp_user_behav_data <- reactive({
  req(!is.null(input$ubp_product_group), !is.null(input$ubp_user_behav))
  if(input$ubp_user_behav == '# Active Days'){
    df <- ubp_user_act_days_rdf %>%
      filter(product_group %in% input$ubp_product_group) %>%
      rename(x_axis_col = act_days, y_axis_col = n_users)
  }
  
  else if(input$ubp_user_behav == '# Active Weeks'){
    df <- ubp_user_act_weeks_rdf %>%
      filter(product_group %in% input$ubp_product_group) %>%
      rename(x_axis_col = act_weeks, y_axis_col = n_users)
  }
  
  else if(input$ubp_user_behav == 'Session Time'){
    df <- ubp_user_session_time_rdf %>%
      filter(product_group %in% input$ubp_product_group) %>%
      rename(x_axis_col = session_time, y_axis_col = n_sessions)
  }
  
  label <- labs(
    title = paste0('Product Group: ',input$ubp_product_group),
    subtitle = input$ou_select_year
  )
  
  list(data = df, label = label)
})


output$ubp_n_students <- renderInfoBox({
  infoBox(format(as.numeric(ubp_info_box_data_rdf$n_students[ubp_info_box_data_rdf$product_group==input$ubp_product_group]), big.mark=","), "Number of Students", 
          color = "orange", icon = icon("fas fa-users"), fill = TRUE)
})

output$ubp_n_teachers <- renderInfoBox({
  infoBox(format(as.numeric(ubp_info_box_data_rdf$n_teachers[ubp_info_box_data_rdf$product_group==input$ubp_product_group]), big.mark=","), "Number of Teachers", 
          color = "orange", icon = icon("fas fa-chalkboard-teacher"), fill = TRUE)
})

output$ubp_n_products <- renderInfoBox({
  infoBox(format(as.numeric(ubp_info_box_data_rdf$n_products[ubp_info_box_data_rdf$product_group==input$ubp_product_group]), big.mark=","), "Number of Products", 
          color = "orange", icon = icon("fas fa-book-open"), fill = TRUE)
})

output$ubp_product_users_plot <- renderPlot({
  req(!is.null(input$ubp_product_group))
  
  product_wise_users_plot(product_wise_plot_data(),select_color(), input$ubp_product_group)
})

output$ubp_pg_usage_over_time_plot <- renderPlot({
  
  product_group_usage_over_time_plot(ubp_usage_over_time_plot_data()$data) +
    ubp_usage_over_time_plot_data()$label
})

output$ubp_user_behav_plot <- renderPlot({
  
  user_behav_plot(ubp_user_behav_data()$data, input$ubp_user_behav) +
    ubp_user_behav_data()$label
})


output$ubp_product_comparison_table <- DT::renderDataTable({
  req(!is.null(input$ubp_product_group))
  
  ncols <- ncol(ubp_product_comparison_data()$table_data)
  rows <- ubp_product_comparison_data()$table_data$Feature
  columns <- colnames(ubp_product_comparison_data()$table_data)[2:ncols]
  
  out <- break_col_by_row_func(ubp_product_comparison_data()$table_data, rows, columns, ncols)
  
  init_tbl <- DT::datatable(ubp_product_comparison_data()$table_data, rownames = FALSE, filter = 'top', options = list(
    pageLength = 10, autoWidth = TRUE, sDom  = '<"top">lrt<"bottom">ip'
  ))
  
  purrr::reduce(columns, function(x, y) {
    DT::formatStyle(x, y, backgroundColor = DT::styleEqual(out$values_data[[y]], as.vector(unname(out$colors_data[[y]]))))
    
  }, .init = init_tbl)
})

output$ubp_product_comparison_plot <- renderPlot({
  req(!is.null(input$ubp_product_group), !is.null(input$ou_select_year))
  
  product_comparison_plot(ubp_product_comparison_data()$plot_data,input$ubp_product_group,input$ou_select_year, select_comparison_color())
})


output$ubp_content_area_usage_plot <- renderPlot({
  req(!is.null(input$ubp_cu_deeper_dive))
  
  ncols <- c('Yes' = 2, 'No' = 1)
  
  content_usage_plot(ubp_content_area_usage_plot_data()$data, ncols[input$ubp_cu_deeper_dive]) +
    ubp_content_area_usage_plot_data()$label

}, height = function(){
  ubp_content_area_usage_plot_data()$height
})

