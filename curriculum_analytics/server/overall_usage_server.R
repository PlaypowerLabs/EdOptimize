#-------UI ELEMENTS---------
output$ou_infobox_ui <- renderUI({
  req(!is.null(input$ou_select_year), !is.null(input$ou_select_pg))
  
  fluidPage(
    fluidRow(infoBoxOutput("ou_n_products"), infoBoxOutput("ou_n_students"), infoBoxOutput("ou_n_teachers")), 
    fluidRow(infoBoxOutput("ou_n_contents_accessed"), infoBoxOutput("ou_n_schools"), infoBoxOutput("ou_n_districts")),
    br()
  )
})

output$ou_act_weeks_plot_ui <- renderUI({
  req(!is.null(input$ou_act_weeks_filter), !is.null(input$ou_select_pg), !is.null(input$ou_select_year))
  
  fluidPage(
    h5(paste0('Product Group: ', input$ou_select_pg)),
    h6(input$ou_select_year),
    fluidRow(box(title = "Teacher", background = "blue", solidHeader = TRUE,
                 plotOutput("ou_act_weeks_teacher_plot", height = '400px', width = '100%')),
             box(title = "Student", background = "blue", solidHeader = TRUE,
                 plotOutput("ou_act_weeks_student_plot", height = '400px', width = '100%')))
  )
  
})

#-----------REACTIVE DATA PREP---------

ou_productwise_data <- reactive({
  req(!is.null(input$ou_select_pg), !is.null(input$ou_select_year), !is.null(input$ou_usage_metric))
  
  if(input$ou_usage_metric == '# Users'){
    
    df <- ou_productwise_rdf %>%
      filter(product_group %in% input$ou_select_pg) %>%
      mutate(product = factor(product, levels = product_group_products_rdf %>%
                                filter(product_group %in% input$ou_select_pg) %>%
                                .$product),
             product_labels = str_wrap(product, width = 15) %>%
               factor(levels = levels(product) %>%
                        str_wrap(width = 15))) %>%
      select(-n_sessions) %>%
      rename(x_axis_col = n_users)
    
  }
  else if(input$ou_usage_metric == '# Sessions'){
    
    df <- ou_productwise_rdf %>%
      filter(product_group %in% input$ou_select_pg) %>%
      mutate(product = factor(product, levels = product_group_products_rdf %>%
                                filter(product_group %in% input$ou_select_pg) %>%
                                .$product),
             product_labels = str_wrap(product, width = 15) %>%
               factor(levels = levels(product) %>%
                        str_wrap(width = 15))) %>%
      select(-n_users) %>%
      rename(x_axis_col = n_sessions)
  }
  
  table_data <- ou_productwise_rdf %>%
    filter(product_group %in% input$ou_select_pg) %>%
    select(-product_group, -n_sessions) %>%
    pivot_wider(names_from = userroletype, values_from = n_users, values_fill = list(n_users = 0)) %>%
    mutate(product = factor(product, levels = product_group_products_rdf %>%
                              filter(product_group %in% input$ou_select_pg) %>%
                              .$product)) %>%
    arrange(product) %>%
    rename(Product = product, '# Students' = Student, '# Teachers' = Teacher)

  label <- labs(
    title = paste0('Product Group: ', input$ou_select_pg),
    subtitle = input$ou_select_year
  )
  
  list(data = df, label = label, table_data = table_data)
})

ou_plot1_sel_colpal <- reactive({
  req(!is.null(input$ou_select_pg))
  
  name <- unique(ou_productwise_data()$data$product)
  pal <- c('#7ed6df','#FFC312','#ff6b81','#badc58')
  color_pal <- sample(pal)[1:length(name)]
  names(color_pal) <- name
  color_pal
})

ou_act_weeks_data <- reactive({
  req(!is.null(input$ou_act_weeks_filter), !is.null(input$ou_select_pg))
  
  if(input$ou_act_weeks_filter == '% of Active Weeks'){
    df <- ou_perc_act_weeks_rdf %>%
      filter(product_group %in% input$ou_select_pg) %>%
      rename(summariser = act_weeks_label, filler = perc_users)
  }
  else if(input$ou_act_weeks_filter == 'Total Active Weeks'){
    df <- ou_act_weeks_rdf %>%
      filter(product_group %in% input$ou_select_pg) %>%
      rename(summariser = act_weeks_label, filler = perc_users)
  }
  df
})

#----------OUTPUT ELEMENTS--------
output$ou_n_products <- renderInfoBox({
  infoBox(format(as.numeric(ou_infobox_rdf$n_products[ou_infobox_rdf$product_group == input$ou_select_pg]), big.mark=","), "Number of Products", 
          color = "teal", icon = icon("fas fa-book"), fill = TRUE)
})

output$ou_n_students <- renderInfoBox({
  infoBox(format(as.numeric(ou_infobox_rdf$n_students[ou_infobox_rdf$product_group == input$ou_select_pg]), big.mark=","), "Number of Students", 
          color = "teal", icon = icon("fas fa-users"), fill = TRUE)
})

output$ou_n_teachers <- renderInfoBox({
  infoBox(format(as.numeric(ou_infobox_rdf$n_teachers[ou_infobox_rdf$product_group == input$ou_select_pg]), big.mark=","), "Number of Teachers", 
          color = "teal", icon = icon("fas fa-chalkboard-teacher"), fill = TRUE)
})

output$ou_n_contents_accessed <- renderInfoBox({
  infoBox(format(as.numeric(ou_infobox_rdf$n_contents_accessed[ou_infobox_rdf$product_group == input$ou_select_pg]), big.mark=","), "Contents Accessed", 
          color = "teal", icon = icon("fas fa-book-open"), fill = TRUE)
})

output$ou_n_schools <- renderInfoBox({
  infoBox(format(as.numeric(ou_infobox_rdf$n_schools[ou_infobox_rdf$product_group == input$ou_select_pg]), big.mark=","), "Number of Schools", 
          color = "teal", icon = icon("fas fa-school"), fill = TRUE)
})

output$ou_n_districts <- renderInfoBox({
  infoBox(format(as.numeric(ou_infobox_rdf$n_districts[ou_infobox_rdf$product_group == input$ou_select_pg]), big.mark=","), "Number of Districts", 
          color = "teal", icon = icon("fas fa-university"), fill = TRUE)
})

output$ou_productwise_plot <- renderPlot({
  req(!is.null(input$ou_select_pg), !is.null(input$ou_usage_metric))

  product_wise_vertical_plot(ou_productwise_data()$data, ou_plot1_sel_colpal(), input$ou_usage_metric) +
    ou_productwise_data()$label
})

output$ou_productwise_table <- DT::renderDataTable({
  
  req(!is.null(input$ou_select_pg))
  
  ncols <- ncol(ou_productwise_data()$table_data)
  columns <- colnames(ou_productwise_data()$table_data)[(ncols-1):ncols]
  break_col <- break_col_func(ou_productwise_data()$table_data, columns)
  
  brks <- break_col$brks
  clrs <- break_col$clrs
  
  init_tbl <- DT::datatable(ou_productwise_data()$table_data, filter = 'top', rownames = FALSE, options = list(
    pageLength = 10, autoWidth = TRUE, sDom  = '<"top">lrt<"bottom">ip'
  ), class = 'cell-border stripe')
  
  
  
  purrr::reduce(columns, function(x, y) {
    DT::formatStyle(x, y, backgroundColor = DT::styleInterval(brks[[y]], clrs[[y]]))

  }, .init = init_tbl)
  
})

output$ou_act_weeks_teacher_plot <- renderPlot({
  req(!is.null(input$ou_act_weeks_filter), !is.null(input$ou_select_pg))
  
  active_weeks_pie_chart(ou_act_weeks_data(), 'Teacher')
})

output$ou_act_weeks_student_plot <- renderPlot({
  req(!is.null(input$ou_act_weeks_filter), !is.null(input$ou_select_pg))
  
  active_weeks_pie_chart(ou_act_weeks_data(), 'Student')
})

