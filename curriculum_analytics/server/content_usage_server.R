#----UI ELEMENTS------
#----usage type options----
output$cu_usage_type_options_ui <- renderUI({
  req(!is.null(input$cu_usage_type))
  
  if(input$cu_usage_type == 'By Product'){
    x <- product_group_products_rdf %>%
      filter(product_group %in% input$ou_select_pg) %>%
      .$product
    
    selectInput(inputId = 'cu_usage_type_product_options', label = 'Product:', choices = x)
  }
})
#-----content summary-------
output$cu_content_summary_title_ui <- renderUI({
  req(!is.null(input$cu_usage_type), !is.null(input$ou_select_pg))
  
  fluidPage(
    if(input$cu_usage_type == 'Overall'){
      
      h5(paste0('Product Group: ', input$ou_select_pg))
    }
    else if(input$cu_usage_type == 'By Product'){
      req(!is.null(input$cu_usage_type_product_options))
      
      h5(paste0('Product: ', input$cu_usage_type_product_options))
    }
    ,
    h6(input$ou_select_year)
  )  
})

output$cu_content_summary_ui <- renderUI({
  
  fluidPage(
    fluidRow(infoBoxOutput("cu_n_content_items"), infoBoxOutput("cu_avg_contents_per_student"), infoBoxOutput("cu_avg_contents_per_teacher")),
    br(),
    fluidRow(
      box(title = "Teacher", background = "blue", solidHeader = TRUE,
          plotOutput("cu_content_summary_teacher_plot", height = '400px', width = '100%')),
      box(title = "Student", background = "blue", solidHeader = TRUE,
          plotOutput("cu_content_summary_student_plot", height = '400px', width = '100%'))
    )
  )
})

#----REACTIVE COMPONENTS------
#----content summary pie plot data----
cu_content_summary_data <- reactive({
  req(!is.null(input$ou_select_pg), !is.null(input$cu_usage_type))
  
  if(input$cu_usage_type == 'Overall'){
    df <- cu_overall_users_rdf %>%
      filter(product_group %in% input$ou_select_pg) %>%
      rename(summariser = feature, filler = perc_users) %>%
      select(summariser, userroletype, filler, color)
  }
  
  else if(input$cu_usage_type == 'By Product'){
    req(!is.null(input$cu_usage_type_product_options))
    
    df <- cu_by_product_users_rdf %>%
      filter(product %in% input$cu_usage_type_product_options) %>%
      rename(summariser = feature, filler = perc_users) %>%
      select(summariser, userroletype, filler, color)
  }
  
  df
})

#-----content pop plot data----
cu_content_pop_data <- reactive({
  req(!is.null(input$ou_select_pg), !is.null(input$cu_usage_type), !is.null(input$ou_select_year))
  
  if(input$cu_usage_type == 'Overall'){
    df <- cu_overall_content_pop_rdf %>%
      filter(product_group %in% input$ou_select_pg)
    
    label <- labs(
      title = paste0('Product Group: ', input$ou_select_pg),
      subtitle = input$ou_select_year
    )
  }
  
  else if(input$cu_usage_type == 'By Product'){
    req(!is.null(input$cu_usage_type_product_options))
    
    df <- cu_by_product_content_pop_rdf %>%
      filter(product %in% input$cu_usage_type_product_options)
    
    label <- labs(
      title = paste0('Product: ', input$cu_usage_type_product_options),
      subtitle = input$ou_select_year
    )
  }
  
  list(data = df, label = label)
})

#-----content type usage-----
cu_content_type_usage_data <- reactive({
  req(!is.null(input$ou_select_pg), !is.null(input$cu_usage_type),
      !is.null(input$ou_select_year), !is.null(input$cu_usage_metric))
  
  if(input$cu_usage_type == 'Overall'){
    df <- cu_overall_ct_usage_rdf %>%
      filter(product_group %in% input$ou_select_pg)
    
    label <- labs(
      title = paste0('Product Group: ',input$ou_select_pg),
      subtitle = input$ou_select_year
    )
  }
  else if(input$cu_usage_type == 'By Product'){
    req(!is.null(input$cu_usage_type_product_options))
    
    df <- cu_by_product_ct_usage_rdf %>%
      filter(product %in% input$cu_usage_type_product_options)
    
    label <- labs(
      title = paste0('Product: ',input$cu_usage_type_product_options),
      subtitle = input$ou_select_year
    )
  }
  
  if(input$cu_usage_metric == '# of users'){
    df_2 <- df %>%
      rename(y_axis_col = n_users) %>%
      select(userroletype, contenttype, y_axis_col)
  }
  else if(input$cu_usage_metric == '# events per user'){
    df_2 <- df %>%
      rename(y_axis_col = n_events_per_user) %>%
      select(userroletype, contenttype, y_axis_col)
  }
  else if(input$cu_usage_metric == 'Unique content items'){
    df_2 <- df %>%
      rename(y_axis_col = n_contents) %>%
      select(userroletype, contenttype, y_axis_col)
  }
  
  list(data = df_2, label = label)
})

#------content type usage select color-----
cu_content_type_usage_sel_col <- reactive({
  req(!is.null(input$ou_select_pg), !is.null(input$cu_usage_type),
      !is.null(input$ou_select_year), !is.null(input$cu_usage_metric),
      !is.null(cu_content_type_usage_data()$data))
  
  name <- unique(cu_content_type_usage_data()$data$contenttype)
  pal <- c('#66C5CC','#F89C74','#87C55F','#9EB9F3','#FE88B1','#C9DB74','#8BE0A4')
  color_pal <- sample(pal)[1:length(name)]
  names(color_pal) <- name
  color_pal
})

#----content type uot data----
cu_content_type_uot_data <- reactive({
  req(!is.null(input$ou_select_pg), !is.null(input$cu_usage_type),
      !is.null(input$ou_select_year), !is.null(input$cu_fill_by))
  
  if(input$cu_usage_type == 'Overall'){
    df <- cu_overall_ct_uot_rdf %>%
      filter(product_group %in% input$ou_select_pg)
    
    label <- labs(
      title = paste0('Product Group: ',input$ou_select_pg),
      subtitle = input$ou_select_year
    )
  }
  else if(input$cu_usage_type == 'By Product'){
    req(!is.null(input$cu_usage_type_product_options))
    
    df <- cu_by_product_ct_uot_rdf %>%
      filter(product %in% input$cu_usage_type_product_options)
    
    label <- labs(
      title = paste0('Product: ',input$cu_usage_type_product_options),
      subtitle = input$ou_select_year
    )
  }
  
  if(input$cu_fill_by == '% Users'){
    df_2 <- df %>%
      rename(fill_col = perc_users) %>%
      select(contenttype, date_binned_by_week, fill_col)
  }
  else if(input$cu_fill_by == 'Total Users'){
    df_2 <- df %>%
      rename(fill_col = n_users) %>%
      select(contenttype, date_binned_by_week, fill_col)
  }
  
  list(data = df_2, label = label)
})

#-------content usage table data------

cu_table_data <- reactive({
  req(!is.null(input$ou_select_pg), !is.null(input$cu_usage_type))
  
  if(input$cu_usage_type == 'Overall'){
   
    df <- cu_table_data_rdf %>%
      filter(product_group %in% input$ou_select_pg) %>%
      mutate(product = factor(product, levels = product_group_products_rdf %>%
                                filter(product_group %in% input$ou_select_pg) %>%
                                .$product),
             content = factor(content, levels = gtools::mixedsort(unique(content)))) %>%
      arrange(product, content)
  }
  
  else if(input$cu_usage_type == 'By Product'){
    req(!is.null(input$cu_usage_type_product_options))
    
    df <- cu_table_data_rdf %>%
      filter(product %in% input$cu_usage_type_product_options) %>%
      mutate(content = factor(content, levels = gtools::mixedsort(content))) %>%
      arrange(content)
  }
  
  
  col_name_mapping <- c(product_group = 'Product Group', product = 'Product', content = 'Content ID',
                        title = 'Title', contenttype = 'Content Type', n_students = 'Students',
                        n_teachers = 'Teachers', avg_time = 'Avg Time Spent', 
                        sd_time = 'SD Time Spent')
  
  old_names <- colnames(df)
  new_names <- col_name_mapping[old_names]
  colnames(df) <- new_names
  
  df
})


#------OUTPUT ELEMENTS-------
#----infobox output-----

output$cu_n_content_items <- renderInfoBox({
  req(!is.null(input$ou_select_pg), !is.null(input$cu_usage_type))
  
  if(input$cu_usage_type == 'Overall'){
    infoBox(format(as.numeric(cu_overall_rdf$n_contents[cu_overall_rdf$product_group == input$ou_select_pg]), big.mark=","), "Content Items", 
            color = "orange", icon = icon("fas fa-book-open"), fill = TRUE)
  }
  
  else if(input$cu_usage_type == 'By Product'){
    req(!is.null(input$cu_usage_type_product_options))
    
    infoBox(format(as.numeric(cu_by_product_rdf$n_contents[cu_by_product_rdf$product == input$cu_usage_type_product_options]), big.mark=","),
            "Content Items", color = "orange", icon = icon("fas fa-book-open"), fill = TRUE)
  }
})

output$cu_avg_contents_per_student <- renderInfoBox({
  req(!is.null(input$ou_select_pg), !is.null(input$cu_usage_type))
  
  if(input$cu_usage_type == 'Overall'){
    infoBox(format(as.numeric(cu_overall_rdf$avg_contents_per_student[cu_overall_rdf$product_group == input$ou_select_pg]), big.mark=","),
            "Avg Contents per Student", color = "orange", icon = icon("fas fa-users"), fill = TRUE)
  }
  
  else if(input$cu_usage_type == 'By Product'){
    req(!is.null(input$cu_usage_type_product_options))
    
    infoBox(format(as.numeric(cu_by_product_rdf$avg_contents_per_student[cu_by_product_rdf$product == input$cu_usage_type_product_options]), big.mark=","),
            "Avg Contents per Student", color = "orange", icon = icon("fas fa-users"), fill = TRUE)
  }
})

output$cu_avg_contents_per_teacher <- renderInfoBox({
  req(!is.null(input$ou_select_pg), !is.null(input$cu_usage_type))
  
  if(input$cu_usage_type == 'Overall'){
    infoBox(format(as.numeric(cu_overall_rdf$avg_contents_per_teacher[cu_overall_rdf$product_group == input$ou_select_pg]), big.mark=","),
            "Avg Contents per Teacher", color = "orange", icon = icon("fas fa-chalkboard-teacher"), fill = TRUE)
  }
  
  else if(input$cu_usage_type == 'By Product'){
    req(!is.null(input$cu_usage_type_product_options))
    
    infoBox(format(as.numeric(cu_by_product_rdf$avg_contents_per_teacher[cu_by_product_rdf$product == input$cu_usage_type_product_options]), big.mark=","),
            "Avg Contents per Teacher", color = "orange", icon = icon("fas fa-chalkboard-teacher"), fill = TRUE)
  }
})

#---content summary pie plot----

output$cu_content_summary_teacher_plot <- renderPlot({
  req(!is.null(input$ou_select_pg), !is.null(input$cu_usage_type))

  content_usage_summary_pie_plot(cu_content_summary_data(), 'Teacher')
})

output$cu_content_summary_student_plot <- renderPlot({
  req(!is.null(input$ou_select_pg), !is.null(input$cu_usage_type))

  content_usage_summary_pie_plot(cu_content_summary_data(), 'Student')
})

#----content pop plot-----

output$cu_content_pop_plot <- renderPlot({
  req(!is.null(input$ou_select_pg), !is.null(input$cu_usage_type))
  
  content_popularity_plot(cu_content_pop_data()$data) +
    cu_content_pop_data()$label
  
})

#-----content type usage plot-----
output$cu_content_type_usage_plot <- renderPlot({
  req(!is.null(input$ou_select_pg), !is.null(input$cu_usage_type), !is.null(input$cu_usage_metric))
  
  content_type_usage_plot(cu_content_type_usage_data()$data, cu_content_type_usage_sel_col(), input$cu_usage_metric) +
    cu_content_type_usage_data()$label
})

#-----content type uot plot-----
output$cu_content_type_usage_heatmap <- renderPlot({
  req(!is.null(input$ou_select_pg), !is.null(input$cu_usage_type), !is.null(input$cu_fill_by))
  
  content_type_uot_tile_plot(cu_content_type_uot_data()$data, input$cu_fill_by) +
    cu_content_type_uot_data()$label
})

#-----content usage table-----
output$cu_table <- DT::renderDataTable({
  req(!is.null(input$ou_select_pg), !is.null(input$cu_usage_type))
  
  ncols <- ncol(cu_table_data())
  columns <- colnames(cu_table_data())[(ncols-3):ncols]
  break_col <- break_col_func(cu_table_data(), columns)
  
  brks <- break_col$brks
  clrs <- break_col$clrs
  
  init_tbl <- DT::datatable(cu_table_data(), filter = 'top', options = list(
    pageLength = 10, autoWidth = TRUE, sDom  = '<"top">lrt<"bottom">ip'
  ), class = 'cell-border stripe')
  
  purrr::reduce(columns, function(x, y) {
    DT::formatStyle(x, y, backgroundColor = DT::styleInterval(brks[[y]], clrs[[y]]))
    
  }, .init = init_tbl)
})


