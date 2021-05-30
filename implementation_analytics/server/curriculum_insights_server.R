#----UI ELEMENTS-----
#----select product ui-----
output$ci_sel_product_ui <- renderUI({
  req(!is.null(input$select_district))

  x <- district_products %>%
    filter(districtname %in% input$select_district) %>%
    .$product
  
  selectInput(inputId = 'ci_sel_product', label = 'Select Product:', 
              choices = x)
  
})

#----plot ui----
output$ci_plot_ui <- renderUI({
  req(!is.null(input$ci_plot_type))
  
  if(input$ci_plot_type == 'Usage Over Time'){
   
    fluidPage(
      h4('Usage Over Time Plot'),
      hr(style="border-top: 1px solid #000000;"),
      tabsetPanel(id = 'ci_uot_plots',
                  tabPanel(title = 'New vs Returning Users', value = 'ci_nvr',
                           plotOutput("ci_nvr_plot", height = 'auto', width = "100%") %>%
                             withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')),
                  tabPanel(title = 'Avg Session Duration', value = 'ci_asd',
                           plotOutput("ci_asd_plot", height = 'auto', width = "100%") %>%
                             withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')),
                  tabPanel(title = 'Avg Events per User', value = 'ci_aeu',
                           plotOutput("ci_aeu_plot", height = 'auto', width = "100%") %>%
                             withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')))
    ) 
  }
  
  else if(input$ci_plot_type == 'Curriculum Pacing'){
    
    fluidPage(
      h4('Pacing Chart'),
      hr(style="border-top: 1px solid #000000;"),
      tabsetPanel(id = 'ci_cp_plots',
                  tabPanel(title = 'Usage Heatmap', value = 'ci_usage_heatmap',
                           plotOutput("ci_usage_heatmap_plot", height = "700px", width = "100%") %>%
                             withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')),
                  tabPanel(title = 'Performance Heatmap', value = 'ci_perf_heatmap',
                           plotOutput("ci_perf_heatmap_plot", height = "700px", width = "100%") %>%
                             withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')),
                  tabPanel(title = 'Performance Dotchart', value = 'ci_perf_dotchart',
                           plotOutput("ci_perf_dotchart_plot", height = "700px", width = "100%") %>%
                             withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')))
    )
  }
  
})

#----add feature ui------
output$ci_add_feature_ui <- renderUI({
  req(!is.null(input$ci_plot_type), !is.null(input$ci_uot_plots))
  
  if(input$ci_plot_type == 'Usage Over Time' && input$ci_uot_plots == 'ci_nvr'){
    
    checkboxGroupInput(inputId = 'ci_add_feature', label = 'Add Feature to Plot:',
                       choices = c('Leaving User', 'Bouncing User'))
  }
})

#------user type ui----
output$ci_user_type_ui <- renderUI({
  req(!is.null(input$ci_plot_type), !is.null(input$ci_cp_plots))
  
  if(input$ci_plot_type == 'Curriculum Pacing' && input$ci_cp_plots == 'ci_usage_heatmap'){
    
    radioButtons(inputId = 'ci_user_type', label = 'User Type:',
                 choices = c('Student','Teacher'))
  }
})

#-----fill by ui----
output$ci_fill_by_ui <- renderUI({
  req(!is.null(input$ci_plot_type), !is.null(input$ci_cp_plots))
  
  if(input$ci_plot_type == 'Curriculum Pacing' && input$ci_cp_plots == 'ci_usage_heatmap'){
    
    radioButtons(inputId = 'ci_fill_by', label = 'Fill by:',
                 choices = c('# Users', 'Avg Events per User', '# Events'))
  }
})

#----table ui----
output$ci_table_ui <- renderUI({
  req(!is.null(input$ci_plot_type))
  
  if(input$ci_plot_type == 'Usage Over Time'){
    fluidPage(
      h4('Usage Over Time Summary Table'),
      hr(style="border-top: 1px solid #000000;"),
      DT::dataTableOutput(outputId = 'ci_uot_table') %>%
        withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')
    )
  }
  
  else if(input$ci_plot_type == 'Curriculum Pacing'){
    fluidPage(
      h4('Pacing Summary Table'),
      hr(style="border-top: 1px solid #000000;"),
      DT::dataTableOutput(outputId = 'ci_cp_table') %>%
        withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')
    )
  }
})
  
#-----REACTIVE COMPONENTS-------
#---nvr plot data prep-----

ci_nvr_plot_data <- reactive({
  req(!is.null(input$select_district),
      input$ci_sel_product %in% (district_products %>%
                                   filter(districtname %in% input$select_district) %>%
                                   .$product))
  
  df <- ci_nvr_rdf %>%
    filter(districtname %in% input$select_district,
           product %in% input$ci_sel_product) %>%
    select(-districtname, -product, -tot_users)
  
  if(!is.null(input$ci_add_feature)){
    if(length(input$ci_add_feature) == 1){
      if(input$ci_add_feature == 'Leaving User'){
        df_2 <- df %>%
          mutate(return = return-leaving) %>%
          select(-bouncing) %>%
          rename('New User' = new, 'Returning User' = return, 'Leaving User' = leaving)
      }
      
      else if(input$ci_add_feature == 'Bouncing User'){
        df_2 <- df %>%
          mutate(new = new-bouncing) %>%
          select(-leaving) %>%
          rename('New User' = new, 'Returning User' = return, 'Bouncing User' = bouncing)
      }
    }
    
    else if(length(input$ci_add_feature) == 2){
      df_2 <- df %>%
        mutate(new = new-bouncing, return = return-leaving) %>%
        rename('New User' = new, 'Returning User' = return, 'Leaving User' = leaving, 'Bouncing User' = bouncing)
    }
  }
  
  else if(is.null(input$ci_add_feature)){
    df_2 <- df %>%
      select(-leaving, -bouncing) %>%
      rename('New User' = new, 'Returning User' = return)
  }
  
  df_3 <- df_2 %>%
    pivot_longer(cols = c(-userroletype, -date_binned_by_week),
                 names_to = 'feature', values_to = 'value')
  
  label <- labs(title = 'Usage Over Time',
                subtitle = paste0('District: ',input$select_district,'\n',
                                  'Product: ',input$ci_sel_product, '\n',
                                  input$select_year))
  
  height <- if_else(n_distinct(df$userroletype) == 1, 450, 700)
  
  list(data = df_3, label = label, height = height)
})

#----asd plot data prep----
ci_asd_plot_data <- reactive({
  req(!is.null(input$select_district),
      input$ci_sel_product %in% (district_products %>%
                                   filter(districtname %in% input$select_district) %>%
                                   .$product))
  
  df <- ci_asd_rdf %>%
    filter(districtname %in% input$select_district,
           product %in% input$ci_sel_product) %>%
    select(userroletype, date_binned_by_week, avg_duration, perc_users)
  
  label <- labs(title = 'Average Session Duration Over Time',
                subtitle = paste0('District: ',input$select_district,'\n',
                                  'Product: ',input$ci_sel_product, '\n',
                                  input$select_year))
  
  height <- if_else(n_distinct(df$userroletype) == 1, 450, 700)
  
  list(data = df, label = label, height = height)
})

#----aeu plot data prep----
ci_aeu_plot_data <- reactive({
  req(!is.null(input$select_district),
      input$ci_sel_product %in% (district_products %>%
                                   filter(districtname %in% input$select_district) %>%
                                   .$product))
  
  df <- ci_aeu_rdf %>%
    filter(districtname %in% input$select_district,
           product %in% input$ci_sel_product) %>%
    select(userroletype, date_binned_by_week, n_events_per_user, perc_users)
  
  label <- labs(title = 'Average Events Per User Over Time',
                subtitle = paste0('District: ',input$select_district,'\n',
                                  'Product: ',input$ci_sel_product, '\n',
                                  input$select_year))
  
  height <- if_else(n_distinct(df$userroletype) == 1, 450, 700)
  
  list(data = df, label = label, height = height)
})

#-----usage heatmap plot data prep-----
ci_usage_heatmap_plot_data <- reactive({
  req(!is.null(input$select_district),
      input$ci_sel_product %in% (district_products %>%
                                   filter(districtname %in% input$select_district) %>%
                                   .$product),
      !is.null(input$ci_fill_by))
  
  filler_col <- c('# Users' = 'n_users', 'Avg Events per User' = 'n_events_per_user',
                  '# Events' = 'n_events')
  
  df <- ci_usage_heatmap_rdf %>%
    filter(districtname %in% input$select_district,
           product %in% input$ci_sel_product) %>%
    rename('fill_by' = unname(filler_col[input$ci_fill_by])) %>%
    select(userroletype, title, date_binned_by_week, fill_by)
  
  df_2 <- ci_topic_lesson_rdf %>%
    filter(product %in% input$ci_sel_product) %>%
    select(-product)
  
  label <- labs(
    title = paste0('District: ', input$select_district)
  )
  
  list(data = df, y_label_data = df_2, label = label)
  
})

#-----perf heatmap plot data prep-----
ci_perf_heatmap_plot_data <- reactive({
  req(!is.null(input$select_district),
      input$ci_sel_product %in% (district_products %>%
                                   filter(districtname %in% input$select_district) %>%
                                   .$product))
  
  df <- ci_perf_heatmap_rdf %>%
    filter(districtname %in% input$select_district,
           product %in% input$ci_sel_product) %>%
    mutate(score_levels = case_when(between(avg_score,0,35.49) ~ '0-35',
                                    between(avg_score,35.5,70.49) ~ '36-70',
                                    between(avg_score,70.5,100) ~ '71-100') %>%
             factor(levels = c('0-35','36-70','71-100'))) %>%
    select(-districtname, -product, -avg_score)
  
  df_2 <- ci_topic_lesson_rdf %>%
    filter(product %in% input$ci_sel_product) %>%
    select(-product)
  
  label <- labs(
    title = paste0('District: ', input$select_district)
  )
  
  list(data = df, y_label_data = df_2, label = label)
})

#----perf dot chart data prep----
ci_perf_dotchart_plot_data <- reactive({
  req(!is.null(input$select_district),
      input$ci_sel_product %in% (district_products %>%
                                   filter(districtname %in% input$select_district) %>%
                                   .$product))
  
  df <- ci_perf_dotchart_rdf %>%
    filter(districtname %in% input$select_district,
           product %in% input$ci_sel_product) %>%
    mutate(score_levels = case_when(between(avg_score,0,35.49) ~ '0-35',
                                    between(avg_score,35.5,70.49) ~ '36-70',
                                    between(avg_score,70.5,100) ~ '71-100') %>%
             factor(levels = c('0-35','36-70','71-100'))) %>%
    select(-districtname, -product, -title, -avg_score)
  
  df_2 <- ci_topic_content_rdf %>%
    filter(product %in% input$ci_sel_product) %>%
    select(-product, -title)
  
  label <- labs(
    title = paste0('District: ', input$select_district)
  )
  
  list(data = df, y_label_data = df_2, label = label)
})


#----uot table data prep----
ci_uot_table_data <- reactive({
  req(!is.null(input$select_district),
      input$ci_sel_product %in% (district_products %>%
                                   filter(districtname %in% input$select_district) %>%
                                   .$product))
  
  df_1 <- ci_nvr_rdf %>%
    filter(districtname %in% input$select_district,
           product %in% input$ci_sel_product) %>%
    select(-districtname, -product)
  
  df_2 <- ci_asd_rdf %>%
    filter(districtname %in% input$select_district,
           product %in% input$ci_sel_product) %>%
    select(userroletype, date_binned_by_week, avg_duration)
  
  df_3 <- ci_aeu_rdf %>%
    filter(districtname %in% input$select_district,
           product %in% input$ci_sel_product) %>%
    select(userroletype, date_binned_by_week, n_events_per_user)
  
  start_date <- min(df_1$date_binned_by_week)
  end_date <- max(df_1$date_binned_by_week)
  
  full_table <- expand_grid(date_binned_by_week = seq.Date(from = start_date, to = end_date, by = 'week'),
                            userroletype = c('Student','Teacher'))
  
  df_1 %>%
    inner_join(y = df_2, by = c('date_binned_by_week','userroletype')) %>%
    inner_join(y = df_3, by = c('date_binned_by_week','userroletype')) %>%
    right_join(y = full_table, by = c('date_binned_by_week','userroletype')) %>%
    replace_na(list(tot_users = 0, new = 0, return = 0, leaving = 0, bouncing = 0, avg_duration = 0, n_events_per_user = 0)) %>%
    pivot_wider(names_from = userroletype,
                values_from = c(tot_users, new, return, leaving, bouncing, avg_duration, n_events_per_user),
                names_sep = '.',
                values_fill = list(tot_users = 0, new = 0, return = 0, leaving = 0, bouncing = 0, avg_duration = 0, n_events_per_user = 0)) %>%
    mutate(districtname = input$select_district,
           product = input$ci_sel_product) %>%
    select(districtname, product, date_binned_by_week, ends_with('.Student'), ends_with('.Teacher')) %>%
    arrange(date_binned_by_week) %>%
    mutate(date_binned_by_week = paste0(day(date_binned_by_week),' ',month.abb[month(date_binned_by_week)],', ',year(date_binned_by_week)))
  
})

#----cp table data prep----
ci_cp_table_data <- reactive({
  req(!is.null(input$select_district),
      input$ci_sel_product %in% (district_products %>%
                                   filter(districtname %in% input$select_district) %>%
                                   .$product),
      !is.null(input$ci_cp_plots))
  
  if(input$ci_cp_plots == 'ci_usage_heatmap'){
    
    df <- ci_usage_heatmap_rdf %>%
      filter(districtname %in% input$select_district,
             product %in% input$ci_sel_product,
             userroletype %in% input$ci_user_type) %>%
      select(-districtname, -product,-userroletype) %>%
      rename('Curriculum' = title, 'Date(binned by week)' = date_binned_by_week , '# Users' = n_users,
             '# Events' = n_events, '# Events per User' = n_events_per_user)
  }
  
  else if(input$ci_cp_plots == 'ci_perf_heatmap'){
    
    df <- ci_perf_heatmap_rdf %>%
      filter(districtname %in% input$select_district,
             product %in% input$ci_sel_product) %>%
      select(-districtname, -product) %>%
      rename('Curriculum' = title, 'Date(binned by week)' = date_binned_by_week,
             '# Students' = n_users, 'Avg Percent Score' = avg_score)
  }
  
  else if(input$ci_cp_plots == 'ci_perf_dotchart'){
    
    df <- ci_perf_dotchart_rdf %>%
      filter(districtname %in% input$select_district,
             product %in% input$ci_sel_product) %>%
      select(-districtname, -product, -title) %>%
      rename('Curriculum' = content, 'Date(binned by week)' = date_binned_by_week,
             '# Students' = n_users, 'Avg Percent Score' = avg_score)
  }
  
  df_2 <- df %>%
    mutate(`Date(binned by week)` = paste0(day(`Date(binned by week)`),' ',month.abb[month(`Date(binned by week)`)],', ',year(`Date(binned by week)`)))
  
  df_2
})

  


#-----OUTPUT ELEMENTS-----
#----nvr plot-----
output$ci_nvr_plot <- renderPlot({
  req(input$ci_plot_type == 'Usage Over Time',
      input$ci_uot_plots == 'ci_nvr')
  
  district_product_new_vs_returning_users_plot(ci_nvr_plot_data()$data) +
    ci_nvr_plot_data()$label
  
},height = function(){
  ci_nvr_plot_data()$height
})

#---asd plot-----
output$ci_asd_plot <- renderPlot({
  req(input$ci_plot_type == 'Usage Over Time',
      input$ci_uot_plots == 'ci_asd')
  
  district_product_avg_session_duration_over_time_plot(ci_asd_plot_data()$data) +
    ci_asd_plot_data()$label
  
},height = function(){
  ci_asd_plot_data()$height
})

#---aeu plot-----
output$ci_aeu_plot <- renderPlot({
  req(input$ci_plot_type == 'Usage Over Time',
      input$ci_uot_plots == 'ci_aeu')
  
  district_product_avg_events_per_user_over_time_plot(ci_aeu_plot_data()$data) +
    ci_aeu_plot_data()$label
  
},height = function(){
  ci_aeu_plot_data()$height
})


#----usage heatmap plot-----
output$ci_usage_heatmap_plot <- renderPlot({
  req(input$ci_plot_type == 'Curriculum Pacing',
      input$ci_cp_plots == 'ci_usage_heatmap',
      input$ci_sel_product %in% (district_products %>%
                                   filter(districtname %in% input$select_district) %>%
                                   .$product),
      !is.null(input$ci_fill_by),
      !is.null(input$select_year),
      !is.null(input$ci_user_type))
  
  if(input$ci_user_type == 'Teacher'){
    validate(
      need(nrow(ci_usage_heatmap_plot_data()$data %>%
                  filter(userroletype == 'Teacher')) != 0, 'No Teacher Usage')
    )
  }
  
  else if(input$ci_user_type == 'Student'){
    validate(
      need(nrow(ci_usage_heatmap_plot_data()$data %>%
                  filter(userroletype == 'Student')) != 0, 'No Student Usage')
    )
  }

  usage_heatmap_pacing_plot(ci_usage_heatmap_plot_data()$data,
                            ci_usage_heatmap_plot_data()$y_label_data,
                            input$ci_sel_product,
                            input$ci_user_type,
                            input$ci_fill_by,
                            input$select_year) +
    ci_usage_heatmap_plot_data()$label
  
  
  
})

#------perf heatmap plot-----
output$ci_perf_heatmap_plot <- renderPlot({
  req(input$ci_plot_type == 'Curriculum Pacing',
      input$ci_cp_plots == 'ci_perf_heatmap',
      input$ci_sel_product %in% (district_products %>%
                                   filter(districtname %in% input$select_district) %>%
                                   .$product),
      !is.null(input$select_year))
  
  validate(
    need(nrow(ci_perf_heatmap_plot_data()$data) != 0, 'No Usage')
  )
  
  performance_heatmap_pacing_plot(ci_perf_heatmap_plot_data()$data,
                                  ci_perf_heatmap_plot_data()$y_label_data,
                                  input$ci_sel_product,
                                  input$select_year) +
    ci_perf_heatmap_plot_data()$label
  
  
  
})

#-----perf dot chart------
output$ci_perf_dotchart_plot <- renderPlot({
  req(input$ci_plot_type == 'Curriculum Pacing',
      input$ci_cp_plots == 'ci_perf_dotchart',
      input$ci_sel_product %in% (district_products %>%
                                   filter(districtname %in% input$select_district) %>%
                                   .$product),
      !is.null(input$select_year))
  
  validate(
    need(nrow(ci_perf_dotchart_plot_data()$data) != 0, 'No Usage')
  )
  
  performance_dotchart_pacing_plot(ci_perf_dotchart_plot_data()$data,
                                   ci_perf_dotchart_plot_data()$y_label_data,
                                   input$ci_sel_product,
                                   input$select_year) +
    ci_perf_dotchart_plot_data()$label
  
})

#-----uot table-------
output$ci_uot_table <- DT::renderDataTable({
  req(input$ci_plot_type == 'Usage Over Time')
  
  tags_html <- tags$table(class = "display", 
                          tags$thead(do.call(tags$tr, c(list(tags$th(rowspan = 2, "District"),
                                                             tags$th(rowspan = 2, "Product"),
                                                             tags$th(rowspan = 2, 'Date(binned by week)'),
                                                             tags$th(colspan = 7, "Student"),
                                                             tags$th(colspan = 7, "Teacher")))),
                                     tags$tr(lapply(rep(c("# Total Users","# New Users", "# Returning Users", "# Leaving Users", "# Bouncing Users", "Avg Session Duration(mins)", "Avg Events per User"), 2), tags$th))))

  
  sketch <- htmltools::withTags(tags_html)
  
  n_cols <- ncol(ci_uot_table_data())
  
  columns <- colnames(ci_uot_table_data())[(n_cols-13):n_cols]
  break_col <- break_col_func(ci_uot_table_data(), columns)
  
  brks <- break_col$brks
  clrs <- break_col$clrs
  
  init_tbl <- DT::datatable(ci_uot_table_data(), container = sketch, filter = 'top',
                            rownames = FALSE, class = 'cell-border stripe',
                            options(list(autoWidth = TRUE, sDom  = '<"top">lrt<"bottom">ip',
                                         FixedHeader = TRUE)))
  
  purrr::reduce(columns, function(x, y) {
    DT::formatStyle(x, y, backgroundColor = DT::styleInterval(brks[[y]], clrs[[y]]))
    
  }, .init = init_tbl)
  
    
})

#----cp table-----
output$ci_cp_table <- DT::renderDataTable({
  req(input$ci_plot_type == 'Curriculum Pacing',
      !is.null(input$ci_cp_plots))
  
  validate(
    need(nrow(ci_cp_table_data()) != 0, 'No Usage')
  )
  
  sub_cols <- c('ci_usage_heatmap' = 2, 'ci_perf_heatmap' = 1, 'ci_perf_dotchart' = 1)
  
  ncols <- ncol(ci_cp_table_data())
  columns <- colnames(ci_cp_table_data())[(ncols-sub_cols[input$ci_cp_plots]):ncols]
  break_col <- break_col_func(ci_cp_table_data(), columns)
  
  brks <- break_col$brks
  clrs <- break_col$clrs
  
  init_tbl <- DT::datatable(ci_cp_table_data(), filter = 'top', options = list(
    pageLength = 25, autoWidth = TRUE, sDom  = '<"top">lrt<"bottom">ip'
  ), class = 'cell-border stripe')
  
  purrr::reduce(columns, function(x, y) {
    DT::formatStyle(x, y, backgroundColor = DT::styleInterval(brks[[y]], clrs[[y]]))
    
  }, .init = init_tbl)
  
})  
