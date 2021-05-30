min_date_day <- uot_overall_rdf %>%
  filter(res %in% 'Day') %>%
  .$dateid %>%
  min()

min_date_week <- uot_overall_rdf %>%
  filter(res %in% 'Week') %>%
  .$dateid %>%
  min()

min_date_month <- uot_overall_rdf %>%
  filter(res %in% 'Month') %>%
  .$dateid %>%
  min()

max_date_day <- uot_overall_rdf %>%
  filter(res %in% 'Day') %>%
  .$dateid %>%
  max()

max_date_week <- uot_overall_rdf %>%
  filter(res %in% 'Week') %>%
  .$dateid %>%
  max()

max_date_month <- uot_overall_rdf %>%
  filter(res %in% 'Month') %>%
  .$dateid %>%
  max()

day(max_date_month) <- days_in_month(month(max_date_month))




#----UI ELEMENTS-------
#-----date range--------
output$uot_date_range_ui <- renderUI({
  req(!is.null(input$uot_res))
  
  if(input$uot_res == 'Day'){
    MydateRangeInput(inputId = 'uot_date_range_day', label = 'Select Duration:',
                     start = min_date_day, end = max_date_day, min = min_date_day, max = max_date_day, minviewmode="days",
                     format = 'dd-mm-yyyy')
  }
  
  else if(input$uot_res == 'Week'){
    MydateRangeInput(inputId = 'uot_date_range_week', label = 'Select Duration:',
                     start = min_date_week, end = max_date_week, min = min_date_week, max = max_date_week, minviewmode="days",
                     format = 'dd-mm-yyyy')
  }
  
  else if(input$uot_res == 'Month'){
    MydateRangeInput(inputId = 'uot_date_range_month', label = 'Select Duration:',
                     start = min_date_month, end = max_date_month, min = min_date_month, max = max_date_month, minviewmode="months",
                     format = 'M-yyyy')
  }
})
#-----usage type options------
output$uot_usage_type_options_ui <- renderUI({
  req(!is.null(input$uot_usage_type))
  
  if(input$uot_usage_type == 'By Product'){
    x <- product_group_products_rdf %>%
      filter(product_group %in% input$ou_select_pg) %>%
      .$product
    
    selectInput(inputId = 'uot_usage_type_product_options', label = 'Product:', choices = x)
  }
})

#------add feature--------
output$uot_add_feature_ui <- renderUI({
  req(!is.null(input$uot_plots))
  
  if(input$uot_plots == 'uot_nvr'){
    checkboxGroupInput(inputId = 'uot_add_feature', label = 'Add Feature to Plot:',
                       choices = c('Leaving User', 'Bouncing User'))
  }
})

#----------REACTIVE COMPONENTS------
#-----date range-----
date_range <- reactive({
  req(!is.null(input$uot_res))
  
  if(input$uot_res == 'Day'){
    req(!is.null(input$uot_date_range_day))
    date_range <- input$uot_date_range_day
  }
  
  else if(input$uot_res == 'Week'){
    req(!is.null(input$uot_date_range_week))
    date_range <- input$uot_date_range_week
  }
  
  else if(input$uot_res == 'Month'){
    
    req(!is.null(input$uot_date_range_month))
    date_range <- input$uot_date_range_month
    day(date_range) <- 1
  }
  date_range
})


#-----uot plot data-----

uot_plot_data <- reactive({
  req(!is.null(input$uot_res), !is.null(date_range()), !is.null(input$uot_usage_type))
  
  validate(
    need(date_range()[1] <= date_range()[2], 'Invalid Duration')
  )
  
  if(input$uot_usage_type == 'Overall'){
    df <- uot_overall_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range()[1], date_range()[2]),
             product_group %in% input$ou_select_pg) %>%
      select(-product_group, -res, -tot_users)
    
    label <- labs(title = paste0('Product Group: ',input$ou_select_pg),
                  subtitle = input$ou_select_year)
  }
  
  else if(input$uot_usage_type == 'By Product'){
    req(!is.null(input$uot_usage_type_product_options))
    
    df <- uot_by_product_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range()[1], date_range()[2]),
             product %in% input$uot_usage_type_product_options) %>%
      select(-product, -res, -tot_users)
    
    label <- labs(title = paste0('Product: ', input$uot_usage_type_product_options),
                  subtitle = input$ou_select_year)
  }
  
  validate(
    need(nrow(df) != 0, 'No Usage for Selected Filters')
  )
  
  if(!is.null(input$uot_add_feature)){
    if(length(input$uot_add_feature) == 1){
      if(input$uot_add_feature == 'Leaving User'){
        df_2 <- df %>%
          mutate(return = return-leaving) %>%
          select(-bouncing) %>%
          rename('New User' = new, 'Returning User' = return, 'Leaving User' = leaving)
      }
      
      else if(input$uot_add_feature == 'Bouncing User'){
        df_2 <- df %>%
          mutate(new = new-bouncing) %>%
          select(-leaving) %>%
          rename('New User' = new, 'Returning User' = return, 'Bouncing User' = bouncing)
      }
    }
    
    else if(length(input$uot_add_feature) == 2){
      df_2 <- df %>%
        mutate(new = new-bouncing, return = return-leaving) %>%
        rename('New User' = new, 'Returning User' = return, 'Leaving User' = leaving, 'Bouncing User' = bouncing)
    }
  }
  
  else if(is.null(input$uot_add_feature)){
    df_2 <- df %>%
      select(-leaving, -bouncing) %>%
      rename('New User' = new, 'Returning User' = return)
  }
  
  df_3 <- df_2 %>%
    pivot_longer(cols = c(-userroletype,-dateid), names_to = 'feature', values_to = 'value')

  list(data = df_3, label = label)
  
})

#----uot asd plot data------
uot_asd_plot_data <- reactive({
  req(!is.null(input$uot_res), !is.null(date_range()), !is.null(input$uot_usage_type))
  
  validate(
    need(date_range()[1] <= date_range()[2], 'Invalid Duration')
  )
  
  if(input$uot_usage_type == 'Overall'){
    df <- uot_asd_overall_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range()[1], date_range()[2]),
             product_group %in% input$ou_select_pg) %>%
      select(-product_group, -res, -tot_users, -n_users)
    
    label <- labs(title = paste0('Product Group: ',input$ou_select_pg),
                  subtitle = input$ou_select_year)
  }
  
  else if(input$uot_usage_type == 'By Product'){
    req(!is.null(input$uot_usage_type_product_options))
    
    df <- uot_asd_by_product_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range()[1], date_range()[2]),
             product %in% input$uot_usage_type_product_options) %>%
      select(-product, -res, -tot_users, -n_users)
    
    label <- labs(title = paste0('Product: ', input$uot_usage_type_product_options),
                  subtitle = input$ou_select_year)
  }
  
  validate(
    need(nrow(df) != 0, 'No Usage for Selected Filters')
  )
  
  list(data = df, label = label)
  
})

#----uot aeu plot data-----
uot_aeu_plot_data <- reactive({
  req(!is.null(input$uot_res), !is.null(date_range()), !is.null(input$uot_usage_type))
  
  validate(
    need(date_range()[1] <= date_range()[2], 'Invalid Duration')
  )
  
  if(input$uot_usage_type == 'Overall'){
    df <- uot_aeu_overall_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range()[1], date_range()[2]),
             product_group %in% input$ou_select_pg) %>%
      select(-product_group, -res, -tot_users, -n_users)
    
    label <- labs(title = paste0('Product Group: ',input$ou_select_pg),
                  subtitle = input$ou_select_year)
  }
  
  else if(input$uot_usage_type == 'By Product'){
    req(!is.null(input$uot_usage_type_product_options))
    
    df <- uot_aeu_by_product_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range()[1], date_range()[2]),
             product %in% input$uot_usage_type_product_options) %>%
      select(-product, -res, -tot_users, -n_users)
    
    label <- labs(title = paste0('Product: ', input$uot_usage_type_product_options),
                  subtitle = input$ou_select_year)
  }
  
  validate(
    need(nrow(df) != 0, 'No Usage for Selected Filters')
  )
  
  list(data = df, label = label)
  
})

#-----uot table data------
uot_table_data <- reactive({
  req(!is.null(input$uot_res), !is.null(date_range()), !is.null(input$uot_usage_type))
  
  validate(
    need(date_range()[1] <= date_range()[2], 'Invalid Duration')
  )
  
  if(input$uot_usage_type == 'Overall'){
    
    df_1 <- uot_overall_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range()[1], date_range()[2]),
             product_group %in% input$ou_select_pg) %>%
      mutate(new = new-bouncing, return = return-leaving) %>%
      select(-res)
    
    validate(
      need(nrow(df_1) != 0, 'No Usage for Selected Filters')
    )
    
    df_2 <- uot_asd_overall_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range()[1], date_range()[2]),
             product_group %in% input$ou_select_pg) %>%
      select(-res, -n_users, -tot_users, -perc_users)
    
    df_3 <- uot_aeu_overall_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range()[1], date_range()[2]),
             product_group %in% input$ou_select_pg) %>%
      select(-res, -tot_users, -n_users, -perc_users)
    
    df_1 %>%
      inner_join(y = df_2, by = c('product_group','dateid','userroletype')) %>%
      inner_join(y = df_3, by = c('product_group','dateid','userroletype')) %>%
      pivot_wider(names_from = userroletype,
                  values_from = c(tot_users, new, return, leaving, bouncing, avg_session_duration, avg_events_per_user),
                  names_sep = '.',
                  values_fill = list(tot_users = 0, new = 0, return = 0, leaving = 0, bouncing = 0, avg_session_duration = 0, avg_events_per_user = 0)) %>%
      select(product_group, dateid, ends_with('.Student'), ends_with('.Teacher')) %>%
      arrange(dateid) %>%
      mutate(dateid = if(input$uot_res == 'Month') paste0(month.abb[month(dateid)],'-',year(dateid)) else paste0(day(dateid),' ',month.abb[month(dateid)],', ',year(dateid)))
  }
  
  else if(input$uot_usage_type == 'By Product'){
    req(!is.null(input$uot_usage_type_product_options))
    
    df_1 <- uot_by_product_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range()[1], date_range()[2]),
             product %in% input$uot_usage_type_product_options) %>%
      mutate(new = new-bouncing, return = return-leaving) %>%
      select(-res)
    
    validate(
      need(nrow(df_1) != 0, 'No Usage for Selected Filters')
    )
    
    df_2 <- uot_asd_by_product_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range()[1], date_range()[2]),
             product %in% input$uot_usage_type_product_options) %>%
      select(-res, -n_users, -tot_users, -perc_users)
    
    df_3 <- uot_aeu_by_product_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range()[1], date_range()[2]),
             product %in% input$uot_usage_type_product_options) %>%
      select(-res, -tot_users, -n_users, -perc_users)
    
    df_1 %>%
      inner_join(y = df_2, by = c('product','dateid','userroletype')) %>%
      inner_join(y = df_3, by = c('product','dateid','userroletype')) %>%
      pivot_wider(names_from = userroletype,
                  values_from = c(tot_users, new, return, leaving, bouncing, avg_session_duration, avg_events_per_user),
                  names_sep = '.',
                  values_fill = list(tot_users = 0, new = 0, return = 0, leaving = 0, bouncing = 0, avg_session_duration = 0, avg_events_per_user = 0)) %>%
      select(product, dateid, ends_with('.Student'), ends_with('.Teacher')) %>%
      arrange(dateid) %>%
      mutate(dateid = if(input$uot_res == 'Month') paste0(month.abb[month(dateid)],'-',year(dateid)) else paste0(day(dateid),' ',month.abb[month(dateid)],', ',year(dateid)))
  }
  
})

#----OUTPUT ELEMENTS------
#---uot plot----

output$uot_plot <- renderPlot({
  req(!is.null(input$uot_res), !is.null(input$uot_fix_y))
  
  new_vs_returning_users_plot(uot_plot_data()$data, input$uot_res, input$uot_fix_y) +
    uot_plot_data()$label
})

#----uot asd plot-----
output$uot_asd_plot <- renderPlot({
  req(!is.null(input$uot_res), !is.null(input$uot_fix_y))
  
  avg_session_duration_over_time_plot(uot_asd_plot_data()$data, input$uot_res, input$uot_fix_y) +
    uot_asd_plot_data()$label
})

#----uot aeu plot-----
output$uot_aeu_plot <- renderPlot({
  req(!is.null(input$uot_res), !is.null(input$uot_fix_y))
  
  avg_events_per_user_over_time_plot(uot_aeu_plot_data()$data, input$uot_res, input$uot_fix_y) +
    uot_aeu_plot_data()$label
})

#----uot table-----
output$uot_table <- DT::renderDataTable({
  req(!is.null(input$uot_res), !is.null(input$uot_usage_type))
  
  dateid_header <- c(Day = 'Date', Week = 'Date(binned by week)', Month = 'Month')
  usage_type_header <- c(Overall = 'Product Group', 'By Product' = 'Product')
  
  tags_html <- tags$table(class = "display",
                          tags$thead(do.call(tags$tr, c(list(tags$th(rowspan = 2, usage_type_header[input$uot_usage_type]),
                                                             tags$th(rowspan = 2, dateid_header[input$uot_res]),
                                                             tags$th(colspan = 7, "Student"),
                                                             tags$th(colspan = 7, "Teacher")))),
                                     tags$tr(lapply(rep(c("# Total Users","# New Users", "# Returning Users", "# Leaving Users", "# Bouncing Users", "Avg Session Duration(mins)", "Avg Events per User"), 2), tags$th))))
  
  sketch <- htmltools::withTags(tags_html)
  n_cols <- ncol(uot_table_data())
  
  columns <- colnames(uot_table_data())[(n_cols-13):n_cols]
  break_col <- break_col_func(uot_table_data(), columns)
  
  brks <- break_col$brks
  clrs <- break_col$clrs
  
  init_tbl <- DT::datatable(uot_table_data(), container = sketch, filter = 'top', rownames = FALSE, class = 'cell-border stripe',
                            # options(list(autoWidth = TRUE, dom = 'tip')))
                            options(list(autoWidth = TRUE, sDom  = '<"top">lrt<"bottom">ip')))
  
  purrr::reduce(columns, function(x, y) {
    DT::formatStyle(x, y, backgroundColor = DT::styleInterval(brks[[y]], clrs[[y]]))
    
  }, .init = init_tbl)
})
