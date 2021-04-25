output$uot_date_range_ui <- renderUI({
  req(!is.null(input$uot_res))
  
  if(input$uot_res == 'Day'){
    MydateRangeInput(inputId = 'uot_date_range_day', label = 'Select Duration:',
                     start = '2018-08-01', end = '2019-07-31', min = '2018-08-01', max = '2019-07-31', minviewmode="days",
                     format = 'dd-mm-yyyy')
  }
  
  else if(input$uot_res == 'Week'){
    MydateRangeInput(inputId = 'uot_date_range_week', label = 'Select Duration:',
                     start = '2018-08-01', end = '2019-07-31', min = '2018-08-01', max = '2019-07-31', minviewmode="days",
                     format = 'dd-mm-yyyy')
  }
  
  else if(input$uot_res == 'Month'){
    MydateRangeInput(inputId = 'uot_date_range_month', label = 'Select Duration:',
                     start = '2018-08-01', end = '2019-07-31', min = '2018-08-01', max = '2019-07-31', minviewmode="months",
                     format = 'M-yyyy')
  }
})



output$uot_usage_type_options_ui <- renderUI({
  req(input$uot_usage_type %in% c('Grade Band', 'Subject', 'Most Used Product Groups'), input$uot_facet == 'False')
  
  if(input$uot_usage_type == 'Subject'){
    selectInput(inputId = 'uot_usage_type_subject_options', label = 'Subject:',
                choices = subjects)
  }
  
  else if(input$uot_usage_type == 'Grade Band'){
    selectInput(inputId = 'uot_usage_type_gb_options', label = 'Grade Band:',
                choices = grade_bands)
  }
  
  else if(input$uot_usage_type == 'Most Used Product Groups'){
    selectInput(inputId = 'uot_usage_type_pg_options', label = 'Product Group:',
                choices = top_4_product_groups)
  }
})

output$uot_add_feature_ui <- renderUI({
  req(!is.null(input$uot_plots))
  
  if(input$uot_plots == 'uot_nvr'){
    checkboxGroupInput(inputId = 'uot_add_feature', label = 'Add Feature to Plot:',
                       choices = c('Leaving User', 'Bouncing User'))
  }
})



output$uot_facet_ui <- renderUI({
  req(input$uot_usage_type %in% c('Grade Band', 'Subject', 'Most Used Product Groups'))
  
  if(input$uot_usage_type %in% c('Grade Band', 'Subject', 'Most Used Product Groups')){
    radioButtons(inputId = 'uot_facet', label = 'Facet Plot:', choices = c('True','False'), selected = 'False')
  }
})

output$uot_userrole_ui <- renderUI({
  req(input$uot_facet == 'True',
      input$uot_usage_type %in% c('Grade Band', 'Subject', 'Most Used Product Groups'))
  
  if(input$uot_facet == 'True' && input$uot_usage_type %in% c('Grade Band', 'Subject', 'Most Used Product Groups')){
    radioButtons(inputId = 'uot_userrole', label = 'User Type:', choices = c('Student','Teacher'), selected = 'Student')
  }
})

output$uot_fix_y_ui <- renderUI({
  req(input$uot_facet == 'True',
      input$uot_usage_type %in% c('Grade Band', 'Subject', 'Most Used Product Groups'))
  
  if(input$uot_facet == 'True' && input$uot_usage_type %in% c('Grade Band', 'Subject', 'Most Used Product Groups')){
    radioButtons(inputId = 'uot_fix_y', label = 'Fix Y-axis:', choices = c('True','False'), selected = 'False')
  }
})

uot_plot_data <- reactive({
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
  
  validate(
    need(date_range[1] <= date_range[2], 'Invalid Duration')
  )
  
  req(!is.null(input$uot_usage_type))
  
  
  if(input$uot_usage_type == 'Overall'){
    df <- uot_overall_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
      select(-res) %>%
      mutate(facet_col = NA)
    
    label <- labs(title = 'Usage Over Time',
                  subtitle = input$ou_select_year)
    
    table_df <- uot_overall_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
      select(-res)
  }
  
  else if(input$uot_usage_type == 'Subject'){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet=='False'){
      req(!is.null(input$uot_usage_type_subject_options))
      
      df <- uot_subject_rdf %>%
        filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2]), subject %in% input$uot_usage_type_subject_options) %>%
        select(-subject,-res) %>%
        mutate(facet_col = NA)
      
      label <- labs(title = 'Usage Over Time',
                    subtitle = paste0('Subject: ',input$uot_usage_type_subject_options,'\n',input$ou_select_year))
    }
    
    else if(input$uot_facet=='True'){
      req(!is.null(input$uot_userrole))
      
      df <- uot_subject_rdf %>%
        filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
        select(-res) %>%
        rename(facet_col = subject)
      
      label <- labs(title = 'Usage Over Time',
                    subtitle = paste0('User Type: ',input$uot_userrole,'\n',input$ou_select_year))
    }
    
    table_df <- uot_subject_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
      select(-res) %>%
      rename(usage_type_col = subject)
  }
  
  else if(input$uot_usage_type == 'Grade Band'){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet=='False'){
      req(!is.null(input$uot_usage_type_gb_options))
      
      df <- uot_grade_band_rdf %>%
        filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2]), grade_band %in% input$uot_usage_type_gb_options) %>%
        select(-grade_band,-res) %>%
        mutate(facet_col = NA)
      
      label <- labs(title = 'Usage Over Time',
                    subtitle = paste0('Grade Band: ',input$uot_usage_type_gb_options,'\n',input$ou_select_year))
    }
    
    else if(input$uot_facet=='True'){
      req(!is.null(input$uot_userrole))
      
      df <- uot_grade_band_rdf %>%
        filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
        select(-res) %>%
        rename(facet_col = grade_band)
      
      label <- labs(title = 'Usage Over Time',
                    subtitle = paste0('User Type: ',input$uot_userrole,'\n',input$ou_select_year))
    }
    table_df <- uot_grade_band_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
      select(-res) %>%
      rename(usage_type_col = grade_band)
  }
  
  else if(input$uot_usage_type == 'Most Used Product Groups'){
    
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet=='False'){
      req(!is.null(input$uot_usage_type_pg_options))
      
      df <- uot_top_4_product_group_rdf %>%
        filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2]), product_group %in% input$uot_usage_type_pg_options) %>%
        select(-product_group,-res) %>%
        mutate(facet_col = NA)
      
      label <- labs(title = 'Usage Over Time',
                    subtitle = paste0('Product Group: ',input$uot_usage_type_pg_options,'\n',input$ou_select_year))
    }
    
    else if(input$uot_facet=='True'){
      req(!is.null(input$uot_userrole))
      
      df <- uot_top_4_product_group_rdf %>%
        filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
        select(-res) %>%
        rename(facet_col = product_group)
      
      label <- labs(title = 'Usage Over Time',
                    subtitle = paste0('User Type: ',input$uot_userrole,'\n',input$ou_select_year))
    }
    table_df <- uot_top_4_product_group_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
      select(-res) %>%
      rename(usage_type_col = product_group)
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
    pivot_longer(cols = c(-facet_col,-userroletype,-dateid),
                 names_to = 'feature', values_to = 'value')
  
  
  df_4 <- table_df %>%
    mutate(new = new-bouncing, return = return-leaving, total = new+return+leaving+bouncing)
  
  list(data = df_3, title = label, data_2 = df_4)
  
})

uot_asd_plot_data <- reactive({
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
  
  validate(
    need(date_range[1] <= date_range[2], 'Invalid Duration')
  )
  
  req(!is.null(input$uot_usage_type))
  
  
  if(input$uot_usage_type == 'Overall'){
    df <- uot_asd_overall_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
      select(-res)
    
    label <- labs(title = 'Average Session Duration Over Time',
                  subtitle = input$ou_select_year)
    
    table_df <- uot_asd_overall_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
      select(-res)
  }
  
  else if(input$uot_usage_type == 'Subject'){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet=='False'){
      req(!is.null(input$uot_usage_type_subject_options))
      
      df <- uot_asd_subject_rdf %>%
        filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2]), subject %in% input$uot_usage_type_subject_options) %>%
        select(-subject,-res)
      
      label <- labs(title = 'Average Session Duration Over Time',
                    subtitle = paste0('Subject: ',input$uot_usage_type_subject_options,'\n',input$ou_select_year))
    }
    
    else if(input$uot_facet=='True'){
      req(!is.null(input$uot_userrole))
      
      df <- uot_asd_subject_rdf %>%
        filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
        select(-res) %>%
        rename(facet_col = subject)
      
      label <- labs(title = 'Average Session Duration Over Time',
                    subtitle = paste0('User Type: ',input$uot_userrole,'\n',input$ou_select_year))
    }
    
    table_df <- uot_asd_subject_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
      select(-res) %>%
      rename(usage_type_col = subject)
  }
  
  else if(input$uot_usage_type == 'Grade Band'){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet=='False'){
      req(!is.null(input$uot_usage_type_gb_options))
      
      df <- uot_asd_grade_band_rdf %>%
        filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2]), grade_band %in% input$uot_usage_type_gb_options) %>%
        select(-grade_band,-res)
      
      label <- labs(title = 'Average Session Duration Over Time',
                    subtitle = paste0('Grade Band: ',input$uot_usage_type_gb_options,'\n',input$ou_select_year))
    }
    
    else if(input$uot_facet=='True'){
      req(!is.null(input$uot_userrole))
      
      df <- uot_asd_grade_band_rdf %>%
        filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
        select(-res) %>%
        rename(facet_col = grade_band)
      
      label <- labs(title = 'Average Session Duration Over Time',
                    subtitle = paste0('User Type: ',input$uot_userrole,'\n',input$ou_select_year))
    }
    table_df <- uot_asd_grade_band_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
      select(-res) %>%
      rename(usage_type_col = grade_band)
  }
  
  else if(input$uot_usage_type == 'Most Used Product Groups'){
    
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet=='False'){
      req(!is.null(input$uot_usage_type_pg_options))
      
      df <- uot_asd_top_4_product_group_rdf %>%
        filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2]), product_group %in% input$uot_usage_type_pg_options) %>%
        select(-product_group,-res)
      
      label <- labs(title = 'Average Session Duration Over Time',
                    subtitle = paste0('Product Group: ',input$uot_usage_type_pg_options,'\n',input$ou_select_year))
    }
    
    else if(input$uot_facet=='True'){
      req(!is.null(input$uot_userrole))
      
      df <- uot_asd_top_4_product_group_rdf %>%
        filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
        select(-res) %>%
        rename(facet_col = product_group)
      
      label <- labs(title = 'Average Session Duration Over Time',
                    subtitle = paste0('User Type: ',input$uot_userrole,'\n',input$ou_select_year))
    }
    table_df <- uot_asd_top_4_product_group_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
      select(-res) %>%
      rename(usage_type_col = product_group)
  }
  
  validate(
    need(nrow(df) != 0, 'No Usage for Selected Filters')
  )
  
  list(data = df, title = label, data_2 = table_df)
  
})

uot_aeu_plot_data <- reactive({
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
  
  validate(
    need(date_range[1] <= date_range[2], 'Invalid Duration')
  )
  
  req(!is.null(input$uot_usage_type))
  
  
  if(input$uot_usage_type == 'Overall'){
    df <- uot_aeu_overall_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
      select(-res)
    
    label <- labs(title = 'Average Events Per User Over Time',
                  subtitle = input$ou_select_year)
    
    table_df <- uot_aeu_overall_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
      select(-res)
  }
  
  else if(input$uot_usage_type == 'Subject'){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet=='False'){
      req(!is.null(input$uot_usage_type_subject_options))
      
      df <- uot_aeu_subject_rdf %>%
        filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2]), subject %in% input$uot_usage_type_subject_options) %>%
        select(-subject,-res)
      
      label <- labs(title = 'Average Events Per User Over Time',
                    subtitle = paste0('Subject: ',input$uot_usage_type_subject_options,'\n',input$ou_select_year))
    }
    
    else if(input$uot_facet=='True'){
      req(!is.null(input$uot_userrole))
      
      df <- uot_aeu_subject_rdf %>%
        filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
        select(-res) %>%
        rename(facet_col = subject)
      
      label <- labs(title = 'Average Events Per User Over Time',
                    subtitle = paste0('User Type: ',input$uot_userrole,'\n',input$ou_select_year))
    }
    
    table_df <- uot_aeu_subject_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
      select(-res) %>%
      rename(usage_type_col = subject)
  }
  
  else if(input$uot_usage_type == 'Grade Band'){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet=='False'){
      req(!is.null(input$uot_usage_type_gb_options))
      
      df <- uot_aeu_grade_band_rdf %>%
        filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2]), grade_band %in% input$uot_usage_type_gb_options) %>%
        select(-grade_band,-res)
      
      label <- labs(title = 'Average Events Per User Over Time',
                    subtitle = paste0('Grade Band: ',input$uot_usage_type_gb_options,'\n',input$ou_select_year))
    }
    
    else if(input$uot_facet=='True'){
      req(!is.null(input$uot_userrole))
      
      df <- uot_aeu_grade_band_rdf %>%
        filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
        select(-res) %>%
        rename(facet_col = grade_band)
      
      label <- labs(title = 'Average Events Per User Over Time',
                    subtitle = paste0('User Type: ',input$uot_userrole,'\n',input$ou_select_year))
    }
    table_df <- uot_aeu_grade_band_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
      select(-res) %>%
      rename(usage_type_col = grade_band)
  }
  
  else if(input$uot_usage_type == 'Most Used Product Groups'){
    
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet=='False'){
      req(!is.null(input$uot_usage_type_pg_options))
      
      df <- uot_aeu_top_4_product_group_rdf %>%
        filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2]), product_group %in% input$uot_usage_type_pg_options) %>%
        select(-product_group,-res)
      
      label <- labs(title = 'Average Events Per User Over Time',
                    subtitle = paste0('Product Group: ',input$uot_usage_type_pg_options,'\n',input$ou_select_year))
    }
    
    else if(input$uot_facet=='True'){
      req(!is.null(input$uot_userrole))
      
      df <- uot_aeu_top_4_product_group_rdf %>%
        filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
        select(-res) %>%
        rename(facet_col = product_group)
      
      label <- labs(title = 'Average Events Per User Over Time',
                    subtitle = paste0('User Type: ',input$uot_userrole,'\n',input$ou_select_year))
    }
    table_df <- uot_aeu_top_4_product_group_rdf %>%
      filter(res %in% input$uot_res, between(dateid, date_range[1], date_range[2])) %>%
      select(-res) %>%
      rename(usage_type_col = product_group)
  }
  
  validate(
    need(nrow(df) != 0, 'No Usage for Selected Filters')
  )
  
  list(data = df, title = label, data_2 = table_df)
  
})


uot_table_data <- reactive({

  req(!is.null(input$uot_usage_type), !is.null(input$uot_res))
  
  if(input$uot_usage_type == 'Overall'){

    df <- uot_aeu_plot_data()$data_2 %>%
      select(-n_users, -perc_users) %>%
      inner_join(y = uot_asd_plot_data()$data_2 %>% select(-n_sessions,-perc_sessions),
                 by = c('dateid', 'userroletype')) %>%
      inner_join(y = uot_plot_data()$data_2,
                 by = c('dateid', 'userroletype')) %>%
      pivot_wider(names_from = userroletype,
                  values_from = c(total,new, return, leaving, bouncing, avg_duration, n_events_per_user),
                  names_sep = '.',
                  values_fill = list(total = 0,new = 0, return = 0, leaving = 0, bouncing = 0, avg_duration = 0, n_events_per_user = 0)) %>%
      select(dateid, ends_with('.Student'), ends_with('.Teacher')) %>%
      arrange(dateid) %>%
      mutate(dateid = if(input$uot_res == 'Month') paste0(month.abb[month(dateid)],'-',year(dateid)) else paste0(day(dateid),' ',month.abb[month(dateid)],', ',year(dateid)))
  }

  else if(input$uot_usage_type %in% c('Subject', 'Grade Band', 'Most Used Product Groups')){

    df <- uot_aeu_plot_data()$data_2 %>%
      select(-n_users, -perc_users) %>%
      inner_join(y = uot_asd_plot_data()$data_2 %>% select(-n_sessions,-perc_sessions),
                 by = c('usage_type_col','dateid','userroletype')) %>%
      inner_join(y = uot_plot_data()$data_2,
                 by = c('usage_type_col','dateid', 'userroletype')) %>%
      pivot_wider(names_from = userroletype,
                  values_from = c(total,new, return, leaving, bouncing, avg_duration, n_events_per_user),
                  names_sep = '.',
                  values_fill = list(total = 0,new = 0, return = 0, leaving = 0, bouncing = 0, avg_duration = 0, n_events_per_user = 0)) %>%
      select(usage_type_col, dateid, ends_with('.Student'), ends_with('.Teacher')) %>%
      arrange(usage_type_col,dateid) %>%
      mutate(dateid = if(input$uot_res == 'Month') paste0(month.abb[month(dateid)],'-',year(dateid)) else paste0(day(dateid),' ',month.abb[month(dateid)],', ',year(dateid)))
  }
  df
})


output$uot_plot <- renderPlot({
  req(!is.null(input$uot_res), !is.null(input$uot_usage_type))
  
  if(input$uot_usage_type == 'Overall'){
    
    usage_over_time_plot(uot_plot_data()$data, input$uot_res, 'False', NULL, 'False') +
      uot_plot_data()$title
  }
  
  else if(input$uot_usage_type %in% c('Subject', 'Grade Band', 'Most Used Product Groups')){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet == 'False'){
      usage_over_time_plot(uot_plot_data()$data, input$uot_res, input$uot_facet, NULL, 'False') +
        uot_plot_data()$title
    }
    
    else if(input$uot_facet == 'True'){
      req(!is.null(input$uot_userrole), !is.null(input$uot_fix_y))
      
      usage_over_time_plot(uot_plot_data()$data, input$uot_res, input$uot_facet, input$uot_userrole, input$uot_fix_y) +
        uot_plot_data()$title
    }
  }
  
})

output$uot_asd_plot <- renderPlot({
  req(!is.null(input$uot_res), !is.null(input$uot_usage_type))
  
  if(input$uot_usage_type == 'Overall'){
    
    avg_session_duration_over_time_plot(uot_asd_plot_data()$data, input$uot_res, 'False', NULL, 'False') +
      uot_asd_plot_data()$title
  }
  
  else if(input$uot_usage_type %in% c('Subject', 'Grade Band', 'Most Used Product Groups')){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet == 'False'){
      avg_session_duration_over_time_plot(uot_asd_plot_data()$data, input$uot_res, input$uot_facet, NULL, 'False') +
        uot_asd_plot_data()$title
    }
    
    else if(input$uot_facet == 'True'){
      req(!is.null(input$uot_userrole), !is.null(input$uot_fix_y))
      
      avg_session_duration_over_time_plot(uot_asd_plot_data()$data, input$uot_res, input$uot_facet, input$uot_userrole, input$uot_fix_y) +
        uot_asd_plot_data()$title
    }
  } 

  
})

output$uot_aeu_plot <- renderPlot({
  req(!is.null(input$uot_res), !is.null(input$uot_usage_type))
  
  if(input$uot_usage_type == 'Overall'){
    
    avg_events_per_user_over_time_plot(uot_aeu_plot_data()$data, input$uot_res, 'False', NULL, 'False') +
      uot_aeu_plot_data()$title
  }
  
  else if(input$uot_usage_type %in% c('Subject', 'Grade Band', 'Most Used Product Groups')){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet == 'False'){
      avg_events_per_user_over_time_plot(uot_aeu_plot_data()$data, input$uot_res, input$uot_facet, NULL, 'False') +
        uot_aeu_plot_data()$title
    }
    
    else if(input$uot_facet == 'True'){
      req(!is.null(input$uot_userrole), !is.null(input$uot_fix_y))
      
      avg_events_per_user_over_time_plot(uot_aeu_plot_data()$data, input$uot_res, input$uot_facet, input$uot_userrole, input$uot_fix_y) +
        uot_aeu_plot_data()$title
    }
  } 

})

output$uot_table <- DT::renderDataTable({
  req(!is.null(input$uot_res), !is.null(input$uot_usage_type))
  
  dateid_header <- c(Day = 'Date', Week = 'Date(binned by week)', Month = 'Month')
  
  if(input$uot_usage_type == "Overall"){
    tags_html <- tags$table(class = "display",
      tags$thead(do.call(tags$tr, c(list(tags$th(rowspan = 2, dateid_header[input$uot_res]),
                                         tags$th(colspan = 7, "Student"),
                                         tags$th(colspan = 7, "Teacher")))),
                 tags$tr(lapply(rep(c("# Total Users","# New Users", "# Returning Users", "# Leaving Users", "# Bouncing Users", "Avg Session Duration(mins)", "Avg Events per User"), 2), tags$th))))
  }
  else if(input$uot_usage_type == 'Grade Band'){
    tags_html <- tags$table(class = "display", 
                            tags$thead(do.call(tags$tr, c(list(tags$th(rowspan = 2, "Grade Band"),
                                                               tags$th(rowspan = 2, dateid_header[input$uot_res]),
                                                               tags$th(colspan = 7, "Student"),
                                                               tags$th(colspan = 7, "Teacher")))),
                                       tags$tr(lapply(rep(c("# Total Users","# New Users", "# Returning Users", "# Leaving Users", "# Bouncing Users", "Avg Session Duration(mins)", "Avg Events per User"), 2), tags$th))))
  }
  else if(input$uot_usage_type == 'Subject'){
    tags_html <- tags$table(class = "display", 
                            tags$thead(do.call(tags$tr, c(list(tags$th(rowspan = 2, "Discipline"),
                                                               tags$th(rowspan = 2, dateid_header[input$uot_res]),
                                                               tags$th(colspan = 7, "Student"),
                                                               tags$th(colspan = 7, "Teacher")))),
                                       tags$tr(lapply(rep(c("# Total Users","# New Users", "# Returning Users", "# Leaving Users", "# Bouncing Users", "Avg Session Duration(mins)", "Avg Events per User"), 2), tags$th))))
  }
  else if(input$uot_usage_type == 'Most Used Product Groups'){
    tags_html <- tags$table(class = "display", 
                            tags$thead(do.call(tags$tr, c(list(tags$th(rowspan = 2, "Product Group"),
                                                               tags$th(rowspan = 2, dateid_header[input$uot_res]),
                                                               tags$th(colspan = 7, "Student"),
                                                               tags$th(colspan = 7, "Teacher")))),
                                       tags$tr(lapply(rep(c("# Total Users","# New Users", "# Returning Users", "# Leaving Users", "# Bouncing Users", "Avg Session Duration(mins)", "Avg Events per User"), 2), tags$th))))
  }
  
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

