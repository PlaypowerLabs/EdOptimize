#-----min-max date range-----
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

#-----UI ELEMENTS-----
#-----date range ui-----

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

#----usage type options ui-----
output$uot_usage_type_options_ui <- renderUI({
  req(input$uot_usage_type %in% c('Grade Band', 'Subject', 'Most Used Product Groups'),
      input$uot_facet == 'False',
      !is.null(input$select_district))
  
  if(input$uot_usage_type == 'Subject'){
    
    x <- district_subjects %>%
      filter(districtname %in% input$select_district) %>%
      .$subject
    
    selectInput(inputId = 'uot_usage_type_subject_options', label = 'Subject',
                choices = x)
  }
  
  else if(input$uot_usage_type == 'Grade Band'){
    
    x <- district_grade_bands %>%
      filter(districtname %in% input$select_district) %>%
      .$grade_band
    
    selectInput(inputId = 'uot_usage_type_gb_options', label = 'Grade Band',
                choices = x)
  }
  
  else if(input$uot_usage_type == 'Most Used Product Groups'){
    
    x <- uot_districtwise_top_pg_rdf %>%
      filter(districtname %in% input$select_district) %>%
      .$product_group
    
    selectInput(inputId = 'uot_usage_type_pg_options', label = 'Product Group',
                choices = x)
  }
})

#----add feature ui----
output$uot_add_feature_ui <- renderUI({
  req(!is.null(input$uot_plots))
  
  if(input$uot_plots == 'uot_nvr'){
    checkboxGroupInput(inputId = 'uot_add_feature', label = 'Add Feature to Plot:',
                       choices = c('Leaving User', 'Bouncing User'))
  }
})

#-----facet ui-----
output$uot_facet_ui <- renderUI({
  req(input$uot_usage_type %in% c('Grade Band', 'Subject', 'Most Used Product Groups'))
  
  if(input$uot_usage_type %in% c('Grade Band', 'Subject', 'Most Used Product Groups')){
    radioButtons(inputId = 'uot_facet', label = 'Facet Plot:', choices = c('True','False'), selected = 'False')
  }
})

#----userrole ui----
output$uot_userrole_ui <- renderUI({
  req(input$uot_facet == 'True',
      input$uot_usage_type %in% c('Grade Band', 'Subject', 'Most Used Product Groups'))
  
  if(input$uot_facet == 'True' && input$uot_usage_type %in% c('Grade Band', 'Subject', 'Most Used Product Groups')){
    radioButtons(inputId = 'uot_userrole', label = 'User Type:', choices = c('Student','Teacher'), selected = 'Student')
  }
})

#------fix y axis ui-----
output$uot_fix_y_ui <- renderUI({
  req(input$uot_facet == 'True',
      input$uot_usage_type %in% c('Grade Band', 'Subject', 'Most Used Product Groups'))
  
  if(input$uot_facet == 'True' && input$uot_usage_type %in% c('Grade Band', 'Subject', 'Most Used Product Groups')){
    radioButtons(inputId = 'uot_fix_y', label = 'Fix Y-axis:', choices = c('True','False'), selected = 'False')
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

#----uot plot data prep-----

uot_plot_data <- reactive({
  req(!is.null(input$uot_res), !is.null(input$select_district), !is.null(input$uot_usage_type),
      !is.null(date_range()))
  
  validate(
    need(date_range()[1] <= date_range()[2], 'Invalid Duration')
  )
  
  if(input$uot_usage_type == 'Overall'){
    df <- uot_overall_rdf %>%
      filter(res %in% input$uot_res,
             between(dateid, date_range()[1], date_range()[2]),
             districtname %in% input$select_district) %>%
      select(-districtname, -res, -tot_users) %>%
      rename(facet_col = userroletype)
    
    label <- labs(title = 'Usage Over Time',
                  subtitle = paste0('District: ',input$select_district,'\n',
                                    input$select_year))
    
    height <- if_else(n_distinct(df$facet_col) == 1, 450, 700)
  }
  
  else if(input$uot_usage_type == 'Subject'){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet=='False'){
      req(!is.null(input$uot_usage_type_subject_options))
      
      df <- uot_subject_rdf %>%
        filter(res %in% input$uot_res,
               between(dateid, date_range()[1], date_range()[2]),
               subject %in% input$uot_usage_type_subject_options,
               districtname %in% input$select_district) %>%
        select(-districtname, -subject, -res, -tot_users) %>%
        rename(facet_col = userroletype)
      
      label <- labs(title = 'Usage Over Time',
                    subtitle = paste0('District: ',input$select_district,'\n',
                                      'Subject: ',input$uot_usage_type_subject_options,'\n',
                                      input$select_year))
      
      height <- if_else(n_distinct(df$facet_col) == 1, 450, 700)
    }
    
    else if(input$uot_facet=='True'){
      req(!is.null(input$uot_userrole))
      
      df <- uot_subject_rdf %>%
        filter(res %in% input$uot_res,
               between(dateid, date_range()[1], date_range()[2]),
               userroletype %in% input$uot_userrole,
               districtname %in% input$select_district) %>%
        select(-districtname, -userroletype, -res, -tot_users) %>%
        rename(facet_col = subject)
      
      label <- labs(title = 'Usage Over Time',
                    subtitle = paste0('District: ',input$select_district, '\n',
                                      'User Type: ',input$uot_userrole,'\n',
                                      input$select_year))
      
      height <- if_else(n_distinct(df$facet_col) <= 2, 450, 700)
    }
    
  }
  
  else if(input$uot_usage_type == 'Grade Band'){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet=='False'){
      req(!is.null(input$uot_usage_type_gb_options))
      
      df <- uot_grade_band_rdf %>%
        filter(res %in% input$uot_res,
               between(dateid, date_range()[1], date_range()[2]),
               grade_band %in% input$uot_usage_type_gb_options,
               districtname %in% input$select_district) %>%
        select(-districtname, -grade_band,-res, -tot_users) %>%
        rename(facet_col = userroletype)
      
      label <- labs(title = 'Usage Over Time',
                    subtitle = paste0('District: ', input$select_district, '\n',
                                      'Grade Band: ',input$uot_usage_type_gb_options,'\n',
                                      input$select_year))
      
      height <- if_else(n_distinct(df$facet_col) == 1, 450, 700)
    }
    
    else if(input$uot_facet=='True'){
      req(!is.null(input$uot_userrole))
      
      df <- uot_grade_band_rdf %>%
        filter(res %in% input$uot_res,
               between(dateid, date_range()[1], date_range()[2]),
               districtname %in% input$select_district,
               userroletype %in% input$uot_userrole) %>%
        select(-districtname, -userroletype, -res, -tot_users) %>%
        rename(facet_col = grade_band)
      
      label <- labs(title = 'Usage Over Time',
                    subtitle = paste0('District: ', input$select_district, '\n',
                                      'User Type: ',input$uot_userrole,'\n',
                                      input$select_year))
      
      height <- if_else(n_distinct(df$facet_col) <= 2, 450, 700)
    }
    
  }
  
  else if(input$uot_usage_type == 'Most Used Product Groups'){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet=='False'){
      req(!is.null(input$uot_usage_type_pg_options))
      
      df <- uot_top_4_product_group_rdf %>%
        filter(res %in% input$uot_res,
               between(dateid, date_range()[1], date_range()[2]),
               product_group %in% input$uot_usage_type_pg_options,
               districtname %in% input$select_district) %>%
        select(-districtname, -product_group,-res, -tot_users) %>%
        rename(facet_col = userroletype)
      
      label <- labs(title = 'Usage Over Time',
                    subtitle = paste0('District: ', input$select_district, '\n',
                                      'Product Group: ',input$uot_usage_type_pg_options,'\n',
                                      input$select_year))
      
      height <- if_else(n_distinct(df$facet_col) == 1, 450, 700)
    }
    
    else if(input$uot_facet=='True'){
      req(!is.null(input$uot_userrole))
      
      df <- uot_top_4_product_group_rdf %>%
        filter(res %in% input$uot_res,
               between(dateid, date_range()[1], date_range()[2]),
               districtname %in% input$select_district,
               userroletype %in% input$uot_userrole) %>%
        select(-districtname, -userroletype, -res, -tot_users) %>%
        rename(facet_col = product_group)
      
      label <- labs(title = 'Usage Over Time',
                    subtitle = paste0('District: ', input$select_district, '\n',
                                      'User Type: ',input$uot_userrole,'\n',
                                      input$select_year))
      
      height <- if_else(n_distinct(df$facet_col) <= 2, 450, 700)
    }
    
  }
  
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
    pivot_longer(cols = c(-facet_col, -dateid),
                 names_to = 'feature', values_to = 'value')
  
  list(data = df_3, label = label, height = height)
  
})

#----avg session duration over time data prep-----

uot_asd_plot_data <- reactive({
  req(!is.null(input$uot_res), !is.null(input$select_district), !is.null(input$uot_usage_type),
      !is.null(date_range()))
  
  validate(
    need(date_range()[1] <= date_range()[2], 'Invalid Duration')
  )
  
  if(input$uot_usage_type == 'Overall'){
    df <- uot_asd_overall_rdf %>%
      filter(res %in% input$uot_res,
             between(dateid, date_range()[1], date_range()[2]),
             districtname %in% input$select_district) %>%
      select(-districtname, -res, -n_users, -tot_users) %>%
      rename(facet_col = userroletype)
    
    label <- labs(title = 'Average Session Duration Over Time',
                  subtitle = paste0('District: ',input$select_district,'\n',
                                    input$select_year))
    
    height <- if_else(n_distinct(df$facet_col) == 1, 450, 700)
  }
  
  else if(input$uot_usage_type == 'Subject'){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet=='False'){
      req(!is.null(input$uot_usage_type_subject_options))
      
      df <- uot_asd_subject_rdf %>%
        filter(res %in% input$uot_res,
               between(dateid, date_range()[1], date_range()[2]),
               subject %in% input$uot_usage_type_subject_options,
               districtname %in% input$select_district) %>%
        select(-districtname, -subject, -res, -n_users, -tot_users) %>%
        rename(facet_col = userroletype)
      
      label <- labs(title = 'Average Session Duration Over Time',
                    subtitle = paste0('District: ',input$select_district,'\n',
                                      'Subject: ',input$uot_usage_type_subject_options,'\n',
                                      input$select_year))
      
      height <- if_else(n_distinct(df$facet_col) == 1, 450, 700)
    }
    
    else if(input$uot_facet=='True'){
      req(!is.null(input$uot_userrole))
      
      df <- uot_asd_subject_rdf %>%
        filter(res %in% input$uot_res,
               between(dateid, date_range()[1], date_range()[2]),
               userroletype %in% input$uot_userrole,
               districtname %in% input$select_district) %>%
        select(-districtname, -userroletype, -res, -n_users, -tot_users) %>%
        rename(facet_col = subject)
      
      label <- labs(title = 'Average Session Duration Over Time',
                    subtitle = paste0('District: ',input$select_district, '\n',
                                      'User Type: ',input$uot_userrole,'\n',
                                      input$select_year))
      
      height <- if_else(n_distinct(df$facet_col) <= 2, 450, 700)
    }
    
  }
  
  else if(input$uot_usage_type == 'Grade Band'){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet=='False'){
      req(!is.null(input$uot_usage_type_gb_options))
      
      df <- uot_asd_grade_band_rdf %>%
        filter(res %in% input$uot_res,
               between(dateid, date_range()[1], date_range()[2]),
               grade_band %in% input$uot_usage_type_gb_options,
               districtname %in% input$select_district) %>%
        select(-districtname, -grade_band,-res, -n_users, -tot_users) %>%
        rename(facet_col = userroletype)
      
      label <- labs(title = 'Average Session Duration Over Time',
                    subtitle = paste0('District: ', input$select_district, '\n',
                                      'Grade Band: ',input$uot_usage_type_gb_options,'\n',
                                      input$select_year))
      
      height <- if_else(n_distinct(df$facet_col) == 1, 450, 700)
    }
    
    else if(input$uot_facet=='True'){
      req(!is.null(input$uot_userrole))
      
      df <- uot_asd_grade_band_rdf %>%
        filter(res %in% input$uot_res,
               between(dateid, date_range()[1], date_range()[2]),
               districtname %in% input$select_district,
               userroletype %in% input$uot_userrole) %>%
        select(-districtname, -userroletype, -res, -n_users, -tot_users) %>%
        rename(facet_col = grade_band)
      
      label <- labs(title = 'Average Session Duration Over Time',
                    subtitle = paste0('District: ', input$select_district, '\n',
                                      'User Type: ',input$uot_userrole,'\n',
                                      input$select_year))
      
      height <- if_else(n_distinct(df$facet_col) <= 2, 450, 700)
    }
    
  }
  
  else if(input$uot_usage_type == 'Most Used Product Groups'){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet=='False'){
      req(!is.null(input$uot_usage_type_pg_options))
      
      df <- uot_asd_top_4_product_group_rdf %>%
        filter(res %in% input$uot_res,
               between(dateid, date_range()[1], date_range()[2]),
               product_group %in% input$uot_usage_type_pg_options,
               districtname %in% input$select_district) %>%
        select(-districtname, -product_group, -res, -n_users, -tot_users) %>%
        rename(facet_col = userroletype)
      
      label <- labs(title = 'Average Session Duration Over Time',
                    subtitle = paste0('District: ', input$select_district, '\n',
                                      'Product Group: ',input$uot_usage_type_pg_options,'\n',
                                      input$select_year))
      
      height <- if_else(n_distinct(df$facet_col) == 1, 450, 700)
    }
    
    else if(input$uot_facet=='True'){
      req(!is.null(input$uot_userrole))
      
      df <- uot_asd_top_4_product_group_rdf %>%
        filter(res %in% input$uot_res,
               between(dateid, date_range()[1], date_range()[2]),
               districtname %in% input$select_district,
               userroletype %in% input$uot_userrole) %>%
        select(-districtname, -userroletype, -res, -n_users, -tot_users) %>%
        rename(facet_col = product_group)
      
      label <- labs(title = 'Average Session Duration Over Time',
                    subtitle = paste0('District: ', input$select_district, '\n',
                                      'User Type: ',input$uot_userrole,'\n',
                                      input$select_year))
      
      height <- if_else(n_distinct(df$facet_col) <= 2, 450, 700)
    }
    
  }
  
  list(data = df, label = label, height = height)
  
})

#-----events per user over time data prep----

uot_aeu_plot_data <- reactive({
  req(!is.null(input$uot_res), !is.null(input$select_district), !is.null(input$uot_usage_type),
      !is.null(date_range()))
  
  validate(
    need(date_range()[1] <= date_range()[2], 'Invalid Duration')
  )
  
  if(input$uot_usage_type == 'Overall'){
    df <- uot_aeu_overall_rdf %>%
      filter(res %in% input$uot_res,
             between(dateid, date_range()[1], date_range()[2]),
             districtname %in% input$select_district) %>%
      select(-districtname, -res, -n_users, -tot_users) %>%
      rename(facet_col = userroletype)
    
    label <- labs(title = 'Average Events Per User Over Time',
                  subtitle = paste0('District: ',input$select_district,'\n',
                                    input$select_year))
    
    height <- if_else(n_distinct(df$facet_col) == 1, 450, 700)
  }
  
  else if(input$uot_usage_type == 'Subject'){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet=='False'){
      req(!is.null(input$uot_usage_type_subject_options))
      
      df <- uot_aeu_subject_rdf %>%
        filter(res %in% input$uot_res,
               between(dateid, date_range()[1], date_range()[2]),
               subject %in% input$uot_usage_type_subject_options,
               districtname %in% input$select_district) %>%
        select(-districtname, -subject, -res, -n_users, -tot_users) %>%
        rename(facet_col = userroletype)
      
      label <- labs(title = 'Average Events Per User Over Time',
                    subtitle = paste0('District: ',input$select_district,'\n',
                                      'Subject: ',input$uot_usage_type_subject_options,'\n',
                                      input$select_year))
      
      height <- if_else(n_distinct(df$facet_col) == 1, 450, 700)
    }
    
    else if(input$uot_facet=='True'){
      req(!is.null(input$uot_userrole))
      
      df <- uot_aeu_subject_rdf %>%
        filter(res %in% input$uot_res,
               between(dateid, date_range()[1], date_range()[2]),
               userroletype %in% input$uot_userrole,
               districtname %in% input$select_district) %>%
        select(-districtname, -userroletype, -res, -n_users, -tot_users) %>%
        rename(facet_col = subject)
      
      label <- labs(title = 'Average Events Per User Over Time',
                    subtitle = paste0('District: ',input$select_district, '\n',
                                      'User Type: ',input$uot_userrole,'\n',
                                      input$select_year))
      
      height <- if_else(n_distinct(df$facet_col) <= 2, 450, 700)
    }
    
  }
  
  else if(input$uot_usage_type == 'Grade Band'){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet=='False'){
      req(!is.null(input$uot_usage_type_gb_options))
      
      df <- uot_aeu_grade_band_rdf %>%
        filter(res %in% input$uot_res,
               between(dateid, date_range()[1], date_range()[2]),
               grade_band %in% input$uot_usage_type_gb_options,
               districtname %in% input$select_district) %>%
        select(-districtname, -grade_band,-res, -n_users, -tot_users) %>%
        rename(facet_col = userroletype)
      
      label <- labs(title = 'Average Events Per User Over Time',
                    subtitle = paste0('District: ', input$select_district, '\n',
                                      'Grade Band: ',input$uot_usage_type_gb_options,'\n',
                                      input$select_year))
      
      height <- if_else(n_distinct(df$facet_col) == 1, 450, 700)
    }
    
    else if(input$uot_facet=='True'){
      req(!is.null(input$uot_userrole))
      
      df <- uot_aeu_grade_band_rdf %>%
        filter(res %in% input$uot_res,
               between(dateid, date_range()[1], date_range()[2]),
               districtname %in% input$select_district,
               userroletype %in% input$uot_userrole) %>%
        select(-districtname, -userroletype, -res, -n_users, -tot_users) %>%
        rename(facet_col = grade_band)
      
      label <- labs(title = 'Average Events Per User Over Time',
                    subtitle = paste0('District: ', input$select_district, '\n',
                                      'User Type: ',input$uot_userrole,'\n',
                                      input$select_year))
      
      height <- if_else(n_distinct(df$facet_col) <= 2, 450, 700)
    }
    
  }
  
  else if(input$uot_usage_type == 'Most Used Product Groups'){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet=='False'){
      req(!is.null(input$uot_usage_type_pg_options))
      
      df <- uot_aeu_top_4_product_group_rdf %>%
        filter(res %in% input$uot_res,
               between(dateid, date_range()[1], date_range()[2]),
               product_group %in% input$uot_usage_type_pg_options,
               districtname %in% input$select_district) %>%
        select(-districtname, -product_group, -res, -n_users, -tot_users) %>%
        rename(facet_col = userroletype)
      
      label <- labs(title = 'Average Events Per User Over Time',
                    subtitle = paste0('District: ', input$select_district, '\n',
                                      'Product Group: ',input$uot_usage_type_pg_options,'\n',
                                      input$select_year))
      
      height <- if_else(n_distinct(df$facet_col) == 1, 450, 700)
    }
    
    else if(input$uot_facet=='True'){
      req(!is.null(input$uot_userrole))
      
      df <- uot_aeu_top_4_product_group_rdf %>%
        filter(res %in% input$uot_res,
               between(dateid, date_range()[1], date_range()[2]),
               districtname %in% input$select_district,
               userroletype %in% input$uot_userrole) %>%
        select(-districtname, -userroletype, -res, -n_users, -tot_users) %>%
        rename(facet_col = product_group)
      
      label <- labs(title = 'Average Events Per User Over Time',
                    subtitle = paste0('District: ', input$select_district, '\n',
                                      'User Type: ',input$uot_userrole,'\n',
                                      input$select_year))
      
      height <- if_else(n_distinct(df$facet_col) <= 2, 450, 700)
    }
    
  }
  
  list(data = df, label = label, height = height)
  
})

#----table data prep-----

uot_table_data <- reactive({
  req(!is.null(input$uot_res), !is.null(date_range()), !is.null(input$uot_usage_type),
      !is.null(input$select_district))
  
  validate(
    need(date_range()[1] <= date_range()[2], 'Invalid Duration')
  )
  
  res_unit <- c('Day' = 'day', 'Week' = 'week', 'Month' = 'month')
  
  start_dateid <- if_else(input$uot_res == 'Week',
                          ceiling_date(date_range()[1], unit = 'week', change_on_boundary = FALSE),
                          date_range()[1])
  
  end_dateid <- if_else(input$uot_res == 'Week',
                        floor_date(date_range()[2], unit = 'week'),
                        date_range()[2])
  
  if(input$uot_usage_type == 'Overall'){
    
    df_1 <- uot_overall_rdf %>%
      filter(res %in% input$uot_res,
             between(dateid, date_range()[1], date_range()[2]),
             districtname %in% input$select_district) %>%
      mutate(new = new-bouncing, return = return-leaving) %>%
      select(-districtname, -res)
    
    validate(
      need(nrow(df_1) != 0, 'No Usage for Selected Filters')
    )
    
    df_2 <- uot_asd_overall_rdf %>%
      filter(res %in% input$uot_res,
             between(dateid, date_range()[1], date_range()[2]),
             districtname %in% input$select_district) %>%
      select(userroletype, dateid, avg_duration)
    
    df_3 <- uot_aeu_overall_rdf %>%
      filter(res %in% input$uot_res,
             between(dateid, date_range()[1], date_range()[2]),
             districtname %in% input$select_district) %>%
      select(userroletype, dateid, n_events_per_user)
    
    full_table <- expand_grid(dateid = seq.Date(from = start_dateid, to = end_dateid, by = res_unit[input$uot_res]),
                              userroletype = c('Student','Teacher'))
    
    df_1 %>%
      inner_join(y = df_2, by = c('dateid','userroletype')) %>%
      inner_join(y = df_3, by = c('dateid','userroletype')) %>%
      right_join(y = full_table, by = c('dateid','userroletype')) %>%
      replace_na(list(tot_users = 0, new = 0, return = 0, leaving = 0, bouncing = 0, avg_duration = 0, n_events_per_user = 0)) %>%
      pivot_wider(names_from = userroletype,
                  values_from = c(tot_users, new, return, leaving, bouncing, avg_duration, n_events_per_user),
                  names_sep = '.',
                  values_fill = list(tot_users = 0, new = 0, return = 0, leaving = 0, bouncing = 0, avg_duration = 0, n_events_per_user = 0)) %>%
      mutate(districtname = input$select_district) %>%
      select(districtname, dateid, ends_with('.Student'), ends_with('.Teacher')) %>%
      arrange(dateid) %>%
      mutate(dateid = if(input$uot_res == 'Month') paste0(month.abb[month(dateid)],'-',year(dateid)) else paste0(day(dateid),' ',month.abb[month(dateid)],', ',year(dateid)))
  }
  
  else if(input$uot_usage_type == 'Subject'){
    
    df_1 <- uot_subject_rdf %>%
      filter(res %in% input$uot_res,
             between(dateid, date_range()[1], date_range()[2]),
             districtname %in% input$select_district) %>%
      mutate(new = new-bouncing, return = return-leaving) %>%
      select(-districtname, -res)
    
    validate(
      need(nrow(df_1) != 0, 'No Usage for Selected Filters')
    )
    
    df_2 <- uot_asd_subject_rdf %>%
      filter(res %in% input$uot_res,
             between(dateid, date_range()[1], date_range()[2]),
             districtname %in% input$select_district) %>%
      select(subject, userroletype, dateid, avg_duration)
    
    df_3 <- uot_aeu_subject_rdf %>%
      filter(res %in% input$uot_res,
             between(dateid, date_range()[1], date_range()[2]),
             districtname %in% input$select_district) %>%
      select(subject, userroletype, dateid, n_events_per_user)
    
    
    subjects <- unique(df_1$subject)
    
    full_table <- expand_grid(dateid = seq.Date(from = start_dateid, to = end_dateid, by = res_unit[input$uot_res]),
                              userroletype = c('Student','Teacher'),
                              subject = subjects)
    
    df_1 %>%
      inner_join(y = df_2, by = c('subject','dateid','userroletype')) %>%
      inner_join(y = df_3, by = c('subject','dateid','userroletype')) %>%
      right_join(y = full_table, by = c('subject','dateid','userroletype')) %>%
      replace_na(list(tot_users = 0, new = 0, return = 0, leaving = 0, bouncing = 0, avg_duration = 0, n_events_per_user = 0)) %>%
      pivot_wider(names_from = userroletype,
                  values_from = c(tot_users, new, return, leaving, bouncing, avg_duration, n_events_per_user),
                  names_sep = '.',
                  values_fill = list(tot_users = 0, new = 0, return = 0, leaving = 0, bouncing = 0, avg_duration = 0, n_events_per_user = 0)) %>%
      mutate(districtname = input$select_district) %>%
      select(districtname, subject, dateid, ends_with('.Student'), ends_with('.Teacher')) %>%
      arrange(subject, dateid) %>%
      mutate(dateid = if(input$uot_res == 'Month') paste0(month.abb[month(dateid)],'-',year(dateid)) else paste0(day(dateid),' ',month.abb[month(dateid)],', ',year(dateid)))
  }
  
  else if(input$uot_usage_type == 'Grade Band'){
    
    df_1 <- uot_grade_band_rdf %>%
      filter(res %in% input$uot_res,
             between(dateid, date_range()[1], date_range()[2]),
             districtname %in% input$select_district) %>%
      mutate(new = new-bouncing, return = return-leaving) %>%
      select(-districtname, -res)
    
    validate(
      need(nrow(df_1) != 0, 'No Usage for Selected Filters')
    )
    
    df_2 <- uot_asd_grade_band_rdf %>%
      filter(res %in% input$uot_res,
             between(dateid, date_range()[1], date_range()[2]),
             districtname %in% input$select_district) %>%
      select(grade_band, userroletype, dateid, avg_duration)
    
    df_3 <- uot_aeu_grade_band_rdf %>%
      filter(res %in% input$uot_res,
             between(dateid, date_range()[1], date_range()[2]),
             districtname %in% input$select_district) %>%
      select(grade_band, userroletype, dateid, n_events_per_user)
    
    gbs <- unique(df_1$grade_band)
    
    full_table <- expand_grid(dateid = seq.Date(from = start_dateid, to = end_dateid, by = res_unit[input$uot_res]),
                              userroletype = c('Student','Teacher'),
                              grade_band = gbs)
    
    df_1 %>%
      inner_join(y = df_2, by = c('grade_band','dateid','userroletype')) %>%
      inner_join(y = df_3, by = c('grade_band','dateid','userroletype')) %>%
      right_join(y = full_table, by = c('grade_band','dateid','userroletype')) %>%
      replace_na(list(tot_users = 0, new = 0, return = 0, leaving = 0, bouncing = 0, avg_duration = 0, n_events_per_user = 0)) %>%
      pivot_wider(names_from = userroletype,
                  values_from = c(tot_users, new, return, leaving, bouncing, avg_duration, n_events_per_user),
                  names_sep = '.',
                  values_fill = list(tot_users = 0, new = 0, return = 0, leaving = 0, bouncing = 0, avg_duration = 0, n_events_per_user = 0)) %>%
      mutate(districtname = input$select_district) %>%
      select(districtname, grade_band, dateid, ends_with('.Student'), ends_with('.Teacher')) %>%
      mutate(grade_band = factor(grade_band, levels = c('K-2','3-5','6-8','9-12'))) %>%
      arrange(grade_band, dateid) %>%
      mutate(grade_band = as.character(grade_band)) %>%
      mutate(dateid = if(input$uot_res == 'Month') paste0(month.abb[month(dateid)],'-',year(dateid)) else paste0(day(dateid),' ',month.abb[month(dateid)],', ',year(dateid)))
  }
  
  else if(input$uot_usage_type == 'Most Used Product Groups'){
    
    df_1 <- uot_top_4_product_group_rdf %>%
      filter(res %in% input$uot_res,
             between(dateid, date_range()[1], date_range()[2]),
             districtname %in% input$select_district) %>%
      mutate(new = new-bouncing, return = return-leaving) %>%
      select(-districtname, -res)
    
    validate(
      need(nrow(df_1) != 0, 'No Usage for Selected Filters')
    )
    
    df_2 <- uot_asd_top_4_product_group_rdf %>%
      filter(res %in% input$uot_res,
             between(dateid, date_range()[1], date_range()[2]),
             districtname %in% input$select_district) %>%
      select(product_group, userroletype, dateid, avg_duration)
    
    df_3 <- uot_aeu_top_4_product_group_rdf %>%
      filter(res %in% input$uot_res,
             between(dateid, date_range()[1], date_range()[2]),
             districtname %in% input$select_district) %>%
      select(product_group, userroletype, dateid, n_events_per_user)
    
    pgs <- unique(df_1$product_group)
    
    full_table <- expand_grid(dateid = seq.Date(from = start_dateid, to = end_dateid, by = res_unit[input$uot_res]),
                              userroletype = c('Student','Teacher'),
                              product_group = pgs)
    
    df_1 %>%
      inner_join(y = df_2, by = c('product_group','dateid','userroletype')) %>%
      inner_join(y = df_3, by = c('product_group','dateid','userroletype')) %>%
      right_join(y = full_table, by = c('product_group','dateid','userroletype')) %>%
      replace_na(list(tot_users = 0, new = 0, return = 0, leaving = 0, bouncing = 0, avg_duration = 0, n_events_per_user = 0)) %>%
      pivot_wider(names_from = userroletype,
                  values_from = c(tot_users, new, return, leaving, bouncing, avg_duration, n_events_per_user),
                  names_sep = '.',
                  values_fill = list(tot_users = 0, new = 0, return = 0, leaving = 0, bouncing = 0, avg_duration = 0, n_events_per_user = 0)) %>%
      mutate(districtname = input$select_district) %>%
      select(districtname, product_group, dateid, ends_with('.Student'), ends_with('.Teacher')) %>%
      arrange(product_group, dateid) %>%
      mutate(dateid = if(input$uot_res == 'Month') paste0(month.abb[month(dateid)],'-',year(dateid)) else paste0(day(dateid),' ',month.abb[month(dateid)],', ',year(dateid)))
  }
  
})

#-------OUTPUT ELEMENTS-----

#-----uot plot-----
output$uot_plot <- renderPlot({
  req(!is.null(input$uot_res), !is.null(input$uot_usage_type))
  
  validate(
    need(nrow(uot_plot_data()$data) != 0, 'No Usage for Selected Filters')
  )
  
  if(input$uot_usage_type == 'Overall'){
    
    usage_over_time_plot(uot_plot_data()$data, input$uot_res, 'False', 'False') +
      uot_plot_data()$label
  }
  
  else if(input$uot_usage_type %in% c('Subject', 'Grade Band', 'Most Used Product Groups')){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet == 'False'){
      usage_over_time_plot(uot_plot_data()$data, input$uot_res, input$uot_facet, 'False') +
        uot_plot_data()$label
    }
    
    else if(input$uot_facet == 'True'){
      req(!is.null(input$uot_fix_y))
      
      usage_over_time_plot(uot_plot_data()$data, input$uot_res, input$uot_facet, input$uot_fix_y) +
        uot_plot_data()$label
    }
  }
  
}, height = function(){
  uot_plot_data()$height
})

#-----asd plot-----
output$uot_asd_plot <- renderPlot({
  req(!is.null(input$uot_res), !is.null(input$uot_usage_type))
  
  validate(
    need(nrow(uot_asd_plot_data()$data) != 0, 'No Usage for Selected Filters')
  )
  
  if(input$uot_usage_type == 'Overall'){
    
    avg_session_duration_over_time_plot(uot_asd_plot_data()$data, input$uot_res, 'False', 'False') +
      uot_asd_plot_data()$label
  }
  
  else if(input$uot_usage_type %in% c('Subject', 'Grade Band', 'Most Used Product Groups')){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet == 'False'){
      avg_session_duration_over_time_plot(uot_asd_plot_data()$data, input$uot_res, input$uot_facet, 'False') +
        uot_asd_plot_data()$label
    }
    
    else if(input$uot_facet == 'True'){
      req(!is.null(input$uot_fix_y))
      
      avg_session_duration_over_time_plot(uot_asd_plot_data()$data, input$uot_res, input$uot_facet, input$uot_fix_y) +
        uot_asd_plot_data()$label
    }
  }
  
}, height = function(){
  uot_asd_plot_data()$height
})

#------aeu plot----
output$uot_aeu_plot <- renderPlot({
  req(!is.null(input$uot_res), !is.null(input$uot_usage_type))
  
  validate(
    need(nrow(uot_aeu_plot_data()$data) != 0, 'No Usage for Selected Filters')
  )
   
  if(input$uot_usage_type == 'Overall'){
    
    avg_events_per_user_over_time_plot(uot_aeu_plot_data()$data, input$uot_res, 'False', 'False') +
      uot_aeu_plot_data()$title
  }
  
  else if(input$uot_usage_type %in% c('Subject', 'Grade Band', 'Most Used Product Groups')){
    req(!is.null(input$uot_facet))
    
    if(input$uot_facet == 'False'){
      avg_events_per_user_over_time_plot(uot_aeu_plot_data()$data, input$uot_res, input$uot_facet, 'False') +
        uot_aeu_plot_data()$title
    }
    
    else if(input$uot_facet == 'True'){
      req(!is.null(input$uot_fix_y))
      
      avg_events_per_user_over_time_plot(uot_aeu_plot_data()$data, input$uot_res, input$uot_facet, input$uot_fix_y) +
        uot_aeu_plot_data()$title
    }
  }
  
}, height = function(){
  uot_aeu_plot_data()$height
})

#------table-----

output$uot_table <- DT::renderDataTable({
  req(!is.null(input$uot_res), !is.null(input$uot_usage_type))
  
  # validate(
  #   need(nrow(uot_table_data()) != 0, 'No Usage for Selected Filters')
  # )
  
  dateid_header <- c(Day = 'Date', Week = 'Date(binned by week)', Month = 'Month')
  
  if(input$uot_usage_type == "Overall"){
    tags_html <- tags$table(class = "display",
                            tags$thead(do.call(tags$tr, c(list(tags$th(rowspan = 2, 'District'),
                                                               tags$th(rowspan = 2, dateid_header[input$uot_res]),
                                                               tags$th(colspan = 7, "Student"),
                                                               tags$th(colspan = 7, "Teacher")))),
                                       tags$tr(lapply(rep(c("# Total Users","# New Users", "# Returning Users", "# Leaving Users", "# Bouncing Users", "Avg Session Duration(mins)", "Avg Events per User"), 2), tags$th))))
  }
  else if(input$uot_usage_type == 'Grade Band'){
    tags_html <- tags$table(class = "display", 
                            tags$thead(do.call(tags$tr, c(list(tags$th(rowspan = 2, "District"),
                                                               tags$th(rowspan = 2, "Grade Band"),
                                                               tags$th(rowspan = 2, dateid_header[input$uot_res]),
                                                               tags$th(colspan = 7, "Student"),
                                                               tags$th(colspan = 7, "Teacher")))),
                                       tags$tr(lapply(rep(c("# Total Users","# New Users", "# Returning Users", "# Leaving Users", "# Bouncing Users", "Avg Session Duration(mins)", "Avg Events per User"), 2), tags$th))))
  }
  else if(input$uot_usage_type == 'Subject'){
    tags_html <- tags$table(class = "display", 
                            tags$thead(do.call(tags$tr, c(list(tags$th(rowspan = 2, "District"),
                                                               tags$th(rowspan = 2, "Discipline"),
                                                               tags$th(rowspan = 2, dateid_header[input$uot_res]),
                                                               tags$th(colspan = 7, "Student"),
                                                               tags$th(colspan = 7, "Teacher")))),
                                       tags$tr(lapply(rep(c("# Total Users","# New Users", "# Returning Users", "# Leaving Users", "# Bouncing Users", "Avg Session Duration(mins)", "Avg Events per User"), 2), tags$th))))
  }
  else if(input$uot_usage_type == 'Most Used Product Groups'){
    tags_html <- tags$table(class = "display", 
                            tags$thead(do.call(tags$tr, c(list(tags$th(rowspan = 2, "District"),
                                                               tags$th(rowspan = 2, "Product Group"),
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
  
  init_tbl <- DT::datatable(uot_table_data(), container = sketch, filter = 'top',
                            rownames = FALSE, class = 'cell-border stripe',
                            options(list(autoWidth = TRUE, sDom  = '<"top">lrt<"bottom">ip',
                                         FixedHeader = TRUE, pageLength = 15)))
  
  purrr::reduce(columns, function(x, y) {
    DT::formatStyle(x, y, backgroundColor = DT::styleInterval(brks[[y]], clrs[[y]]))
    
  }, .init = init_tbl)
})







