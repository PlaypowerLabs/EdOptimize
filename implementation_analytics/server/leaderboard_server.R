
#----UI ELEMENTS-----
#----select product ui-----
output$lb_sel_product_ui <- renderUI({
  req(!is.null(input$select_district), !is.null(input$lb_usage_type))
  
  if(input$lb_usage_type == 'By Product'){
    x <- district_products %>%
      filter(districtname %in% input$select_district) %>%
      .$product
    
    selectInput(inputId = 'lb_sel_product', label = 'Select Product:', 
                choices = x)
  }

})

#---heading ui----
output$lb_heading_ui <- renderUI({
  req(!is.null(input$lb_leaderboard_type))
  
  if(input$lb_leaderboard_type == 'School'){
    fluidPage(
      h4('School Leaderboard'),
      hr(style="border-top: 1px solid #000000;"),
    )
  }
  
  else if(input$lb_leaderboard_type == 'Class'){
    fluidPage(
      h4('Class Leaderboard'),
      hr(style="border-top: 1px solid #000000;"),
    )
  }
})

#-----plot ui-----
output$lb_plot_ui <- renderUI({
  
  s <- input$lb_leaderboard_rows_selected
  
  if(length(s)){
    req(!is.null(input$lb_leaderboard_type))
    
    if(input$lb_leaderboard_type == 'School'){
      fluidPage(
        h4('School Explorer'),
        hr(style="border-top: 1px solid #000000;"),
        tabsetPanel(id = 'lb_tab_panel',
                    tabPanel(
                      title = 'New vs Returning Users',
                      value = 'nvr',
                      plotOutput(outputId = 'lb_nvr_plot', height = input$dimension[2] * .4) %>%
                        withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')
                    ),
                    tabPanel(
                      title = 'Events per User Over Time',
                      value = 'eot',
                      plotOutput(outputId = 'lb_eot_plot', height = input$dimension[2] * .4) %>%
                        withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')
                    ))
      )
    }
    
    else if(input$lb_leaderboard_type == 'Class'){
      fluidPage(
        h4('Class Explorer'),
        hr(style="border-top: 1px solid #000000;"),
        tabsetPanel(id = 'lb_tab_panel',
                    tabPanel(
                      title = 'New vs Returning Users',
                      value = 'nvr',
                      plotOutput(outputId = 'lb_nvr_plot', height = input$dimension[2] * .44) %>%
                        withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')
                    ),
                    tabPanel(
                      title = 'Events per User Over Time',
                      value = 'eot',
                      plotOutput(outputId = 'lb_eot_plot', height = input$dimension[2] * .44) %>%
                        withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')
                    ))
      )
    }
    
  }
})


#------REACTIVE COMPONENTS-----
#-----leaderboard data prep----
lb_leaderboard_data <- reactive({
  req(!is.null(input$select_district), !is.null(input$lb_leaderboard_type),
      !is.null(input$lb_usage_type))
  
  if(input$lb_leaderboard_type == 'School'){
   
    if(input$lb_usage_type == 'Overall'){
     
      df <- lb_schoolwise_overall_rdf %>%
        filter(districtname %in% input$select_district) %>%
        select(-districtname)
    }
    
    else if(input$lb_usage_type == 'By Product'){
      req(!is.null(input$lb_sel_product))
      
      df <- lb_schoolwise_by_product_rdf %>%
        filter(districtname %in% input$select_district,
               product %in% input$lb_sel_product) %>%
        select(-districtname, -product)
    }
    
  }
  
  else if(input$lb_leaderboard_type == 'Class'){
    
    if(input$lb_usage_type == 'Overall'){
      
      df <- lb_classwise_overall_rdf %>%
        filter(districtname %in% input$select_district) %>%
        select(-districtname)
    }
    
    else if(input$lb_usage_type == 'By Product'){
      req(!is.null(input$lb_sel_product))
      
      df <- lb_classwise_by_product_rdf %>%
        filter(districtname %in% input$select_district,
               product %in% input$lb_sel_product) %>%
        select(-districtname, -product)
    }
    
  }
  
  scale_cols <- names(df)[c(-1)]
  
  scaled_score_df <- map(scale_cols, function(x){
    scale(df[[x]])
  }) %>%
    bind_cols()
  
  smart_select <- function(df, lb_type){
    if(lb_type == 'School'){
      dplyr::select(df, 'Rank' = rank, 'School Name' = schoolname, 'Avg Active Days per User' = avg_active_days,
                    'Avg Contents per User' = avg_contents, '# Students' = n_students, '# Teachers' = n_teachers,
                    'Avg Student Events' = avg_student_events, 'Avg Teacher Events' = avg_teacher_events)
    }
    else if(lb_type == 'Class'){
      dplyr::select(df, 'Rank' = rank, 'Class Name' = classname, 'Avg Active Days per User' = avg_active_days,
                    'Avg Contents per User' = avg_contents, '# Students' = n_students, '# Teachers' = n_teachers,
                    'Avg Student Events' = avg_student_events, 'Avg Teacher Events' = avg_teacher_events)
    }
  }
  
  df_2 <- df %>%
    mutate(scaled_score_sum = rowSums(scaled_score_df)) %>%
    arrange(desc(scaled_score_sum)) %>%
    mutate(rank = row_number()) %>%
    smart_select(input$lb_leaderboard_type)
  
  df_2
  
})

#----nvr plot data prep----
lb_nvr_plot_data <- reactive({
  req(!is.null(input$lb_leaderboard_type),!is.null(input$lb_usage_type),
      !is.null(input$lb_leaderboard_rows_selected))
  
  if(input$lb_leaderboard_type == 'School'){
   
    school <- lb_leaderboard_data()[['School Name']][input$lb_leaderboard_rows_selected]
    if(input$lb_usage_type == 'Overall'){
      
      df <- lb_schoolwise_overall_nvr_rdf %>%
        filter(schoolname %in% school) %>%
        select(-schoolname) %>%
        pivot_longer(cols = c(new,returning), names_to = 'feature', values_to = 'value') %>%
        mutate(feature = factor(feature, levels = c('returning','new'), labels = c('Returning User', 'New User')))
      
      label <- labs(
        title = 'Overall Usage',
        subtitle = paste0('School: ', school)
      )
    } 
    
    else if(input$lb_usage_type == 'By Product'){
      req(!is.null(input$lb_sel_product))
      
      df <- lb_schoolwise_by_product_nvr_rdf %>%
        filter(schoolname %in% school,
               product %in% input$lb_sel_product) %>%
        select(-schoolname, -product) %>%
        pivot_longer(cols = c(new,returning), names_to = 'feature', values_to = 'value') %>%
        mutate(feature = factor(feature, levels = c('returning','new'), labels = c('Returning User', 'New User')))
      
      label <- labs(
        title = paste0('Product: ', input$lb_sel_product),
        subtitle = paste0('School: ', school)
      )
    }
    
    
  }
  
  else if(input$lb_leaderboard_type == 'Class'){
    
    Class <- lb_leaderboard_data()[['Class Name']][input$lb_leaderboard_rows_selected]
    if(input$lb_usage_type == 'Overall'){
      
      df <- lb_classwise_overall_nvr_rdf %>%
        filter(classname %in% Class) %>%
        select(-classname) %>%
        pivot_longer(cols = c(new,returning), names_to = 'feature', values_to = 'value') %>%
        mutate(feature = factor(feature, levels = c('returning','new'), labels = c('Returning User', 'New User')))
      
      label <- labs(
        title = 'Overall Usage',
        subtitle = paste0('Class: ', Class)
      )
    } 
    
    else if(input$lb_usage_type == 'By Product'){
      req(!is.null(input$lb_sel_product))
      
      df <- lb_classwise_by_product_nvr_rdf %>%
        filter(classname %in% Class,
               product %in% input$lb_sel_product) %>%
        select(-classname, -product) %>%
        pivot_longer(cols = c(new,returning), names_to = 'feature', values_to = 'value') %>%
        mutate(feature = factor(feature, levels = c('returning','new'), labels = c('Returning User', 'New User')))
      
      label <- labs(
        title = paste0('Product: ', input$lb_sel_product),
        subtitle = paste0('Class: ', Class)
      )
    }
    
  }
  
  list(data = df, label = label)
  
})

#-----eot plot data prep----
lb_eot_plot_data <- reactive({
  req(!is.null(input$lb_leaderboard_type),!is.null(input$lb_usage_type),
      !is.null(input$lb_leaderboard_rows_selected))
  
  if(input$lb_leaderboard_type == 'School'){
    
    school <- lb_leaderboard_data()[['School Name']][input$lb_leaderboard_rows_selected]
    if(input$lb_usage_type == 'Overall'){
      
      df <- lb_schoolwise_overall_eot_rdf %>%
        filter(schoolname %in% school) %>%
        select(-schoolname)
      
      label <- labs(
        title = 'Overall Usage',
        subtitle = paste0('School: ', school)
      )
    } 
    
    else if(input$lb_usage_type == 'By Product'){
      req(!is.null(input$lb_sel_product))
      
      df <- lb_schoolwise_by_product_eot_rdf %>%
        filter(schoolname %in% school,
               product %in% input$lb_sel_product) %>%
        select(-schoolname, -product)
      
      label <- labs(
        title = paste0('Product: ', input$lb_sel_product),
        subtitle = paste0('School: ', school)
      )
    }
    
    
  }
  
  else if(input$lb_leaderboard_type == 'Class'){
    
    Class <- lb_leaderboard_data()[['Class Name']][input$lb_leaderboard_rows_selected]
    if(input$lb_usage_type == 'Overall'){
      
      df <- lb_classwise_overall_eot_rdf %>%
        filter(classname %in% Class) %>%
        select(-classname)
      
      label <- labs(
        title = 'Overall Usage',
        subtitle = paste0('Class: ', Class)
      )
    } 
    
    else if(input$lb_usage_type == 'By Product'){
      req(!is.null(input$lb_sel_product))
      
      df <- lb_classwise_by_product_eot_rdf %>%
        filter(classname %in% Class,
               product %in% input$lb_sel_product) %>%
        select(-classname, -product)
      
      label <- labs(
        title = paste0('Product: ', input$lb_sel_product),
        subtitle = paste0('Class: ', Class)
      )
    }
    
  }
  
  list(data = df, label = label)
})




#-----OUTPUT ELEMENTS-----
#----leaderboard output----
options(DT.options = list(pageLength = 15))
output$lb_leaderboard <- DT::renderDataTable({
  
  DT::datatable(lb_leaderboard_data(), filter = 'none', class = 'cell-border stripe', 
                options(list(scrollX = TRUE, sDom  = '<"top">lrt<"bottom">ip',
                             columnDefs = list(list(className = 'dt-center', targets = 0)))), rownames= FALSE,
                selection = 'single', escape = FALSE)
  
})

#----nvr plot----
output$lb_nvr_plot <- renderPlot({
  req(!is.null(input$lb_leaderboard_rows_selected))
  
  district_new_vs_returning_users_plot(lb_nvr_plot_data()$data) +
    lb_nvr_plot_data()$label
})

#-----eot plot----
output$lb_eot_plot <- renderPlot({
  req(!is.null(input$lb_leaderboard_rows_selected))
  
  district_events_per_user_over_time_plot(lb_eot_plot_data()$data) +
    lb_eot_plot_data()$label
})
