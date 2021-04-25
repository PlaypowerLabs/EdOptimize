#-------UI ELEMENTS-------

#----usage type filter ui element-----

output$ou_usage_type_filter_ui <- renderUI({
  req(input$ou_usage_type %in% c('Grade Band Usage', 'Product Group Usage'))
  
  if(input$ou_usage_type == 'Grade Band Usage'){
    radioButtons(inputId = 'ou_usage_type_gb_filter', label = 'Filter Grade Band by:',
                 choices = c('Subject','None'),
                 selected = 'None')
  }
  
  else if(input$ou_usage_type == 'Product Group Usage'){
    radioButtons(inputId = 'ou_usage_type_pg_filter', label = 'Filter Product Group by:',
                 choices = c('Subject','Grade Band','None'),
                 selected = 'None')
  }
  
})

#----usage type filter options ui element-----

output$ou_usage_type_filter_options_ui <- renderUI({
  req(input$ou_usage_type %in% c('Grade Band Usage', 'Product Group Usage'))
  
  if(input$ou_usage_type == 'Grade Band Usage'){
    req(input$ou_usage_type_gb_filter %in% c('Subject'))
    
    if(input$ou_usage_type_gb_filter == 'Subject'){
      req(!is.null(input$select_district))
      
      x <- ou_grade_band_wise_filter_subject_rdf %>%
        filter(districtname %in% input$select_district) %$%
        unique(subject)
      
      selectInput(inputId = 'ou_usage_type_gb_filter_options_sub', label = 'Select Subject:',
                  choices = x)
    }
    
  }
  
  else if(input$ou_usage_type == 'Product Group Usage'){
    req(input$ou_usage_type_pg_filter %in% c('Subject', 'Grade Band'))
    
    if(input$ou_usage_type_pg_filter == 'Subject'){
      req(!is.null(input$select_district))
      
      x <- ou_product_group_wise_filter_subject_rdf %>%
        filter(districtname %in% input$select_district) %$%
        unique(subject)
  
      selectInput(inputId = 'ou_usage_type_pg_filter_options_sub', label = 'Select Subject:',
                  choices = x)
    }
    else if(input$ou_usage_type_pg_filter == 'Grade Band'){
      req(!is.null(input$select_district))
      
      x <- ou_product_group_wise_filter_grade_band_rdf %>%
        filter(districtname %in% input$select_district) %$%
        unique(grade_band)
      
      selectInput(inputId = 'ou_usage_type_pg_filter_options_gb', label = 'Select Grade Band:',
                  choices = x)
    }
  }
  
})

#-----infoboxes ui element------
output$ou_info_boxes_ui <- renderUI({
  
  fluidPage(
    fluidRow(infoBoxOutput("ou_n_product_groups"), infoBoxOutput("ou_n_schools"), infoBoxOutput("ou_n_teachers")), 
    fluidRow(infoBoxOutput("ou_n_products"), infoBoxOutput("ou_n_classes"), infoBoxOutput("ou_n_students"))
  )
  
})

#------usage plot ui----

output$ou_plot_ui <- renderUI({
  fluidPage(
    fluidRow(box(title = "Teacher", background = "blue", solidHeader = TRUE, 
                 plotOutput("ou_teacher_plot", height = "400px", width = "100%")),
             box(title = "Student", background = "blue", solidHeader = TRUE,
                 plotOutput("ou_student_plot", height = "400px", width = "100%")))
  )
})

#-----active weeks plot ui------

output$ou_plot_2_ui <- renderUI({
  fluidPage(
    fluidRow(box(title = "Teacher", background = "blue", solidHeader = TRUE,
                 plotOutput("ou_aw_teacher_plot", height = "400px", width = "100%")),
             box(title = "Student", background = "blue", solidHeader = TRUE,
                 plotOutput("ou_aw_student_plot", height = "400px", width = "100%")))
  )
})

#-----REACTIVE COMPONENTS----
#---usage plot data prep-----

ou_plot_table_data <- reactive({
  req(!is.null(input$ou_usage_type), !is.null(input$ou_usage_metric), !is.null(input$select_district))
  
  if(input$ou_usage_type == 'Subject Usage'){
    df <- ou_subject_wise_rdf %>%
      filter(districtname %in% input$select_district) %>%
      select(subject, userroletype, n_users, perc_users, n_sessions, perc_sessions) %>%
      mutate(summariser = subject)
  }
  
  else if(input$ou_usage_type == 'Grade Band Usage'){
    
    req(!is.null(input$ou_usage_type_gb_filter))
    
    if(input$ou_usage_type_gb_filter == 'None'){
      df <- ou_grade_band_wise_filter_none_rdf %>%
        filter(districtname %in% input$select_district) %>%
        select(grade_band, userroletype, n_users, perc_users, n_sessions, perc_sessions) %>%
        mutate(summariser = grade_band)
    }
    
    
    else if(input$ou_usage_type_gb_filter == 'Subject'){
      req(!is.null(input$ou_usage_type_gb_filter_options_sub))
      
      df <- ou_grade_band_wise_filter_subject_rdf %>%
        filter(districtname %in% input$select_district,
               subject == input$ou_usage_type_gb_filter_options_sub) %>%
        select(subject,grade_band, userroletype, n_users, perc_users, n_sessions, perc_sessions) %>%
        mutate(summariser = grade_band)
    }
  }
  
  else if(input$ou_usage_type == 'Product Group Usage'){
    
    req(!is.null(input$ou_usage_type_pg_filter))
    
    if(input$ou_usage_type_pg_filter == 'None'){
      df <- ou_product_group_wise_filter_none_rdf %>%
        filter(districtname %in% input$select_district) %>%
        select(product_group, userroletype, n_users, perc_users, n_sessions, perc_sessions) %>%
        mutate(summariser = product_group)
    }
    
    
    else if(input$ou_usage_type_pg_filter == 'Subject'){
      req(!is.null(input$ou_usage_type_pg_filter_options_sub))
      
      df <- ou_product_group_wise_filter_subject_rdf %>%
        filter(districtname %in% input$select_district,
               subject == input$ou_usage_type_pg_filter_options_sub) %>%
        select(subject,product_group, userroletype, n_users, perc_users, n_sessions, perc_sessions) %>%
        mutate(summariser = product_group)
      
    }
    
    else if(input$ou_usage_type_pg_filter == 'Grade Band'){
      req(!is.null(input$ou_usage_type_pg_filter_options_gb))
      
      df <- ou_product_group_wise_filter_grade_band_rdf %>%
        filter(districtname %in% input$select_district,
               grade_band == input$ou_usage_type_pg_filter_options_gb) %>%
        select(grade_band, product_group, userroletype, n_users, perc_users, n_sessions, perc_sessions) %>%
        mutate(summariser = product_group)
    }
    
  }
  
  if(input$ou_usage_metric == '# Users'){
    df_2 <- df %>% 
      mutate(filler = perc_users)
  }
  
  else if(input$ou_usage_metric == '# Sessions'){
    df_2 <- df %>%
      mutate(filler = perc_sessions)
  }
  
  df_3 <- df_2 %>%
    select(summariser, userroletype, filler) %>%
    group_by(userroletype) %>%
    arrange(desc(filler)) %>%
    mutate(summariser = if_else(row_number()>4, 'Other', summariser)) %>%
    ungroup() %>%
    group_by(userroletype, summariser) %>%
    summarise(filler = sum(filler)) %>%
    arrange(desc(filler)) %>%
    ungroup()
  
  col_mappings <- c(subject = 'Subject', grade_band = 'Grade Band', product_group = 'Product Group',
                    userroletype = 'User Role', n_users = '# Users', n_sessions = '# Sessions',
                    perc_users = '% Users', perc_sessions = '% Sessions', summariser = 'summariser')
  
  new_col_names <- col_mappings[names(df)]
  names(df) <- new_col_names
  
  list(plot_data = df_3,
       table_data = df %>%
         select(-summariser) %>%
         arrange(desc(`# Users`)))
  
})

#-----overall usage plot color pal-----

ou_col_pal <- reactive({
  req(!is.null(ou_plot_table_data()$plot_data))
  
  all_summariser <- unique(ou_plot_table_data()$plot_data$summariser)
  all_summariser_without_other <- all_summariser[all_summariser != 'Other']
  other_col <- 'grey90'
  pal <- c('#fa9191','#87f5ff','#59ffbd','#fffd8c','#ffc966','#ad7aff','#96a3ff','#99fff0')
  
  c(setNames(object = pal[1:length(all_summariser_without_other)], all_summariser_without_other), 'Other' = other_col)
  
})

#------active weeks data prep-----

ou_aw_data <- reactive({
  req(!is.null(input$ou_active_weeks_filter), !is.null(input$select_district))
  
  if(input$ou_active_weeks_filter == 'Total Active Weeks'){
    df <- ou_active_weeks_abs_rdf %>%
      filter(districtname %in% input$select_district) %>%
      select(act_weeks_label, userroletype, perc_users, color)
  }
  
  else if(input$ou_active_weeks_filter == '% of Active Weeks'){
    df <- ou_active_weeks_perc_rdf %>%
      filter(districtname %in% input$select_district) %>%
      select(act_weeks_label, userroletype, perc_users, color)
  }
  
  df
})


#-----OUTPUT ELEMENTS------

#-----infobox outputs-----

output$ou_n_product_groups <- renderInfoBox({
  req(!is.null(input$select_district))
  
  infoBox(format(as.numeric(ou_infobox_rdf$n_product_groups[ou_infobox_rdf$districtname == input$select_district]), big.mark=","), "Number of Product Groups", 
          color = "teal", icon = icon("fas fa-book"), fill = TRUE)
})

output$ou_n_schools <- renderInfoBox({
  req(!is.null(input$select_district))
  
  infoBox(format(as.numeric(ou_infobox_rdf$n_schools[ou_infobox_rdf$districtname == input$select_district]), big.mark=","), "Number of Schools", 
          color = "teal", icon = icon("fas fa-school"), fill = TRUE)
})

output$ou_n_teachers <- renderInfoBox({
  req(!is.null(input$select_district))
  
  infoBox(format(as.numeric(ou_infobox_rdf$n_teachers[ou_infobox_rdf$districtname == input$select_district]), big.mark=","), "Number of Teachers", 
          color = "teal", icon = icon("fas fa-chalkboard-teacher"), fill = TRUE)
})

output$ou_n_products <- renderInfoBox({
  req(!is.null(input$select_district))
  
  infoBox(format(as.numeric(ou_infobox_rdf$n_products[ou_infobox_rdf$districtname == input$select_district]), big.mark=","), "Number of Products", 
          color = "teal", icon = icon("fas fa-book-open"), fill = TRUE)
})

output$ou_n_classes <- renderInfoBox({
  req(!is.null(input$select_district))
  
  infoBox(format(as.numeric(ou_infobox_rdf$n_classes[ou_infobox_rdf$districtname == input$select_district]), big.mark=","), "Number of Classes", 
          color = "teal", icon = icon("fas fa-users"), fill = TRUE)
})

output$ou_n_students <- renderInfoBox({
  req(!is.null(input$select_district))
  
  infoBox(format(as.numeric(ou_infobox_rdf$n_students[ou_infobox_rdf$districtname == input$select_district]), big.mark=","), "Number of Students", 
          color = "teal", icon = icon("fas fa-user"), fill = TRUE)
})

#-----overall usage plot----
output$ou_teacher_plot <- renderPlot({
  req(!is.null(ou_plot_table_data()$plot_data), !is.null(ou_col_pal()))
  
  validate(
    need(nrow(ou_plot_table_data()$plot_data %>%
                filter(userroletype == 'Teacher')) != 0, 'No Teacher Usage')
  )
  
  overall_summary_pie_plot(ou_plot_table_data()$plot_data, 'Teacher', ou_col_pal())
})

output$ou_student_plot <- renderPlot({
  req(!is.null(ou_plot_table_data()$plot_data), !is.null(ou_col_pal()))
  
  validate(
    need(nrow(ou_plot_table_data()$plot_data %>%
                filter(userroletype == 'Student')) != 0, 'No Student Usage')
  )
  
  overall_summary_pie_plot(ou_plot_table_data()$plot_data, 'Student', ou_col_pal())
})

#-----overall usage table-----
output$ou_table <- DT::renderDataTable({
  req(!is.null(ou_plot_table_data()$table_data))
  
  ncols <- ncol(ou_plot_table_data()$table_data)
  columns <- colnames(ou_plot_table_data()$table_data)[(ncols-3):ncols]
  break_col <- break_col_func(ou_plot_table_data()$table_data, columns)
  
  brks <- break_col$brks
  clrs <- break_col$clrs
  
  init_tbl <- DT::datatable(ou_plot_table_data()$table_data, filter = 'top', options = list(
    pageLength = 10, autoWidth = TRUE, sDom  = '<"top">lrt<"bottom">ip'
  ), class = 'cell-border stripe')
  
  purrr::reduce(columns, function(x, y) {
    DT::formatStyle(x, y, backgroundColor = DT::styleInterval(brks[[y]], clrs[[y]]))
    
  }, .init = init_tbl)
  
})

#-----active weeks plot------

output$ou_aw_teacher_plot <- renderPlot({
  req(!is.null(ou_aw_data()))
  
  validate(
    need(nrow(ou_aw_data() %>%
                filter(userroletype == 'Teacher')) != 0, 'No Teacher Usage')
  )
  
  active_weeks_pie_plot(ou_aw_data(), 'Teacher')
})

output$ou_aw_student_plot <- renderPlot({
  req(!is.null(ou_aw_data()))
  
  validate(
    need(nrow(ou_aw_data() %>%
                filter(userroletype == 'Student')) != 0, 'No Student Usage')
  )
  
  active_weeks_pie_plot(ou_aw_data(), 'Student')
})

