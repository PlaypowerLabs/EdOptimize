output$ubg_usage_type_options_ui <- renderUI({
  req(input$ubg_usage_type %in% c('Grade Band', 'Subject', 'Product Group'))
  
  if(input$ubg_usage_type == 'Subject'){
    selectInput(inputId = 'ubg_usage_type_subject_options', label = 'Subject',
                choices = subjects)
  }
  
  else if(input$ubg_usage_type == 'Grade Band'){
    selectInput(inputId = 'ubg_usage_type_gb_options', label = 'Grade Band',
                choices = grade_bands)
  }
  
  else if(input$ubg_usage_type == 'Product Group'){
    selectInput(inputId = 'ubg_usage_type_pg_options', label = 'Product Group',
                choices = product_groups)
  }
})

heatmap_data <- reactive({
  req(!is.null(input$ubg_usage_type))
  
  if(input$ubg_usage_type == 'Overall'){
    df <- ubg_overall_rdf
  }
  else if(input$ubg_usage_type == 'Subject'){
    req(!is.null(input$ubg_usage_type_subject_options))
    
    df <- ubg_subject_rdf %>%
      filter(subject %in% input$ubg_usage_type_subject_options)
  }
  else if(input$ubg_usage_type == 'Grade Band'){
    req(!is.null(input$ubg_usage_type_gb_options))
    
    df <- ubg_grade_band_rdf %>%
      filter(grade_band %in% input$ubg_usage_type_gb_options)
  }
  else if(input$ubg_usage_type == 'Product Group'){
    req(!is.null(input$ubg_usage_type_pg_options))
    
    df <- ubg_product_group_rdf %>%
      filter(product_group %in% input$ubg_usage_type_pg_options)
  }
  
  
  geo_join(spatial_data = states, data_frame = df, by_sp = 'NAME', by_df = 'state')
})

dotchart_data <- reactive({
  req(!is.null(input$ubg_usage_type))
  
  if(input$ubg_usage_type == 'Overall'){
    df <- ubg_dc_overall_rdf
  }
  else if(input$ubg_usage_type == 'Subject'){
    req(!is.null(input$ubg_usage_type_subject_options))
    
    df <- ubg_dc_subject_rdf %>%
      filter(subject %in% input$ubg_usage_type_subject_options)
  }
  else if(input$ubg_usage_type == 'Grade Band'){
    req(!is.null(input$ubg_usage_type_gb_options))
    
    df <- ubg_dc_grade_band_rdf %>%
      filter(grade_band %in% input$ubg_usage_type_gb_options)
  }
  else if(input$ubg_usage_type == 'Product Group'){
    req(!is.null(input$ubg_usage_type_pg_options))
    
    df <- ubg_dc_product_group_rdf %>%
      filter(product_group %in% input$ubg_usage_type_pg_options)
  }
  
  
  df
})




select_col <- reactive({
  req(!is.null(input$ubg_usage_metric))
  
  columns <- c('# Students' = 'n_students', '# Teachers' = 'n_teachers', '# Sessions' = 'n_sessions',
                    '# Events per Student' = 'n_events_per_student', '# Events per Teacher' = 'n_events_per_teacher')
  columns[input$ubg_usage_metric]
})


colorpal_heatmap <- reactive({
  req(!is.null(heatmap_data()))
  
  colorNumeric(palette = 'Blues', domain = heatmap_data()[[select_col()]])
})

colorpal_dotchart <- reactive({
  req(!is.null(dotchart_data()))
  
  colorNumeric(palette = 'Blues', domain = dotchart_data()[[select_col()]])
})



popup_heatmap <- reactive({
  req(!is.null(heatmap_data()))
  
  
  str_c("<strong>", heatmap_data()$NAME, "</strong>",
        "<br/># Districts: ", heatmap_data()$n_districts,
        "<br/># Schools: ", heatmap_data()$n_schools,
        "<br/># Students: ", heatmap_data()$n_students,
        "<br/># Teachers: ", heatmap_data()$n_teachers,
        "<br/># Sessions: ", heatmap_data()$n_sessions) %>% 
    lapply(htmltools::HTML)
})

popup_dotchart <- reactive({
  req(!is.null(dotchart_data()))
  
  
  str_c("<strong>", dotchart_data()$districtname, "</strong>",
        "<br/># Schools: ", dotchart_data()$n_schools,
        "<br/># Students: ", dotchart_data()$n_students,
        "<br/># Teachers: ", dotchart_data()$n_teachers,
        "<br/># Sessions: ", dotchart_data()$n_sessions) %>% 
    lapply(htmltools::HTML)
})

output$ubg_heatmap <- renderLeaflet({
  
  initial_heatmap_data <- geo_join(spatial_data = states, data_frame = ubg_overall_rdf, by_sp = 'NAME', by_df = 'state')
  initial_colorpal <- colorNumeric(palette = 'Blues', domain = initial_heatmap_data$n_sessions)
  initial_popup_heatmap <- str_c("<strong>", initial_heatmap_data$NAME, "</strong>",
                         "<br/># Districts: ", initial_heatmap_data$n_districts,
                         "<br/># Schools: ", initial_heatmap_data$n_schools,
                         "<br/># Students: ", initial_heatmap_data$n_students,
                         "<br/># Teachers: ", initial_heatmap_data$n_teachers,
                         "<br/># Sessions: ", initial_heatmap_data$n_sessions) %>% 
    lapply(htmltools::HTML)
  
  leaflet(data = initial_heatmap_data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(-95, 37, zoom = 4) %>%
    addPolygons(
      fillColor = ~initial_colorpal(initial_heatmap_data$n_sessions),
      fillOpacity = 1,
      weight = 0.2,
      smoothFactor = 0.2,
      highlight = highlightOptions(
        weight = 5,
        color = "#b4b8cc",
        fillColor = "#b4b8cc",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label=initial_popup_heatmap,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
    addLegend(position = "bottomleft",
              pal = initial_colorpal,
              values = ~initial_heatmap_data$n_sessions,
              title = '# Sessions',
              bins = 5)

})


output$ubg_dotchart <- renderLeaflet({
  
  initial_dotchart_data <- ubg_dc_overall_rdf
  initial_colorpal <- colorNumeric(palette = 'Blues', domain = initial_dotchart_data$n_sessions, alpha = TRUE)
  initial_popup_dotchart <- str_c("<strong>", initial_dotchart_data$districtname, "</strong>",
                                 "<br/># Schools: ", initial_dotchart_data$n_schools,
                                 "<br/># Students: ", initial_dotchart_data$n_students,
                                 "<br/># Teachers: ", initial_dotchart_data$n_teachers,
                                 "<br/># Sessions: ", initial_dotchart_data$n_sessions) %>% 
    lapply(htmltools::HTML)
  
  leaflet(data = initial_dotchart_data) %>%
    addProviderTiles(providers$Stamen.TonerLite) %>%
    setView(-95, 37, zoom = 4) %>%
    addCircles(
      lat = ~lat,
      lng = ~long,
      color = ~initial_colorpal(initial_dotchart_data$n_sessions),
      radius = ~(n_sessions/max(n_sessions))*100000,
      fillOpacity = 1,
      weight = 1,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#b4b8cc",
        fillColor = "#b4b8cc",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label=initial_popup_dotchart,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
    addLegend(position = "bottomleft",
              pal = initial_colorpal,
              values = ~initial_dotchart_data$n_sessions,
              title = '# Sessions',
              bins = 5)
  
})
observe({
  req(!is.null(heatmap_data()))
  leafletProxy("ubg_heatmap", data = heatmap_data()) %>%
    clearShapes() %>%
    addPolygons(
      fillColor = ~colorpal_heatmap()(heatmap_data()[[select_col()]]),
      fillOpacity = 1,
      weight = 0.2,
      smoothFactor = 0.2,
      highlight = highlightOptions(
        weight = 5,
        color = "#b4b8cc",
        fillColor = "#b4b8cc",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label=popup_heatmap(),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))
  
})

observe({
  req(!is.null(dotchart_data()))
  leafletProxy("ubg_dotchart", data = dotchart_data()) %>%
    clearShapes() %>%
    addCircles(
      lat = ~lat,
      lng = ~long,
      color = ~colorpal_dotchart()(dotchart_data()[[select_col()]]),
      radius = ~dotchart_data()[[select_col()]]/max(dotchart_data()[[select_col()]])*100000,
      fillOpacity = 1,
      weight = 1,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#b4b8cc",
        fillColor = "#b4b8cc",
        fillOpacity = 0.5,
        bringToFront = TRUE),
      label=popup_dotchart(),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))
  
})

observe({
  req(!is.null(heatmap_data()))
  proxy <- leafletProxy("ubg_heatmap", data = heatmap_data())
  
  proxy %>%
    clearControls()
    
  pal <- colorpal_heatmap()
  values <- heatmap_data()[[select_col()]]
  title <- input$ubg_usage_metric
  proxy %>%
    addLegend(position = "bottomleft",
              pal = pal,
              values = values,
              title = title,
              bins = 5
    )
})

observe({
  req(!is.null(dotchart_data()))
  proxy <- leafletProxy("ubg_dotchart", data = dotchart_data())
  
  proxy %>%
    clearControls()
  
  pal <- colorpal_dotchart()
  values <- dotchart_data()[[select_col()]]
  title <- input$ubg_usage_metric
  proxy %>%
    addLegend(position = "bottomleft",
              pal = pal,
              values = values,
              title = title,
              bins = 5
    )
})
