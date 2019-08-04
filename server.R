## server.R ##
shinyServer(function(input, output,session){
  # Because too many Unknow users are born in 1969 (system default), I made two data frame.
  # Anything relate to age will use nage data frame.
  if_age = reactive(
    if (input$selected %in% c("age","age_group") | input$fill == "age_group" |
        input$t_fill == "age_group" | input$st_fill == "age_group" ) {
      nage 
    } else {
      nride
    }
  )
  
  # Users tab ####
  # Most of data manipulations are done in preprocessing, so make plot directly
  # Plot
  output$userplot = renderPlot(
      if_age() %>%
        ggplot(aes_string(input$selected)) + 
        geom_bar(aes_string(fill=input$fill),position = input$style) + 
        xlab(str_to_title(input$selected)) +
        ylab("Number of trip")
  )
  # Table
  output$user_table = DT::renderDataTable({
      if_age()%>% group_by_(input$selected,input$fill) %>% summarise(count = n())
  })
  
  # Time tab ####
  # Plot
  output$timeplot = renderPlot(
      if_age() %>%
        ggplot(aes_string(input$t_sel)) + 
        geom_bar(aes_string(fill=input$t_fill),position = input$t_style) + 
        xlab(str_to_title(input$t_sel)) +
        ylab("Number of trip")
  )  
  # Table
  output$time_table = DT::renderDataTable({
      if_age()%>% group_by_(input$t_sel,input$t_fill) %>% summarise(count = n())
  })    
  
  # Station tab ####
  # Plot
  output$stationplot = renderPlot(
      if_age() %>% filter(s_s_name == input$sel_st) %>%
        ggplot(aes_string((input$st_sel))) +
        geom_bar(aes_string(fill=input$st_fill),position = input$st_style) +
        xlab(str_to_title(input$st_sel)) +
        ylab("Number of trip")
  )
  # Table
  output$station_table = DT::renderDataTable({
      if_age() %>% filter(s_s_name == input$sel_st) %>% group_by_(input$st_sel,input$st_fill) %>% summarise(count = n())
  })
  
  # Map tab ####
  # There are many options here, so use a reactive function to do the filters
  nrmapfunc = reactive(
    # Consider age
    if (input$age_checkbox) {
      # Weekday
      if (input$radio_wk=='2') {
                        # The station name for dockless bike is NULL, so filter out
        nage %>% filter(s_s_name!="NULL" & weekday_weekend=='weekday' &
                        # Select hour range  
                        hour >= input$slider_hour[1] & hour <= input$slider_hour[2] &
                        # Select age range  
                        age >= input$slider_age[1] & age <= input$slider_age[2]) 
      # Weekend  
      } else if (input$radio_station=='3'){
        nage %>% filter(s_s_name!="NULL" & weekday_weekend=='weekend' &
                        # Select hour range 
                        hour >= input$slider_hour[1] & hour <= input$slider_hour[2]&
                        # Select age range    
                        age >= input$slider_age[1] & age <= input$slider_age[2]) 
      # All days  
      } else {
        nage %>% filter(s_s_name!="NULL" &
                        # Select hour range 
                        hour >= input$slider_hour[1] & hour <= input$slider_hour[2]&
                        # Select age range  
                        age >= input$slider_age[1] & age <= input$slider_age[2]) 
      }
    # Does not consider age, so no age range
    } else {
      # Weekday
      if (input$radio_wk=='2') {
        nride %>% filter(s_s_name!="NULL" & weekday_weekend=='weekday' &
                         hour >= input$slider_hour[1] & hour <= input$slider_hour[2]) 
      # Weekend
      } else if (input$radio_station=='3'){
        nride %>% filter(s_s_name!="NULL" & weekday_weekend=='weekend' &
                         hour >= input$slider_hour[1] & hour <= input$slider_hour[2]) 
      # All day
      } else {
        nride %>% filter(s_s_name!="NULL" &
                         hour >= input$slider_hour[1] & hour <= input$slider_hour[2]) 
      }
    }
  )
  # Map
  output$nrmap = renderLeaflet({
    # Start stations
    if (input$radio_station=="1") {
      # Use head to select top n stations
      tops = head(nrmapfunc() %>% 
                    group_by(s_s_name,s_s_lat,s_s_long) %>% 
                    summarise(count = n()) %>% arrange(desc(count)),input$slider_top)
      # Make map and put markers on the map
      leaflet(tops) %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addCircleMarkers(lng = ~s_s_long,lat = ~s_s_lat, label = ~s_s_name, 
                         radius = ~(count/300),stroke = F,color = "navy", fillOpacity = 0.5)
    # End stations  
    } else {
      # Use head to select top n stations
      tops = head(nrmapfunc() %>% 
                    group_by(e_s_name,e_s_lat,e_s_long) %>% 
                    summarise(count = n()) %>% arrange(desc(count)),input$slider_top)
      # Make map and put markers on the map
      leaflet(tops) %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addCircleMarkers(lng = ~e_s_long,lat = ~e_s_lat, label = ~e_s_name, 
                         radius = ~(count/300),stroke = F,color = "navy", fillOpacity = 0.5)
    }
  })  
  # Table
  output$place_table = DT::renderDataTable({
    # Start stations
    if (input$radio_station=="1") {
      head(nrmapfunc() %>% 
                    group_by(s_s_name,s_s_lat,s_s_long) %>% 
                    summarise(count = n()) %>% arrange(desc(count)),input$slider_top)
    # End stations  
    } else {
      head(nrmapfunc() %>% 
                    group_by(e_s_name,e_s_lat,e_s_long) %>% 
                    summarise(count = n()) %>% arrange(desc(count)),input$slider_top)
    }
  })    

  # Interactive map tab ####
  # Update end station based on start station
  observe({
    E_stat = unique(nride[s_s_name == input$s_sel, e_s_name])
    updateSelectizeInput(
      session, "e_sel",
      choices = E_stat,
      selected = E_stat[1]
    )
  })  
  
  # Map
  output$MNmap = renderLeaflet({
    # Get the longitude and latitude of the start station
    S_stat = (nride %>% filter(s_s_name==input$s_sel,e_s_name==input$e_sel))[1,]
    # Check if "Find me light rail station" is chosen
    
    if (input$show_st) {
      
      # Find the light rail station closet to the start station based on long and lat
      # k is the nth station in the station database
      min_d = 1000
      
      for (i in 1:nrow(l_r_station)) {
       if ((l_r_station[[i,2]]-S_stat[[5]])**2+(l_r_station[[i,3]]-S_stat[[6]])**2 < min_d){
          min_d = (l_r_station[[i,2]]-S_stat[[5]])**2+(l_r_station[[i,3]]-S_stat[[6]])**2
          k = i
        }
      }
     
      min_d2 = 1000
      # Get the longitude and latitude of the end station based on the start station
      e_temp = nride %>% filter(s_s_name==input$s_sel) %>% group_by(e_s_name,e_s_lat,e_s_long) %>% summarise(count = n())
      # Find the bike station that closest to the light rail station
      for (i in 1:nrow(e_temp)) {
        if ((l_r_station[[k,2]]-e_temp[[i,2]])**2+(l_r_station[[k,3]]-e_temp[[i,3]])**2 < min_d2){
          min_d2 = (l_r_station[[k,2]]-e_temp[[i,2]])**2+(l_r_station[[k,3]]-e_temp[[i,3]])**2
          l = e_temp[[i,1]]
        }
      }
      
      # Obtain the infomation of the start and end station
      S_stat = (nride %>% filter(s_s_name==input$s_sel,e_s_name==l))[1,]
      # Start station is green circle
      # End station is orange circle
      # Light rail station is marked
      leaflet() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addCircleMarkers(data = S_stat, lng = ~e_s_long,lat = ~e_s_lat, label = ~e_s_name,
                         labelOptions = labelOptions(noHide = T),color = "orange", fillOpacity = 0.5)  %>%
        addCircleMarkers(data = S_stat, lng = ~s_s_long,lat = ~s_s_lat, label = ~s_s_name,
                         labelOptions = labelOptions(noHide = T),color = "green", fillOpacity = 0.8)  %>%
        addMarkers(data = l_r_station[k,], lng = ~stop_lon,lat = ~stop_lat,label = ~stop_name,
                   labelOptions = labelOptions(noHide = T))
      # Show the start and end station based on the user choice
    } else {
      leaflet() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addCircleMarkers(data = S_stat, lng = ~e_s_long,lat = ~e_s_lat, label = ~e_s_name,
                         labelOptions = labelOptions(noHide = T),color = "orange", fillOpacity = 0.5)  %>%
        addCircleMarkers(data = S_stat, lng = ~s_s_long,lat = ~s_s_lat, label = ~s_s_name,
                         labelOptions = labelOptions(noHide = T),color = "green", fillOpacity = 0.8) 
        
    }
  })
  
  # Show the traval time
  output$t_dur <- renderText({
    # I tried to use reactive to save time, but I need the vaiable k and l from the calculation
    # Also, R shows error when I put the loop in the reactive function 
    # Therefore, I decide to do the calculation again
    S_stat = (nride %>% filter(s_s_name==input$s_sel,e_s_name==input$e_sel))[1,]
    if (input$show_st) {
      min_d = 1000
      for (i in 1:nrow(l_r_station)) {
        if ((l_r_station[[i,2]]-S_stat[[5]])**2+(l_r_station[[i,3]]-S_stat[[6]])**2 < min_d){
          min_d = (l_r_station[[i,2]]-S_stat[[5]])**2+(l_r_station[[i,3]]-S_stat[[6]])**2
          k = i
        }
      }
      
      min_d2 = 1000
      e_temp = nride %>% filter(s_s_name==input$s_sel) %>% group_by(e_s_name,e_s_lat,e_s_long) %>% summarise(count = n())
      for (i in 1:nrow(e_temp)) {
        if ((l_r_station[[k,2]]-e_temp[[i,2]])**2+(l_r_station[[k,3]]-e_temp[[i,3]])**2 < min_d2){
          min_d2 = (l_r_station[[k,2]]-e_temp[[i,2]])**2+(l_r_station[[k,3]]-e_temp[[i,3]])**2
          l = e_temp[[i,1]]
        }
      }
      # If the bike station is already the closest station, travel time is zero
      if (input$s_sel==l){
        t_text = 0
      } else {
        t_text = nride %>% filter(s_s_name==input$s_sel,e_s_name==l) %>% summarise(median(t_duration))
      }
    } else {
      # If the start and end stations are the same, travel time is zero
      if (input$s_sel==input$e_sel){
        t_text = 0
      } else {
        t_text = nride %>% filter(s_s_name==input$s_sel,e_s_name==input$e_sel) %>% summarise(median(t_duration))
      }      
    }
    # Print travel time
    mins = t_text %/% 60
    secs = t_text %% 60
    paste("The median travel time is",as.character(mins),"min",as.character(secs),'sec')
  })
})

