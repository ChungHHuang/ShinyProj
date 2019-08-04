## ui.R ##
library(shinydashboard)
library(DT)

shinyUI(dashboardPage(
  dashboardHeader(title = "Nice Ride MN"),
### Sidebar ######
  dashboardSidebar(
    sidebarUserPanel(name = "Chung-Hsuan Huang"),
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("address-card")),
      menuItem("Users", tabName = "user", icon = icon("user")),
      menuItem("Time", tabName = "time", icon = icon("calendar-alt")),
      menuItem("Station", tabName = "station", icon = icon("bicycle")),
      menuItem("Popular stations", tabName = "place", icon = icon("location-arrow")),
      menuItem("Interactive map", tabName = "map", icon = icon("map-marked-alt"))
    )
  ),

  dashboardBody(
    tabItems(
      # Introduction ####
      tabItem(tabName = "intro",
              fluidRow(box(width = 12,h1('Data visualization and analysis of Nice Ride'),
              "Minneapolis is one of the top bicycle-friendly city in the US. 
               Biking is an eco-friendly way to explore the city, lakes, and rivers in Minneapolis.",br(),
              "Nice Ride is a non-profit bike-sharing system in Minneapolis starting in 2010.",br(), 
              "This app uses the data from Nice Ride (04/2018-06/2019) to provide an easy way to visualize and analyze 
              the user group and the famous places at different time.",br(),br(),
              "The data can be found ", a(href="https://www.niceridemn.com/system-data", "here")
              ,br(),br(),br(),
              'Here is a brief introduction to the tabs on the left.',br(),
              'Users:',br(),
              'This tab shows the user groups in four different categories: gender, age (age group), user type, and bike type.',br(),
              'User can see the interaction between two categories by choosing the category in a column.',br(),
              'User types are subscriber and customer (one time or one day users).',br(),
              'Bike types are classic (with docker) and dockless (park anywhere).',br(),br(),
              'Time:',br(),
              'This tab shows the numbers of trips based on the month, day, and hour.',br(),
              'User can see different user behavior at different times.',br(),br(),
              'Station:',br(),
              'This tab is similar to the time tab, but the user can choose a specific station to do the analysis.',br(),br(),
              'Popular stations:',br(),
              'This tab uses a map to visualize the famous station in Minneapolis.',br(),
              'The slide bar on the right-hand side provides an easy way to visualize the popular stations at different time and age.',br(),br(),
              'Interactive map:',br(),
              'This interactive map allows users to choose the start (green circle) and end (orange circle) stations.',br(),
              'The median travel time based on the data will show up on the right-hand side.',br(),
              'Once the user clicks "Find me the closest light rail station,"  user can see a marker showing the nearest light rail station and the median travel time.',br(),
              'Besides, the bike station closest to the light rail station will popup on the map.'))
              ),
      # User tab ####
      tabItem(tabName = "user",
              # Plots box 
              fluidRow(box(plotOutput("userplot"),height = 430,width = 9),
              br(),
              br(),
              # Select inputs
              fluidRow(box(selectizeInput("selected","Select category to display",user_choice),
                           selectizeInput("fill","Select category in column",user_choice2),
                           selectizeInput("style","Select plot style",style),width = 3)),
              # Table
              fluidRow(column(12,offset = .1,
                box(DT::dataTableOutput("user_table"),width = 9))
              )
              )),
      # Time tab ####
      tabItem(tabName = "time",
              # Plots box 
              fluidRow(box(plotOutput("timeplot"),height = 430,width = 9),
              br(),
              br(),   
              # Select inputs
              fluidRow(box(selectizeInput("t_sel","Select time scale to display",time_choice),
                           selectizeInput("t_fill","Select category in column",time_choice2),
                           selectizeInput("t_style","Select plot style",style),width = 3)),
              # Table
              fluidRow(column(12,offset = .1,
                box(DT::dataTableOutput("time_table"),width = 9))
              )
              )),
      # Station tab ####
      tabItem(tabName = "station",
              # Plots box 
              fluidRow(box(plotOutput("stationplot"),height = 430,width = 9),
              br(),
              br(),   
              # Select inputs
              fluidRow(box(selectizeInput("sel_st","Select specific station",stat_choice),
                           selectizeInput("st_sel","Select category to display",time_choice),
                           selectizeInput("st_fill","Select category in column",station_choice2),
                           selectizeInput("st_style","Select plot style",style),width = 3)),
              # Table
              fluidRow(column(12,offset = .1,
                box(DT::dataTableOutput("station_table"),width = 9))
              )
              )),
      # Place tab ####
      tabItem(tabName = "place",
              # Map
              fluidRow(box(leafletOutput("nrmap",height = 710),height = 733,width = 9),
              # Select inputs
                # RadioButton1
              fluidRow(box(radioButtons("radio_wk", label = h3("Days of week"),
                            choices = list("All times" = 1, "Weekday" = 2, "Weekend" = 3), 
                            selected = 1),
                           # RadioButton2
                           radioButtons("radio_station", label = h3("Start/End station"),
                            choices = list("Start station" = 1, "End station" = 2), 
                            selected = 1),
                           sliderInput("slider_top", label = h3("Top n busy stations"), 
                            min = 1, max = 25, value = 10),
                           sliderInput("slider_hour", label = h3("Hour range"), 
                            min = 0, max = 24, value = c(0, 24)),
                           br(),
                           # Checkbox
                           checkboxInput("age_checkbox", label = "Consider age", value = F),
                           sliderInput("slider_age", label = h3("Age range"), 
                                      min = 18, max = 82, value = c(18, 82)),
                           align = "center",width=3)),
              # Table
              fluidRow(column(12,offset = .1,
                box(DT::dataTableOutput("place_table"),width = 9))
              )
              )),
      # Map tab ####
      tabItem(tabName = "map",
              # map
              fluidRow(box(leafletOutput("MNmap",height = 710),height = 733,width = 9),
              # Select inputs
              fluidRow(box(selectizeInput("s_sel","Select start station",stat_choice),
                           selectizeInput("e_sel","Select end station",stat_choice),
                           br(),
                           checkboxInput("show_st", "Find me the cloest light rail station", value = FALSE),
                           br(),
                           br(),
                           textOutput("t_dur"),width = 3))
              ))
    )
  )
))
