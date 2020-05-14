# Initial required package 
#if (!require("dplyr")) install.packages("dplyr"); library(dplyr)


#options(shiny.trace = TRUE)  

# Setting working directory
#setwd(dirname(sys.frame(1)$ofile))
options(shiny.maxRequestSize = 300*1024^2)

#List all required packages - dplyr
packages = c("shiny", "shinydashboard", "shinyjs", "DT",  "readr", "shinycssloaders", "shinybusy", "reshape2",
             "shinyalert","ggplot2", "htmlwidgets","plotly","shinyWidgets", "tidyverse","countrycode","dplyr",
             "plyr","stringr", "leaflet","maps","rgeos","rworldmap","rworldxtra","lubridate")


# Install packages not yet installed
# installed_packages = packages %in% rownames(installed.packages())
# if (any(installed_packages == FALSE)) {
#    install.packages(packages[!installed_packages])
# }

library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(readr)
library(shinycssloaders)
library(shinybusy)
library(reshape2)
library(shinyalert)
library(ggplot2)
library(htmlwidgets)
library(plotly)
library(shinyWidgets)
library(tidyverse)
library(countrycode)
library(plyr)
library(dplyr)
library(plotly)
library(stringr)
library(leaflet)
library(maps)
library(rgeos)
library(rworldmap)
library(rworldxtra)
library(lubridate)
library(gapminder)
library(highcharter)
library(xts)

# Packages loading
# lapply(packages, library, character.only = TRUE) %>%  invisible()

formatThousands <- JS(
  "function(data) {",
  "return (data / 1000).toFixed(1) + 'K'",
  "}")

  
############################ menu


sidebar = dashboardSidebar(uiOutput("sidebarpanel") , collapsed = FALSE, disable = FALSE) 

l_today =  format(today(), "%m-%d-%Y")
l_prefix = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/'
l_file = paste0(l_prefix,l_today,'.csv')

if (!file.exists(l_file)) {
  l_today = format(today() - 1 , "%m-%d-%Y")
  }
  

l_title = paste0( "COVID DASHBOARD (Updated: ", l_today,')')

body <- dashboardBody(

      tags$head(
        tags$style(type = "text/css",
                   HTML("th { text-align: center; }")
                  )
               ),
  
    tags$head(tags$style(HTML(
        '.myClass { 
        font-size: 21px;
        line-height: 50px;
        text-align: center;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
    tags$script(HTML(paste0('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> ', l_title ,'</span>\');
      })
     '))),
    
    tabItems(
    
    tabItem("dashboard", class = "active",
            
            
            fluidRow(
                valueBoxOutput("exploration_pa_dat1", width = 3),
                valueBoxOutput("exploration_pa_dat2", width = 3),
                valueBoxOutput("exploration_pa_dat3", width = 3),
                valueBoxOutput("exploration_pa_dat4", width = 3)
            ), #fluidrow
            
            fluidRow(
                
                box(width=6, collapsible = TRUE,  title="Cases by Country",status="primary",solidHeader = TRUE,
                    column(width=12,
                           radioButtons(inputId ="case_type_chart1", inline=TRUE, "",
                                        c("Confirmed" = "Confirmed",
                                          "Recovered" = "Recovered",
                                          "Deaths" = "Deaths", 
                                          "Active" = "Active")),
                           leafletOutput("exploration_pa_geo", width = '100%' ,height="400" ) %>% withSpinner(color="#4262a8") )
                    
                ),# box close
                box(width=6, collapsible = TRUE,  title="Cumulative Cases Over Time",status="primary",solidHeader = TRUE,
                    
                    column(width=12, 
                           radioButtons(inputId ="case_type_chart2", inline=TRUE, "",
                                        c("Confirmed" = "Confirmed",
                                          "Recovered" = "Recovered",
                                          "Deaths" = "Deaths", 
                                          "Active" = "Active")),
                           highchartOutput("exploration_active"  )%>% withSpinner(color="#4262a8") 
                          
                         )
                           
                )# box close
                
            ), #fluidrow    
            
            fluidRow(
                
                box(width=6, collapsible = TRUE,  title="Data Summary",status="primary",solidHeader = TRUE,
                    column(width=12, DT::dataTableOutput("descriptive_dt" ,height="500")  %>% withSpinner(color="#4262a8") )
                    
                ),# box close
                box(width=6, collapsible = TRUE,  title="Cumulative Cases",status="primary",solidHeader = TRUE,
                    column(width=12, plotlyOutput("exploration_ani" , width = '100%' ,height="500") %>% withSpinner(color="#4262a8") )
                )# box close
                
            ) #fluidrow    
            
            
    ),
    
    tabItem("about",
            tags$div(
              tags$h4("Data Science Hackathon"), 
              "This app was built as part of the UCLA Extension Data Science Hackathon.",tags$br(),
              "The data used was obtained from COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University",tags$br(),
              tags$a(href="https://github.com/CSSEGISandData/COVID-19", "GitHub JHU CSSE account"),
              tags$br(),
              tags$h5("Team"),
              tags$ul(
                tags$li("Paramjit Singh"), 
                tags$li("Saul Ventura"), 
                tags$li("Ian  May")
              )
            )
      )
    
 ) #tabitems close


)




header = dashboardHeader(
    title = 'UCLA Hackaton' )






ui = dashboardPage(header, sidebar, body)



################################################




server <- function(input, output ) {
    
    
    USER = reactiveValues(df_covid_countries = NULL,df_covid_countries_top = NULL, df_covid_all = NULL, covid_merge_csv = NULL,df_covid_merge_top =NULL,
                          df_animation = NULL , last_day= NULL, a_day_before = NULL)
    


    #MENU
    
    
    output$sidebarpanel = renderUI({
        
        
        dashboardSidebar(
            
            
            sidebarMenu(
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")) ,


                sliderInput(inputId = "top_n",
                            label = "Top N Countries:",
                            min = 1,
                            max = 20,
                            value = 5),
                
                radioButtons(inputId ="case_type", "Sorted by:",
                             c("Confirmed" = "Confirmed",
                               "Recovered" = "Recovered",
                               "Deaths" = "Deaths",
                               "Active" = "Active",
                               "GDP" = "GDP",
                               "Population" = "Population"
                               )),
                
                checkboxInput("order_type", "Descendant?", TRUE),
                
                menuItem("Forecasting", tabName = "forecast", icon = icon("chart-line")) ,
                menuItem("About", tabName = "about", icon = icon("question"))

            )
            
        )
        
        
        
    })    
    
    
    
    
    # DATA LOAD

    
    
    f_file = function (l_date) {
        l_today =  format(l_date, "%m-%d-%Y")
        l_prefix = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/'
        l_file = paste0(l_prefix,l_today,'.csv')
        return(l_file)
    }
    f_file(today()-1) 
    
    if (file.exists( f_file(today()))) {
        l_last_day_file = f_file(today()) 
        l_a_day_before_file = f_file(today()-1) 
        l_2_day_before_file = f_file(today()-2) 
        l_last_day = today()
        
    } else {
        l_last_day_file = f_file(today()-1) 
        l_a_day_before_file = f_file(today()-2) 
        l_2_day_before_file = f_file(today()-3) 
        l_last_day = today() - 1
    }
    
    USER$last_day = as.character(l_last_day)
    USER$a_day_before = as.character(l_last_day - 1)
    
    
    
    covid_csv = readr::read_csv(l_last_day_file  , locale = readr::locale(encoding = "latin1") )
    covid_adb_csv = readr::read_csv(l_a_day_before_file  , locale = readr::locale(encoding = "latin1") )
    covid_2db_csv = readr::read_csv(l_2_day_before_file  , locale = readr::locale(encoding = "latin1") )
    
    
    df_covid_countries = covid_csv %>% dplyr::group_by(Country_Region,Last_Update) %>% 
      dplyr::summarise(Confirmed=sum(Confirmed), Deaths=sum(Deaths) ,Recovered=sum(Recovered), 
                       Active=sum(Active)) %>%
      dplyr::select(-c('Last_Update'))
    
    df_covid_countries = as.data.frame(df_covid_countries)
    

    df_covid_adb_countries = covid_adb_csv %>% 
      dplyr::select(c('Country_Region','Confirmed','Deaths','Recovered','Active','Last_Update'))
    
    df_covid_adb_countries = df_covid_adb_countries %>% dplyr::group_by(Country_Region,Last_Update) %>% 
        dplyr::summarise(Confirmed=sum(Confirmed), Deaths=sum(Deaths) ,Recovered=sum(Recovered), 
                         Active=sum(Active)) %>%
        dplyr::select(-c('Last_Update'))
    
    df_covid_adb_countries = df_covid_adb_countries %>% dplyr::rename(Confirmed_adb ='Confirmed' , Deaths_adb = 'Deaths',
                                                   Recovered_adb ='Recovered' , Active_adb = 'Active' )
    
    
    df_covid_2db_countries = covid_2db_csv %>% 
      dplyr::select(c('Country_Region','Confirmed','Deaths','Recovered','Active','Last_Update'))
    
    df_covid_2db_countries = df_covid_2db_countries %>% dplyr::group_by(Country_Region,Last_Update) %>% 
      dplyr::summarise(Confirmed=sum(Confirmed), Deaths=sum(Deaths) ,Recovered=sum(Recovered), 
                       Active=sum(Active)) %>%
      dplyr::select(-c('Last_Update'))
    
    df_covid_2db_countries = df_covid_2db_countries %>% dplyr::rename(Confirmed_2db ='Confirmed' , Deaths_2db = 'Deaths',
                                                                      Recovered_2db ='Recovered' , Active_2db = 'Active' )
    
    
    
    
    df_covid_join = df_covid_countries %>% 
        dplyr::inner_join(df_covid_adb_countries , by = c('Country_Region'='Country_Region') ) %>% 
        dplyr::inner_join(df_covid_2db_countries , by = c('Country_Region'='Country_Region') )
    
    df_gdp = gapminder
    df_gdp = df_gdp %>% dplyr::filter(year== 2007)
    df_gdp$GDP = df_gdp$pop * df_gdp$gdpPercap
    df_gdp$Population = df_gdp$pop 
    df_gdp = df_gdp %>% dplyr::select(c('country','GDP','Population'))
    df_gdp$country = as.character(df_gdp$country)
    df_gdp$country[df_gdp$country == "United States"] = "US"
    
    df_covid_join = df_covid_join %>% 
      dplyr::left_join(df_gdp , by = c('Country_Region'='country') )
    
    
print("df_covid_join")
print(str(df_covid_join))
    
    # get world map
    wmap <- getMap(resolution="high")
    # get centroids
    centroids <- gCentroid(wmap, byid=TRUE)
    # get a data.frame with centroids
    df_long_lat <- as.data.frame(centroids)
    names(df_long_lat) = c("long","lat")
    df_long_lat <- tibble::rownames_to_column(df_long_lat, "COUNTRY_NAME")
    
    df_covid_join$Country_Region[df_covid_join$Country_Region == "US"] = "United States of America"
    
    df_covid_join =  merge(df_covid_join, df_long_lat, by.x = "Country_Region", by.y = "COUNTRY_NAME")
    
    
    

    df_covid_join =df_covid_join %>% dplyr::rename(Lat ='lat' , Long_ = 'long' )
    
    df_covid_join$Mortality = df_covid_join$Deaths/df_covid_join$Confirmed
    df_covid_join$New_Cases_adb = ifelse(df_covid_join$Confirmed_adb - df_covid_join$Confirmed_2db>0,df_covid_join$Confirmed_adb - df_covid_join$Confirmed_2db,0)
    df_covid_join$New_Cases = ifelse(df_covid_join$Confirmed - df_covid_join$Confirmed_adb>0,df_covid_join$Confirmed - df_covid_join$Confirmed_adb,0)
    df_covid_join$Change = ((df_covid_join$Confirmed - df_covid_join$Confirmed_adb) - (df_covid_join$Confirmed_adb - df_covid_join$Confirmed_2db ) ) /(df_covid_join$Confirmed_adb - df_covid_join$Confirmed_2db )
    
    df_covid_join =df_covid_join %>% dplyr::rename(`New Cases`= 'New_Cases' , `New Cases from previous day`= 'New_Cases_adb','Country' = 'Country_Region' )
    
    
    ##
    
    
    df_covid_all = covid_csv %>% 
        dplyr::summarise(Confirmed=sum(Confirmed, na.rm = TRUE), Deaths=sum(Deaths, na.rm = TRUE) ,Recovered=sum(Recovered, na.rm = TRUE),  Active=sum(Active, na.rm = TRUE) ) 
    
    USER$df_covid_countries = as.data.frame( df_covid_join)
    USER$df_covid_all = as.data.frame( df_covid_all)
    
    
    
    
    
    
    
    
    observeEvent(c(input$order_type,input$top_n, input$case_type),{
    
      df_covid_countries_top = USER$df_covid_countries
      df_covid_merge_top = USER$covid_merge_csv
      
      print("input$order_type")
      print(input$order_type)
      print(str(df_covid_countries_top))
      print(str(input$top_n))
      print(str(input$case_type))
      
      if (input$order_type) {
        l_top  = input$top_n
        print("df_covid_countries_top 0")
        print(str(df_covid_countries_top))
      } else {
 
        l_top  = input$top_n*-1
        print("df_covid_countries_top 1")
        print(str(df_covid_countries_top))
      }

      print(str(df_covid_countries_top[[input$case_type]]))
      df_covid_countries_top =  df_covid_countries_top %>% 
        dplyr::filter(  df_covid_countries_top[[input$case_type]]  > 0) 
      
      df_covid_countries_top =  df_covid_countries_top %>% 
        dplyr::top_n(l_top, df_covid_countries_top[[input$case_type]]  ) 
      
      df_covid_countries_top = df_covid_countries_top %>% dplyr::arrange(desc(df_covid_countries_top[[input$case_type]])) 
      
      print("df_covid_countries_top")
      print(str(df_covid_countries_top))
      print(str(df_covid_merge_top))
      
      l_countries = unique(df_covid_countries_top$Country)
      
      df_covid_merge_top$Country[df_covid_merge_top$Country == "US"] = "United States of America"
      
      df_covid_merge_top =  df_covid_merge_top %>%  dplyr::filter(Country %in% l_countries) 
      
      df_covid_merge_top$Country[df_covid_merge_top$Country == "United States of America"] = "US"
      
      df_covid_countries_top$Country[df_covid_countries_top$Country == "United States of America"] = "US"
      
      USER$df_covid_countries_top = df_covid_countries_top
      
      USER$df_covid_merge_top = df_covid_merge_top
      

      #l_countries = unique(USER$df_covid_countries$Country_Region)
    
    
      
      ### animation over time
      
      
      df_animation =  df_covid_merge_top 
      
      
      df_animation$Date = as.Date(df_animation$Date, format = "%m/%d/%y")
      
      l_min_date = min(df_animation$Date)
      
      df_animation$Days = as.integer(df_animation$Date - l_min_date + 1)
      
      USER$df_animation = df_animation
      
      print(paste("4 -", Sys.Date()))
      print(str(df_animation))
      
    
    } )
    

    
    
    
    #################################################################################
    ##                               DATA OVER TIME                                ##   
    #################################################################################
    
    f_df_covid = function (l_file_type, l_type) {
      
      l_prefix = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_'
      l_file = paste0(l_prefix,l_file_type,'_global.csv')
      
      l_covid_csv = readr::read_csv(l_file, locale = readr::locale(encoding = "latin1") )
      
      l_covid_csv = l_covid_csv %>% dplyr::select(-c('Province/State','Lat','Long'))
      
      l_covid_csv =l_covid_csv %>% dplyr::rename(Country = `Country/Region`)
      l_covid_csv = l_covid_csv %>% dplyr::group_by(Country) %>%
        dplyr::summarise_all(sum) %>%  dplyr::ungroup()
      l_covid_csv = melt(l_covid_csv, id.vars="Country")
      l_covid_csv =l_covid_csv %>% dplyr::rename_at(vars(c('variable','value')), ~ c('Date',l_type))
      
      
      return(l_covid_csv)
    }
    
    
    covid_confirmed_csv = f_df_covid('confirmed','Confirmed')
    covid_recovered_csv = f_df_covid('recovered','Recovered')
    covid_deaths_csv = f_df_covid('deaths','Deaths')
    
    covid_merge_csv = merge(covid_confirmed_csv,covid_recovered_csv)
    covid_merge_csv = merge(covid_merge_csv,covid_deaths_csv)
    covid_merge_csv$Active = covid_merge_csv$Confirmed - covid_merge_csv$Recovered - covid_merge_csv$Deaths
   
    
    
    
    
    USER$covid_merge_csv = covid_merge_csv
    print("merge")
    print(str(covid_merge_csv))
    

    
    

    
    #################################################################################
    ##                               DASHBOARD                                     ##   
    #################################################################################
    
    
    
    
    
    
    output$exploration_pa_dat1 <- shinydashboard::renderValueBox({
        
        l_rate = USER$df_covid_all$Confirmed
        
        l_label = 'Confirmed (worldwide)'
        shinydashboard::valueBox(
            formatC(l_rate, format="d", big.mark=',')
            ,paste(l_label,'')
            ,icon = icon("user")
            ,color = "teal")  
        
        
    })
    
    output$exploration_pa_dat2 <- renderValueBox({ 
        l_rate = USER$df_covid_all$Deaths
        l_label = 'Deaths (worldwide)'
        shinydashboard::valueBox(
            formatC(l_rate, format="d", big.mark=',')
            ,paste(l_label,'')
            ,icon = icon("user-times")
            ,color = "aqua")  
    })
    output$exploration_pa_dat3 <- renderValueBox({ 
        l_rate = USER$df_covid_all$Recovered
        l_label = 'Recovered (worldwide)'
        shinydashboard::valueBox(
            formatC(l_rate, format="d", big.mark=',')
            ,paste(l_label,'')
            ,icon = icon("user-shield")
            ,color = "teal")  
    })
    
    output$exploration_pa_dat4 <- renderValueBox({
        l_rate = USER$df_covid_all$Active
        l_label = 'Active (worldwide)'
        shinydashboard::valueBox(
            formatC(l_rate, format="d", big.mark=',')
            ,paste(l_label,'')
            ,icon = icon("procedures")
            ,color = "aqua")  
    })
    
    
    
    output$exploration_pa_geo <- renderLeaflet({
        
      if(is.null(USER$df_covid_countries_top)){return()}
      
      l_column_filter = input$case_type_chart1

      if (l_column_filter == 'Confirmed') { l_resize = 1.0 }
      if (l_column_filter == 'Recovered') { l_resize = 3.0 }
      if (l_column_filter == 'Deaths') { l_resize = 3.0 }
      if (l_column_filter == 'Active') { l_resize = 1.0 }
      
      pal <- colorFactor(
        palette = c('orange', 'yellow', 'red', 'green'),
        domain = c('Active', 'Confirmed', 'Deaths', 'Recovered')
      )
      
      
      l_label =   paste0("<strong>",USER$df_covid_countries_top$Country,"</strong><br>", l_column_filter ,
                         ": <strong>", formatC(USER$df_covid_countries_top[[l_column_filter]], format="d", big.mark=',') , "</strong>")
      

      
      leaflet(USER$df_covid_countries_top) %>% addTiles() %>%
            
            setView(lng = -25, lat = 40, zoom = 2) %>%
            
            addCircles(lng = ~Long_, lat = ~Lat, weight = 1, fillOpacity = 0.7,
                       radius = USER$df_covid_countries_top[[l_column_filter]]*l_resize ,  highlightOptions = highlightOptions(color = "black", weight = 2) ,
                       label = lapply(l_label , HTML) , color = ~pal(input$case_type_chart1)   )  %>%
            addMarkers(lng = ~Long_, lat = ~Lat, icon= NULL,
                       popup = l_label   )  
        
    })
    
    
    
    
    
    
    
    
    # output$exploration_active <- renderPlotly({
    #     
    #   if(is.null(USER$df_covid_merge_top)){return()}
    #   
    #     l_column_filter = input$case_type_chart2
    #   print("line")
    #   print(str(USER$df_covid_merge_top))
    #     plot_ly(USER$df_covid_merge_top, x = ~Date, y = USER$df_covid_merge_top[[l_column_filter]]) %>%
    #         add_lines(linetype = ~Country) %>% 
    #       layout(title = "",xaxis = list(title = ""  ))
    #     
    #     
    # } )
    
    output$exploration_active <- renderHighchart({
      
      if(is.null(USER$df_covid_merge_top)){return()}
      
      l_column_filter = input$case_type_chart2
      print("line")
      print(str(USER$df_covid_merge_top))
      
      l_df_ts = USER$df_covid_merge_top %>% dplyr::select(c('Country','Date',l_column_filter))
      print(str(l_df_ts))
      
      l_countries = unique(l_df_ts$Country)
      print(l_countries)
      Hchc <- highchart(type = "stock") 
      
      for (i in 1:length(l_countries)){
        l_df =  l_df_ts %>%  dplyr::filter(Country== l_countries[[i]] ) 
        l_df = dcast(l_df, Date ~ Country, value.var =l_column_filter)
        l_df = xts(l_df[, -1], order.by= as.Date(l_df$Date, format="%m/%d/%y")  )
        print(str(l_df))
        Hchc = Hchc %>%
          hc_add_series(name = l_countries[[i]], l_df , id= as.character(l_df ) )
        
      }
      
      Hchc = Hchc %>% 
        hc_legend(enabled = T, align = "right", verticalAlign = "top",layout = "vertical", x = 0, y = 100)
      
      Hchc

      
      
    } )
    
    
    
    
    output$descriptive_dt =  DT::renderDataTable({
      
      if(is.null(USER$df_covid_countries_top)){return()}
      
      sketch = htmltools::withTags(table(
        class = 'display',
        thead( 
          tr(
            th(rowspan = 2, 'Country'),
            th(rowspan = 2, 'Confirmed'),
            th(colspan = 3, 'New Cases'),
            th(rowspan = 2, 'Deaths'),
            th(rowspan = 2, 'Mortality'),
            th(rowspan = 2, 'Recovered'),
            th(rowspan = 2, 'Active'),
            th(rowspan = 2, 'GDP'),
            th(rowspan = 2, 'Population')
            
          ),
          tr(
            lapply(rep(c(USER$a_day_before, USER$last_day,'Change')), th)
          )
        )
      ))
      

        datatable(USER$df_covid_countries_top %>% dplyr::select(c('Country','Confirmed','New Cases from previous day','New Cases','Change','Deaths',
                                                                  'Mortality','Recovered','Active','GDP','Population'))  , container = sketch, rownames = FALSE,
                  options = list( pageLength = 10,scroller = TRUE,
                                  scrollX = TRUE  ,paging = TRUE,
                                  columnDefs = list(list(visible=FALSE, targets=c(9,10)))
                  ), 
                  class = 'white-space: nowrap'
        )  %>% 
            formatPercentage(c("Change","Mortality"), 1)  %>% 
            formatStyle(c(input$case_type), backgroundColor = "#5ad5fa")  %>% 
            formatStyle(c("Change"), color = styleInterval(0,c("Blue","Red")))  %>% 
            formatRound(columns=c(2,3,4,5,7,8,9), digits=0) 
                        
    },  server = TRUE) 
    
    
    
    
    
    output$exploration_ani <- renderPlotly({
        
      if(is.null(USER$df_covid_countries_top)){return()}
      
      p4 <- plot_ly(USER$df_covid_countries_top,
                    y = ~Country,
                    x = ~Active,
                    type = "bar",orientation = 'h',
                    name = "Active" , text = ~scales::comma(Active, 1), textposition = 'auto', marker = list(color = 'orange')) %>% 
        add_trace(x = ~Recovered,
                  name = "Recovered" , text = ~scales::comma(Recovered, 1), textposition = 'auto',marker = list(color = 'green')) %>% 
        add_trace(x = ~Deaths,
                  name = "Deaths" , text = ~scales::comma(Deaths, 1), textposition = 'auto', marker = list(color = 'red')) %>% 
        layout(xaxis = list(title = ""),yaxis = list(title = ""),
               barmode = "stack") 
      
      p4
        
    } )
    
    
    
}

shinyApp(ui, server)
