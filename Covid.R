# Initial required package 
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)


#options(shiny.trace = TRUE)  

# Setting working directory
#setwd(dirname(sys.frame(1)$ofile))
options(shiny.maxRequestSize = 300*1024^2)

#List all required packages - dplyr
packages = c("shiny", "shinydashboard", "shinyjs", "DT", "sodium", "RJDBC", "ROCR", "rJava", "MASS", "readr", "shinycssloaders", "shinybusy", "reshape2","shinyalert","ggplot2","summarytools","ExPanDaR","DataExplorer", "htmlwidgets","kableExtra","plotly","psych","shinyWidgets","likert", "tidyverse","xts","tidytext","textdata","wordcloud2","countrycode","openintro","dplyr","plyr","matrixStats","stringr", "leaflet","maps","rgeos","rworldmap","maps","rworldxtra","ggalluvial","rpivotTable","highcharter")

# Install packages not yet installed
installed_packages = packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
}

# Packages loading
lapply(packages, library, character.only = TRUE) %>%  invisible()



skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
    skin <- "black"


############################ menu


sidebar = dashboardSidebar(uiOutput("sidebarpanel") , collapsed = TRUE, disable = FALSE) 


body <- dashboardBody(
    
    
    tabItem("dashboard",
            
            
            fluidRow(
                valueBoxOutput("exploration_pa_dat1", width = 3),
                valueBoxOutput("exploration_pa_dat2", width = 3),
                valueBoxOutput("exploration_pa_dat3", width = 3),
                valueBoxOutput("exploration_pa_dat4", width = 3)
            ), #fluidrow
            
            fluidRow(
                
                box(width=6, collapsible = TRUE,  title="",status="primary",solidHeader = TRUE,
                    column(width=12,leafletOutput("exploration_pa_geo", width = '100%' ,height="400" ) %>% withSpinner(color="#4262a8") )
                    
                ),# box close
                box(width=6, collapsible = TRUE,  title="",status="primary",solidHeader = TRUE,
                    column(width=12, plotlyOutput("exploration_pa_l2" , width = '100%' ,height="400") %>% withSpinner(color="#4262a8") )
                )# box close
                
            ), #fluidrow    
            
            fluidRow(
                
                box(width=6, collapsible = TRUE,  title="",status="primary",solidHeader = TRUE,
                    column(width=12, DT::dataTableOutput("descriptive_dt" ,height="500")  %>% withSpinner(color="#4262a8") )
                    
                ),# box close
                box(width=6, collapsible = TRUE,  title="",status="primary",solidHeader = TRUE,
                    column(width=12, plotlyOutput("exploration_ani" , width = '100%' ,height="500") %>% withSpinner(color="#4262a8") )
                )# box close
                
            ) #fluidrow    
            
            
    )
    
)




header <- dashboardHeader(
    title = paste0( "Covid19 Stats, Last Update:", Today())
)






ui <- dashboardPage(header, sidebar, body, skin = skin)



################################################




server <- function(input, output ) {
    
    
    USER = reactiveValues(df_covid_countries = NULL, df_covid_all = NULL, covid_deaths_csv = NULL, df_animation = NULL )
    
    f_file = function (l_date) {
        l_today =  format(l_date, "%m-%d-%Y")
        l_prefix = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/'
        l_file = paste0(l_prefix,l_today,'.csv')
        return(l_file)
    }
    f_file(Today()-1) 
    
    if (file.exists( f_file(Today()))) {
        l_last_day_file = f_file(Today()) 
        l_a_day_before_file = f_file(Today()-1) 
    } else {
        l_last_day_file = f_file(Today()-1) 
        l_a_day_before_file = f_file(Today()-2) 
    }
    
    
    covid_csv = readr::read_csv(l_last_day_file  , locale = readr::locale(encoding = "latin1") )
    covid_adb_csv = readr::read_csv(l_a_day_before_file  , locale = readr::locale(encoding = "latin1") )
    
    
    
    ##
    l_countries = list('US','Australia','Canada','France','Germany','Italy','Japan', 
                       'SouthKorea','Netherlands','Spain','Sweden','Switzerland','United Kingdom')
    
    covid_csv = covid_csv %>% dplyr::filter(Country_Region %in% l_countries  ) %>%
        dplyr::select(c('Country_Region','Lat','Long_','Confirmed','Deaths','Recovered','Active','Last_Update'))
    
    
    df_covid_countries = covid_csv %>% dplyr::group_by(Country_Region,Last_Update) %>% 
        dplyr::summarise(Confirmed=sum(Confirmed), Deaths=sum(Deaths) ,Recovered=sum(Recovered), 
                         Active=sum(Active), Lat = mean(Lat,  na.rm = TRUE),  Long_ = mean(Long_,  na.rm = TRUE)) %>%
        dplyr::select(-c('Last_Update'))
    
    covid_adb_csv = covid_adb_csv %>% dplyr::filter(Country_Region %in% l_countries  ) %>%
        dplyr::select(c('Country_Region','Lat','Long_','Confirmed','Deaths','Recovered','Active','Last_Update'))
    
    
    covid_adb_csv = covid_adb_csv %>% dplyr::group_by(Country_Region,Last_Update) %>% 
        dplyr::summarise(Confirmed=sum(Confirmed), Deaths=sum(Deaths) ,Recovered=sum(Recovered), 
                         Active=sum(Active), Lat = mean(Lat,  na.rm = TRUE),  Long_ = mean(Long_,  na.rm = TRUE)) %>%
        dplyr::select(-c('Last_Update'))
    
    covid_adb_csv$Lat = NULL
    covid_adb_csv$Long_  = NULL
    covid_adb_csv =covid_adb_csv %>% dplyr::rename(Confirmed_adb ='Confirmed' , Deaths_adb = 'Deaths',
                                                   Recovered_adb ='Recovered' , Active_adb = 'Active' )
    
    
    covid_join = df_covid_countries %>% 
        dplyr::inner_join(covid_adb_csv , by = c('Country_Region'='Country_Region') )
    
    
    # get world map
    wmap <- getMap(resolution="high")
    # get centroids
    centroids <- gCentroid(wmap, byid=TRUE)
    # get a data.frame with centroids
    df_long_lat <- as.data.frame(centroids)
    names(df_long_lat) = c("long","lat")
    df_long_lat <- tibble::rownames_to_column(df_long_lat, "COUNTRY_NAME")
    
    covid_join$Country_Region[covid_join$Country_Region == "US"] = "United States of America"
    
    covid_join <- merge(covid_join, df_long_lat, by.x = "Country_Region", by.y = "COUNTRY_NAME")
    
    covid_join$Lat = NULL
    covid_join$Long_  = NULL
    
    covid_join =covid_join %>% dplyr::rename(Lat ='lat' , Long_ = 'long' )
    
    covid_join$Mortality = covid_join$Deaths/covid_join$Confirmed
    covid_join$New_Cases = ifelse(covid_join$Confirmed - covid_join$Confirmed_adb>0,covid_join$Confirmed - covid_join$Confirmed_adb,0)
    covid_join$Change = (covid_join$Confirmed - covid_join$Confirmed_adb)/covid_join$Confirmed_adb
    
    covid_join =covid_join %>% dplyr::rename(`New Cases`= 'New_Cases' )
    
    
    ##
    
    
    df_covid_all = covid_csv %>% 
        dplyr::summarise(Confirmed=sum(Confirmed), Deaths=sum(Deaths) ,Recovered=sum(Recovered),  Active=sum(Active) ) 
    
    USER$df_covid_countries = covid_join
    USER$df_covid_all = df_covid_all
    
    
    
    covid_deaths_csv = readr::read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv', locale = readr::locale(encoding = "latin1") )
    
    covid_deaths_csv = covid_deaths_csv %>% dplyr::filter(`Country/Region` %in% l_countries  ) %>%
        dplyr::select(-c('Province/State','Lat','Long'))
    covid_deaths_csv =covid_deaths_csv %>% dplyr::rename(Country = `Country/Region`)
    covid_deaths_csv = covid_deaths_csv %>% dplyr::group_by(Country) %>%
        dplyr::summarise_all(sum) %>%
        dplyr::ungroup()
    covid_deaths_csv = melt(covid_deaths_csv, id.vars="Country")
    covid_deaths_csv =covid_deaths_csv %>% dplyr::rename(Date = 'variable', Deaths = 'value')
    
    USER$covid_deaths_csv = covid_deaths_csv
    
    
    ### animation over time
    
    covid_confirmed_csv = readr::read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv', locale = readr::locale(encoding = "latin1") )
    
    
    covid_confirmed_csv = covid_confirmed_csv %>% dplyr::filter(`Country/Region` %in% l_countries  ) %>%
        dplyr::select(-c('Province/State','Lat','Long'))
    covid_confirmed_csv =covid_confirmed_csv %>% dplyr::rename(Country = `Country/Region`)
    covid_confirmed_csv = covid_confirmed_csv %>% dplyr::group_by(Country) %>%
        dplyr::summarise_all(sum) %>%
        dplyr::ungroup()
    covid_confirmed_csv = melt(covid_confirmed_csv, id.vars="Country")
    covid_confirmed_csv =covid_confirmed_csv %>% dplyr::rename(Date = 'variable', Confirmed = 'value')
    
    
    df_animation =  covid_deaths_csv %>%
        dplyr::inner_join(covid_confirmed_csv , by = c('Country'='Country','Date'='Date'))
    
    
    df_animation$Date = as.Date(df_animation$Date, format = "%m/%d/%y")
    
    l_min_date = min(df_animation$Date)
    
    df_animation$Days = df_animation$Date - l_min_date + 1
    
    USER$df_animation = df_animation
    
    
    
    
    
    
    
    
    
    
    output$sidebarpanel = renderUI({
        
        
        dashboardSidebar(
            sidebarMenu(
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")) 
            )
            
        )
        
        
        
    })    
    
    
    
    
    
    
    
    
    
    
    
    
    ######################dashboard
    
    
    output$exploration_pa_dat1 <- shinydashboard::renderValueBox({
        
        l_rate = USER$df_covid_all$Confirmed
        
        l_label = 'Confirmed'
        shinydashboard::valueBox(
            formatC(l_rate, format="d", big.mark=',')
            ,paste(l_label,'')
            ,icon = icon("user")
            ,color = "teal")  
        
        
    })
    
    
    
    
    
    output$exploration_pa_dat2 <- renderValueBox({ 
        l_rate = USER$df_covid_all$Deaths
        l_label = 'Deaths'
        shinydashboard::valueBox(
            formatC(l_rate, format="d", big.mark=',')
            ,paste(l_label,'')
            ,icon = icon("user-times")
            ,color = "teal")  
    })
    output$exploration_pa_dat3 <- renderValueBox({ 
        l_rate = USER$df_covid_all$Recovered
        l_label = 'Recovered'
        shinydashboard::valueBox(
            formatC(l_rate, format="d", big.mark=',')
            ,paste(l_label,'')
            ,icon = icon("user-shield")
            ,color = "teal")  
    })
    
    output$exploration_pa_dat4 <- renderValueBox({
        l_rate = USER$df_covid_all$Active
        l_label = 'Active'
        shinydashboard::valueBox(
            formatC(l_rate, format="d", big.mark=',')
            ,paste(l_label,'')
            ,icon = icon("procedures")
            ,color = "teal")  
    })
    
    
    
    output$exploration_pa_geo <- renderLeaflet({
        
        
        
        
        leaflet(USER$df_covid_countries) %>% addTiles() %>%
            
            setView(lng = -25, lat = 40, zoom = 2) %>%
            addCircles(lng = ~Long_, lat = ~Lat, weight = 1, fillOpacity = 0.7,
                       radius = ~(Confirmed) ,  highlightOptions = highlightOptions(color = "black", weight = 2) )%>%
            addMarkers(lng = ~Long_, lat = ~Lat, icon= NULL,
                       popup = paste("<strong>",USER$df_covid_countries$Country_Region,"</strong><br>", 
                                     "Confirmed: <strong>", USER$df_covid_countries$Confirmed, "</strong><br>",
                                     "Deaths: <strong>", USER$df_covid_countries$Deaths, "</strong><br>",
                                     "Recovered: <strong>", USER$df_covid_countries$Recovered, "</strong><br>",
                                     "Active: <strong>", USER$df_covid_countries$Active, "</strong><br>"
                       )     )  
        
    })
    
    
    
    output$exploration_pa_l2 <- renderPlotly({
        
        
        plot_ly(USER$covid_deaths_csv, x = ~Date, y = ~Deaths) %>%
            add_lines(linetype = ~Country)
        
        
    } )
    
    output$descriptive_dt =  DT::renderDataTable({
        
        datatable(USER$df_covid_countries %>% dplyr::select(-c('Confirmed_adb','Deaths_adb','Recovered_adb','Active_adb','Long_','Lat'))  , rownames = FALSE,
                  options = list( pageLength = 10,scroller = TRUE,paging = TRUE
                  ), 
                  class = 'white-space: nowrap'
        )  %>% 
            formatPercentage(c("Change","Mortality"), 1)
        
        
    },  server = TRUE) 
    
    
    
    
    
    output$exploration_ani <- renderPlotly({
        
        base <- USER$df_animation %>%
            plot_ly(x = ~Confirmed, y = ~Deaths, size = ~Deaths, 
                    text = ~Country, hoverinfo = "text") 
        
        
        base %>%
            add_markers(
                color = ~Country, showlegend = F,
                alpha = 0.1, alpha_stroke = 0.7
            ) %>%
            add_markers(color = ~Country, frame = ~Days, ids = ~Country) %>%
            animation_opts(75, redraw = FALSE)
        
        
    } )
    
    
    
}

shinyApp(ui, server)
