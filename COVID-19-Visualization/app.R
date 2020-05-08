#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(dplyr)
library(tidyverse)
library(readr)
library(janitor)
library(gt)
library(ggplot2)
library(scales)
library(plotly)
library(reshape2)

covid_19_global <- read_csv("time_series_covid19_global.csv", na = c("undefined", "NA", ""))
covid_19_us <- read_csv("time_series_covid19_US.csv", na = c("undefined", "NA", ""))
country_id <- read_csv("UID_ISO_FIPS_LookUp_Table.csv", na = c("undefined", "NA", ""))

ui <- navbarPage(
    "COVID-19 in the World",
    
    # Other shiny themes available at https://rstudio.github.io/shinythemes/.
    
    theme = shinytheme("united"),
    
    # First Page, which is a choropleth map about COVID-19 Cumulative Confirmed Cases.
    
    tabPanel("Map",
             fluidRow(
                 
                 # Left Margin
                 
                 column(1),
                 
                 # Right Margin
                 
                 column(10,
                        
                        h2("COVID-19 Cumulative Confirmed Case", align = "center"),
                        br(),
                        
                        # A shiny widget used to select a date for the map.
                        
                        fluidRow(
                            sliderInput("dateMap",
                                    "Select a date:",
                                    min = as.Date("2020-01-22"),
                                    max = as.Date("2020-05-04"),
                                    
                                    # The default value is the lastest day available in the csv file.
                                    
                                    value = as.Date("2020-05-04"),
                                    timeFormat = "%Y-%m-%d"), 
                            
                            # Align the fluidRow in the middle.
                            
                            align="center")
                         )
                 ),
             
             fluidRow(
                 
                 # Name the plotly Output "mapPlot", set its height to 650 px.
                 
                 plotlyOutput("mapPlot", width = "auto", height = "650px"),
            
                 # Align the fluidRow in the middle.
                 
                 align = "center"),
             
             fluidRow(
                 
                 # Left Margin
                 
                 column(1),
                 
                 # Right Margin
                 
                 column(10,
                        p("*The map does not represent the author's political views.", color = "#4F4F4F", align = "center"))
             
             )),
    
    # Second Page "Global", displaying the visualization of spread of COVID-19 across countries and regions in the past three months.  
    
    tabPanel("Global",
             fluidRow(
                 
                 # Left Margin
                 
                 column(1),
                 
                 # Right Margin
                 
                 column(10,
                        
                        # Create a title for the plot: "Visualizing the Spread of COVID-19 across Countries and Regions"
                        
                        h2("Visualizing the Spread of COVID-19 across Countries and Regions", align = "center"),
                        
                        br(),
                        br(),
                        
                        # Using sidebarLayout 
                        
                        sidebarLayout(
                            
                            # Widgets on the left
                            
                            sidebarPanel(
                                
                                # Widget used to select the country or region that user wants to see its visualization.
                                
                                selectInput("country_region",
                                            
                                            # Prompt
                                            
                                            "Select a country or region:",
                                            
                                            # Choices are all the country/region names in the raw-data.
                                            
                                            choices = c('Afghanistan',
                                                        'Albania',
                                                        'Algeria',
                                                        'Andorra',
                                                        'Angola',
                                                        'Antigua and Barbuda',
                                                        'Argentina',
                                                        'Armenia',
                                                        'Australia',
                                                        'Austria',
                                                        'Azerbaijan',
                                                        'Bahamas',
                                                        'Bahrain',
                                                        'Bangladesh',
                                                        'Barbados',
                                                        'Belarus',
                                                        'Belgium',
                                                        'Belize',
                                                        'Benin',
                                                        'Bhutan',
                                                        'Bolivia',
                                                        'Bosnia and Herzegovina',
                                                        'Botswana',
                                                        'Brazil',
                                                        'Brunei',
                                                        'Bulgaria',
                                                        'Burkina Faso',
                                                        'Burma',
                                                        'Burundi',
                                                        'Cabo Verde',
                                                        'Cambodia',
                                                        'Cameroon',
                                                        'Canada',
                                                        'Central African Republic',
                                                        'Chad',
                                                        'Chile',
                                                        'China',
                                                        'Colombia',
                                                        'Comoros',
                                                        'Congo (Brazzaville)',
                                                        'Congo (Kinshasa)',
                                                        'Costa Rica',
                                                        'Cote d\'Ivoire',
                                                        'Croatia',
                                                        'Cuba',
                                                        'Cyprus',
                                                        'Czechia',
                                                        'Denmark',
                                                        'Diamond Princess',
                                                        'Djibouti',
                                                        'Dominica',
                                                        'Dominican Republic',
                                                        'Ecuador',
                                                        'Egypt',
                                                        'El Salvador',
                                                        'Equatorial Guinea',
                                                        'Eritrea',
                                                        'Estonia',
                                                        'Eswatini',
                                                        'Ethiopia',
                                                        'Fiji',
                                                        'Finland',
                                                        'France',
                                                        'Gabon',
                                                        'Gambia',
                                                        'Georgia',
                                                        'Germany',
                                                        'Ghana',
                                                        'Greece',
                                                        'Grenada',
                                                        'Guatemala',
                                                        'Guinea',
                                                        'Guinea-Bissau',
                                                        'Guyana',
                                                        'Haiti',
                                                        'Holy See',
                                                        'Honduras',
                                                        'Hungary',
                                                        'Iceland',
                                                        'India',
                                                        'Indonesia',
                                                        'Iran',
                                                        'Iraq',
                                                        'Ireland',
                                                        'Israel',
                                                        'Italy',
                                                        'Jamaica',
                                                        'Japan',
                                                        'Jordan',
                                                        'Kazakhstan',
                                                        'Kenya',
                                                        'Korea, South',
                                                        'Kosovo',
                                                        'Kuwait',
                                                        'Kyrgyzstan',
                                                        'Laos',
                                                        'Latvia',
                                                        'Lebanon',
                                                        'Liberia',
                                                        'Libya',
                                                        'Liechtenstein',
                                                        'Lithuania',
                                                        'Luxembourg',
                                                        'Madagascar',
                                                        'Malawi',
                                                        'Malaysia',
                                                        'Maldives',
                                                        'Mali',
                                                        'Malta',
                                                        'Mauritania',
                                                        'Mauritius',
                                                        'Mexico',
                                                        'Moldova',
                                                        'Monaco',
                                                        'Mongolia',
                                                        'Montenegro',
                                                        'Morocco',
                                                        'Mozambique',
                                                        'MS Zaandam',
                                                        'Namibia',
                                                        'Nepal',
                                                        'Netherlands',
                                                        'New Zealand',
                                                        'Nicaragua',
                                                        'Niger',
                                                        'Nigeria',
                                                        'North Macedonia',
                                                        'Norway',
                                                        'Oman',
                                                        'Pakistan',
                                                        'Panama',
                                                        'Papua New Guinea',
                                                        'Paraguay',
                                                        'Peru',
                                                        'Philippines',
                                                        'Poland',
                                                        'Portugal',
                                                        'Qatar',
                                                        'Romania',
                                                        'Russia',
                                                        'Rwanda',
                                                        'Saint Kitts and Nevis',
                                                        'Saint Lucia',
                                                        'Saint Vincent and the Grenadines',
                                                        'San Marino',
                                                        'Sao Tome and Principe',
                                                        'Saudi Arabia',
                                                        'Senegal',
                                                        'Serbia',
                                                        'Seychelles',
                                                        'Sierra Leone',
                                                        'Singapore',
                                                        'Slovakia',
                                                        'Slovenia',
                                                        'Somalia',
                                                        'South Africa',
                                                        'South Sudan',
                                                        'Spain',
                                                        'Sri Lanka',
                                                        'Sudan',
                                                        'Suriname',
                                                        'Sweden',
                                                        'Switzerland',
                                                        'Syria',
                                                        'Taiwan*',
                                                        'Tajikistan',
                                                        'Tanzania',
                                                        'Thailand',
                                                        'Timor-Leste',
                                                        'Togo',
                                                        'Trinidad and Tobago',
                                                        'Tunisia',
                                                        'Turkey',
                                                        'Uganda',
                                                        'Ukraine',
                                                        'United Arab Emirates',
                                                        'United Kingdom',
                                                        'Uruguay',
                                                        'US',
                                                        'Uzbekistan',
                                                        'Venezuela',
                                                        'Vietnam',
                                                        'West Bank and Gaza',
                                                        'Western Sahara',
                                                        'Yemen',
                                                        'Zambia',
                                                        'Zimbabwe'),
                                            
                                            # Set default to "China" 
                                            
                                            selected = "China"),
                                
                                # Widget used to select a data range for plotting.
                                
                                sliderInput("dateRange",
                                            "Select a date range:",
                                            min = as.Date("2020-01-22"),
                                            max = as.Date("2020-05-04"),
                                            
                                            # Default set to the longest range.
                                            
                                            value = c(as.Date("2020-01-22"), as.Date("2020-05-04")),
                                            timeFormat = "%Y-%m-%d"),
                                
                                # Widget used to select the scale on Y-axis of the plot.
                                
                                radioButtons("global_axis",
                                             "Choose a scale on Y-axis:",
                                             
                                             # Two scales based on Log 10 or arithmetic.
                                             
                                             choices = c("Arithmetic","Logarithmic"), 
                                             
                                             # Set default to "Arithmetic".
                                             
                                             selected = "Arithmetic"),
                                
                                # Details for the plot.
                                
                                p("Red line represents confirmed cases, blue line represents recovered cases, and green line represents deaths.")),
                            
                            # Plot on the right side.
                            # Plotly Output named "globalPlot".
                            
                            mainPanel(plotlyOutput("globalPlot"))
                            )
                        )
                 )
             ),
    
    # Third Page "Domestic", displaying the visualization of spread of COVID-19 across states in the past three months.  
    
    tabPanel("Domestic",
             fluidRow(
                 
                 # Left Margin
                 
                 column(1),
                 
                 # Right Margin
                 
                 column(10,
                        
                        # Create a title for the plot: "Visualizing the Spread of COVID-19 across States".
                        
                        h2("Visualizing the Spread of COVID-19 across States", align = "center"),
                        
                        br(),
                        br(),
                        
                        # Using sidebarLayout 
                        
                        sidebarLayout(
                            
                            # Widgets on the left
                            
                            sidebarPanel(
                                
                                # Widget used to select a state that user wants to see its visualization.
                                
                                selectInput("state",
                                            
                                            # Prompt
                                            
                                            "Select a state:",
                                            
                                            # Choices are all states/provinces in the time_series_covide19_US.csv.
                                            
                                            choices = c('American Samoa',
                                                        'Guam',
                                                        'Northern Mariana Islands',
                                                        'Puerto Rico',
                                                        'Virgin Islands',
                                                        'Alabama',
                                                        'Alaska',
                                                        'Arizona',
                                                        'Arkansas',
                                                        'California',
                                                        'Colorado',
                                                        'Connecticut',
                                                        'Delaware',
                                                        'District of Columbia',
                                                        'Florida',
                                                        'Georgia',
                                                        'Hawaii',
                                                        'Idaho',
                                                        'Illinois',
                                                        'Indiana',
                                                        'Iowa',
                                                        'Kansas',
                                                        'Kentucky',
                                                        'Louisiana',
                                                        'Maine',
                                                        'Maryland',
                                                        'Massachusetts',
                                                        'Michigan',
                                                        'Minnesota',
                                                        'Mississippi',
                                                        'Missouri',
                                                        'Montana',
                                                        'Nebraska',
                                                        'Nevada',
                                                        'New Hampshire',
                                                        'New Jersey',
                                                        'New Mexico',
                                                        'New York',
                                                        'North Carolina',
                                                        'North Dakota',
                                                        'Ohio',
                                                        'Oklahoma',
                                                        'Oregon',
                                                        'Pennsylvania',
                                                        'Rhode Island',
                                                        'South Carolina',
                                                        'South Dakota',
                                                        'Tennessee',
                                                        'Texas',
                                                        'Utah',
                                                        'Vermont',
                                                        'Virginia',
                                                        'Washington',
                                                        'West Virginia',
                                                        'Wisconsin',
                                                        'Wyoming',
                                                        'Diamond Princess',
                                                        'Grand Princess'),
                                            
                                            # Set default to Massachusetts.
                                            
                                            selected = "Massachusetts"
                                            ),
                                
                                # Widget used to select a data range for plotting.

                                sliderInput("dateRangeState",
                                            "Select a date range:",
                                            min = as.Date("2020-01-22"),
                                            max = as.Date("2020-05-04"),
                                            
                                            # Default set to the longest range.
                                            
                                            value = c(as.Date("2020-01-22"), as.Date("2020-05-04")),
                                            timeFormat = "%Y-%m-%d"),
                                
                                # Widget used to select the scale on Y-axis of the plot.
                                
                                radioButtons("us_axis",
                                             "Choose a scale on Y-axis:",
                                             
                                             # Two scales based on Log 10 or arithmetic.
                                             
                                             choices = c("Arithmetic","Logarithmic"), 
                                             
                                             # Set default to "Arithmetic".
                                             
                                             selected = "Arithmetic"),
                                
                                # Details for the plot.
                                
                                p("Red line represents confirmed cases and blue line represents deaths.")),
                            
                            # Plot on the right side.
                            # Plotly Output named "statePlot".
                            
                            mainPanel(plotlyOutput("statePlot"))
                        )
                 )
             )
    ),
    
    tabPanel("Mortality Rate",
             fluidRow(
                 
                 # Left Margin
                 
                 column(1),
                 
                 # Right Margin
                 
                 column(10,
                        
                        # Create a title for the plot: "Visualizing the Spread of COVID-19 across States".
                        
                        h2("Mortality Rate of COVID-19, by Country/Region", align = "center"),
                        
                        br(),
                        br(),
                        
                        # Using sidebarLayout 
                        
                        sidebarLayout(
                            
                            # Widgets on the left
                            
                            sidebarPanel(
                                
                                # Widget used to select a data range for plotting.
                                
                                sliderInput("dateRate",
                                            "Select a date:",
                                            min = as.Date("2020-01-22"),
                                            max = as.Date("2020-05-04"),
                                            
                                            # The default value is the lastest day available in the csv file.
                                            
                                            value = as.Date("2020-05-04"),
                                            timeFormat = "%Y-%m-%d"), 
                                
                                numericInput("caseRate",
                                            "Enter the lowest confirmed cases for a country to be included in the plot:",
                                            max = 1100000,
                                            
                                            # The default value is the lastest day available in the csv file.
                                            
                                            value = 1000)),
                                
                            # Plot on the right side.
                            # Plotly Output named "ratePlot".
                            
                            mainPanel(plotlyOutput("ratePlot"))
                        )
                 )
             )
    ),
    
    # Fifth Page "Discussion".
    
    tabPanel("Discussion",
             fluidRow(
                 
                 # Left Margin
                 
                 column(1),
                 
                 # Right Margin
                 
                 column(10,
                        h3("Map"),
                        p("The choropleth map on the homepage provides a visualization of the spread of COVID-19 from January 22th to May 4th."),
                        p("The default date is set to May 4th."),
                        h3("Global & Domestic Case Summary"),
                        p("These two webpages 'Global' & 'Domestic' provides a visualization of the spread of COVID-19 inside a country/region or a state."),
                        h3("Morality"),
                        p("On the 'Morality Rate' webpage is the visualization of the morality rate of COVID-19 of countries/regions in a single day between January 22 to May 4."),
                        p("The default date is set to May 4th.")
                        ))),
    
    # Sixth Page "About".
    
    tabPanel("About", 
             fluidRow(
                 
                 # Left Margin
                 
                 column(1),
                 
                 # Right Margin
                 
                 column(10,
                        titlePanel("About"),
                        h3("Project Background and Motivations"),
                        p("Hello, this is my final project for Spring 2020 GOV 1005 about the visualization of COVID-19 in the world."),
                        p("COVID-19 has changed the world. It left me an unimaginable semester. "),
                        h3("Data"),
                        h4(a(href = "https://github.com/CSSEGISandData/COVID-19",
                             "2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE")),
                        h4(a(href = "https://www.iso.org/iso-3166-country-codes.html",
                             "ISO 3166 — Country Codes - ISO")),
                        h3("About Me"),
                        p("My name is Yanghe Liu, an international student from China, and I study everything(or nothing). 
             You can reach me at yanghe_liu@college.harvard.edu.")))))


server <- function(input, output) {

    output$mapPlot <- renderPlotly({
    
        country_id <- country_id %>%
            
            # Select the useful info in the dataframe country_id.
            
            select(`iso3`, `Country_Region`) %>%
            
            # Group by country.
            
            group_by(Country_Region) %>%
            
            # Eliminate repeated rows.
            
            unique()
        
        # Add country id info to the covid_19_global dataframe.
        
        covid_19_global <- inner_join(country_id, covid_19_global, by = c("Country_Region" = "Country/Region")) %>%
            filter(Type == "Confirmed") %>%
            select(-`Province/State`, -`Type`) %>%
            group_by(`Country_Region`, `iso3`) %>%
            summarise_all(sum)
        
        # Set width and color of the line in the map.
        
        l <- list(color = toRGB("grey"), width = 1)
        
        # specify map projection
        
        g <- list(
            showframe = FALSE,
            showcoastlines = F,
            
            # Other projections: 'equirectangular', 'mercator', 'orthographic', 'natural earth', 'kavrayskiy7', 'miller', 'robinson', 'eckert4', 'azimuthal equal area', 'azimuthal equidistant', 'conic equal area', 'conic conformal', 'conic equidistant', 'gnomonic', 'stereographic', 'mollweide', 'hammer', 'transverse mercator', 'albers usa', 'winkel tripel', 'aitoff' and 'sinusoidal'.
            
            projection = list(type = "natural earth")
        )
        
        # Create fig as an object of plot_geo()
        
        fig <- plot_geo()
        
        # Save input$dateMap to date as a type char, waiting to be used as a column name.
        
        date <- as.character(input$dateMap)
            
        fig <- fig %>% add_trace(
            
            # Plot type
            
            type = "choropleth",
            
            # Use [] because date is a variable
            
            z = covid_19_global[[date]], 
            
            # Other colorscales https://plotly.com/python/colorscales/.
            
            autocolorscale = T,
            
            text = covid_19_global$`Country_Region`, 
            locations = covid_19_global$`iso3`, 
            
            # Hide scale
            
            showscale = T,
            
            marker = list(line = l)
            
        )
        
        fig <- fig %>% layout(geo = g) %>%
            
            # https://plot.ly/r/reference/#scatter-marker-colorbar
            
            colorbar(
                # length of the colorbar
                len = 0.8,
                
                # title of the colorbar
                title = "Cases",
                
                # thickness of the colorbar
                
                thickness = 20,
                
                # format of ticks of the colorbar
                
                tickformat = "number")
        
        fig
        
    })
    
    output$globalPlot <- renderPlotly({
        
        # if user select "Logarithmic" scale
        
        if (input$global_axis == "Logarithmic") {
            case_confirmed <- covid_19_global %>%
                filter(Type == "Confirmed") %>%
                filter(`Country/Region` == input$country_region) %>%
                select(-`Province/State`, -`Country/Region`, -`Type`) %>%
                rbind(., total = colSums(.)) %>%
                tail(1) %>%
                melt(variable.name = "Date",
                     value.name = "Case")
            
            case_confirmed$Type <- "Confirmed"
            
            case_deaths <- covid_19_global %>%
                filter(Type == "Deaths") %>%
                filter(`Country/Region` == input$country_region) %>%
                select(-`Province/State`, -`Country/Region`, -`Type`) %>%
                rbind(., total = colSums(.)) %>%
                tail(1) %>%
                melt(variable.name = "Date",
                     value.name = "Case")
            
            case_deaths$Type <- "Deaths"
            
            case_recovered <- covid_19_global %>%
                filter(Type == "Recovered") %>%
                filter(`Country/Region` == input$country_region) %>%
                select(-`Province/State`, -`Country/Region`, -`Type`) %>%
                rbind(., total = colSums(.)) %>%
                tail(1) %>%
                melt(variable.name = "Date",
                     value.name = "Case")
            
            case_recovered$Type <- "Recovered"
            
            # rbind three tables together.
            
            case <- rbind(case_confirmed, case_deaths)
            
            case <- rbind(case, case_recovered)
            
            # Cast case$Date as date format
            
            case$Date <- as.Date(case$Date)
            
            globalPlot <- case %>% 
                ggplot(aes(x = Date)) +
                geom_line(aes(y = Case, color = Type)) +
                geom_point(aes(y = Case, color = Type)) +
                ylab("Case") +
                xlab("Date") +
                
                # Change Y-axis to Log 10 scale.
                
                scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = scales::comma_format(accuracy = 1)) +
                scale_x_date(date_breaks = "2 weeks", limits = as.Date(input$dateRange))+
                ggtitle(paste("COVID-19 Cases in", input$country_region)) +
                theme(axis.text.x = element_text(angle = 0, hjust = 1))
            
            ggplotly(globalPlot)
            
        }
        
        # if user select "Arithmetic" scale
        
        else {
            case_confirmed <- covid_19_global %>%
                filter(Type == "Confirmed") %>%
                filter(`Country/Region` == input$country_region) %>%
                select(-`Province/State`, -`Country/Region`, -`Type`) %>%
                rbind(., total = colSums(.)) %>%
                tail(1) %>%
                melt(variable.name = "Date",
                     value.name = "Case")
            
            case_confirmed$Type <- "Confirmed"
            
            case_deaths <- covid_19_global %>%
                filter(Type == "Deaths") %>%
                filter(`Country/Region` == input$country_region) %>%
                select(-`Province/State`, -`Country/Region`, -`Type`) %>%
                rbind(., total = colSums(.)) %>%
                tail(1) %>%
                melt(variable.name = "Date",
                     value.name = "Case")
            
            case_deaths$Type <- "Deaths"
            
            case_recovered <- covid_19_global %>%
                filter(Type == "Recovered") %>%
                filter(`Country/Region` == input$country_region) %>%
                select(-`Province/State`, -`Country/Region`, -`Type`) %>%
                rbind(., total = colSums(.)) %>%
                tail(1) %>%
                melt(variable.name = "Date",
                     value.name = "Case")
            
            case_recovered$Type <- "Recovered"
            
            # rbind three tables together.
            
            case <- rbind(case_confirmed, case_deaths)
            
            case <- rbind(case, case_recovered)
            
            # Cast case$Date as date format.
            
            case$Date <- as.Date(case$Date)
            
            globalPlot <- case %>% 
                ggplot(aes(x = Date)) +
                geom_line(aes(y = Case, color = Type)) +
                geom_point(aes(y = Case, color = Type)) +
                ylab("Case") +
                xlab("Date") +
                scale_x_date(date_breaks = "2 weeks", limits = as.Date(input$dateRange)) +
                ggtitle(paste("COVID-19 Cases in", input$country_region)) +
                theme(axis.text.x = element_text(angle = 0, hjust = 1))
            
            ggplotly(globalPlot)
        }
        
    })
    
    output$statePlot <- renderPlotly ({
        
        # if user select "Logarithmic" scale
        
        if (input$us_axis == "Logarithmic") {
            state_confirmed_case <- covid_19_us %>%
                filter(Type == "Confirmed") %>%
                filter(`Province_State` == input$state) %>%
                select(-`Province_State`, -`Type`) %>%
                rbind(., total = colSums(.)) %>%
                tail(1) %>%
                melt(variable.name = "Date",
                     value.name = "Case")
            
            state_confirmed_case$Type <- "Confirmed"
            
            state_deaths_case <- covid_19_us %>%
                filter(Type == "Deaths") %>%
                filter(`Province_State` == input$state) %>%
                select(-`Province_State`, -`Type`) %>%
                rbind(., total = colSums(.)) %>%
                tail(1) %>%
                melt(variable.name = "Date",
                     value.name = "Case")
            
            state_deaths_case$Type <- "Deaths"
            
            state_case <- rbind(state_confirmed_case, state_deaths_case)
            
            # Cast case$Date as date format.
            
            state_case$Date <- as.Date(state_case$Date)
            
            statePlot <- state_case %>% 
                ggplot(aes(x = Date)) +
                geom_line(aes(y = Case, color = Type)) +
                geom_point(aes(y = Case, color = Type)) +
                ylab("Case") +
                xlab("Date") +
                
                # Change Y-axis to Log 10 scale.
                
                scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = scales::comma_format(accuracy = 1)) +
                scale_x_date(date_breaks = "2 weeks", limits = as.Date(input$dateRangeState)) +
                ggtitle(paste("COVID-19 Cases in",input$state)) +
                theme(axis.text.x = element_text(angle = 0, hjust = 1))
            
            ggplotly(statePlot)
            
        }
        
        # if user select "Arithmetic" scale
        
        else {
            state_confirmed_case <- covid_19_us %>%
                filter(Type == "Confirmed") %>%
                filter(`Province_State` == input$state) %>%
                select(-`Province_State`,-`Type`) %>%
                rbind(., total = colSums(.)) %>%
                tail(1) %>%
                melt(variable.name = "Date",
                     value.name = "Case")
            
            state_confirmed_case$Type <- "Confirmed"
            
            state_deaths_case <- covid_19_us %>%
                filter(Type == "Deaths") %>%
                filter(`Province_State` == input$state) %>%
                select(-`Province_State`, -`Type`) %>%
                rbind(., total = colSums(.)) %>%
                tail(1) %>%
                melt(variable.name = "Date",
                     value.name = "Case")
            
            state_deaths_case$Type <- "Deaths"
            
            state_case <- rbind(state_confirmed_case, state_deaths_case)
            
            # Cast case$Date as date format.
            
            state_case$Date <- as.Date(state_case$Date)
            
            statePlot <- state_case %>% 
                ggplot(aes(x = Date)) +
                geom_line(aes(y = Case, color = Type)) +
                geom_point(aes(y = Case, color = Type)) +
                ylab("Case") +
                xlab("Date") +
                scale_x_date(date_breaks = "2 weeks", limits = as.Date(input$dateRangeState)) +
                ggtitle(paste("COVID-19 Cases in", input$state)) +
                theme(axis.text.x = element_text(angle = 0, hjust = 1))
            
            ggplotly(statePlot)
            
        }
    })
    
    output$ratePlot <- renderPlotly({
        
        date <- as.character(input$dateRate)
        
        global_table_confirmed <- covid_19_global %>% 
            filter(Type == "Confirmed") %>%
            group_by(`Country/Region`) %>%
            summarise(x = sum(get(date)))
        
        global_table_deaths <- covid_19_global %>% 
            filter(Type == "Deaths") %>%
            group_by(`Country/Region`) %>%
            summarise(y = sum(get(date)))
        
        global_death_rate <- global_table_confirmed %>%
            inner_join(global_table_deaths, by=c("Country/Region")) %>%
            rename("confirm"=`x`,"death"=`y`) %>%
            filter(`confirm` > input$caseRate & `death` > 0) %>%
            mutate(rate = `death`/`confirm`) %>%
            arrange(desc(rate))
        
        table_global_death_rate <- global_death_rate %>%
            ggplot(aes(x=reorder(`Country/Region`,-`rate`),y=rate)) + 
            geom_col() +
            scale_y_continuous(labels = percent) +
            ylab("Morality Rate") +
            xlab("Country/Region") +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 90))
        
        ggplotly(table_global_death_rate)
        
    })
}

# Run the application
shinyApp(ui = ui, server = server)
