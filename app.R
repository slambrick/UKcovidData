#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)      # Web apps
library(ukcovid19)  # COVID-19 data
library(magrittr)   # the %<>% operator
library(tidyverse)  # The tidyverse
library(plotly)     # Interactive plots
library(zoo)        # Rolling averages

#----------------------------- Constants --------------------------------------#

# TODO: have the data stored as a text file and only load data if we haven't
# loaded any today

# Get Covid data by nation
query_filters <- c(
    'areaType=nation'
)

cases_and_deaths = list(
    date = "date",
    areaName = "areaName",
    areaCode = "areaCode",
    casesSpecimen = "newCasesBySpecimenDate",
    deathsDeath = "newDeathsByDeathDate",
    casesPublish = "newCasesByPublishDate",
    deathsPublish = "newDeaths28DaysByPublishDate"
)

cov_data <- tibble(get_data(
    filters = query_filters, 
    structure = cases_and_deaths
))

# Population of the home nations for normalisation, 2019 estimate from Wikipedia
england_population <- 56286961/100000
wales_population <- 3153000/100000
scotland_population <- 5313600/100000
NI_popularion <- 1893700/100000
all_population <- england_population + wales_population + scotland_population + NI_popularion
pops1 <- list("England" = england_population,
              "Wales" = wales_population,
              "Scotland" = scotland_population,
              "Northern Ireland" = NI_popularion,
              "All" = all_population)


# Get Covid data by region
query_filters2 <- c(
    'areaType=region'
)

cov_data2 <- tibble(get_data(
    filters = query_filters2,
    structure = cases_and_deaths
))

# Population of the English regions 
pops2 <- list("South East" = 9180135/100000,
              "London" = 8961989/100000,
              "North West" = 7341196/100000,
              "East of England" = 6236072/100000,
              "West Midlands" = 5934037/100000,
              "South West" = 5624696/100000,
              "Yorkshire and The Humber" = 5502967/100000,
              "East Midlands" = 4835928/100000,
              "North East" = 2669941/100000)

pops <- c(pops1, pops2)
cov_data_all <- bind_rows(cov_data, cov_data2) #%>% 
#    mutate(newDeaths28DaysByPublishDate = replace_na(newDeaths28DaysByPublishDate))
region_list <- append(unique(cov_data_all$areaName), "All")

#----------------------------- Functions --------------------------------------#

normalise_case <- function(cases, country) cases/pops[[country]]
normalise_cases <- Vectorize(normalise_case)

get_nation <- function(df, nation) (df %>% filter(areaName == nation))$my_data

get_total <- function(df) {
    all <- get_nation(df, "England") + get_nation(df, "Wales") + get_nation(df, "Scotland") + 
        get_nation(df, "Northern Ireland")
    df_all <- tibble(date = (df %>% filter(areaName == "England"))$date,
                     my_data = all,
                     areaName = "All")
    bind_rows(df, df_all)
}

get_rolling_av <- function(df, nations, k) {
    for (c in nations)
        df[paste(c, "_av", sep = "")] <- rollmean(df[c], k = k, na.pad=T)
    df
}

month_to_dateStr <- function(n) {
    ds <- if_else(n < 12, paste("2020-", sprintf("%02d", n+1), "-01", sep=""), 
        paste("2021-", sprintf("%02d", n+1-12), "-01", sep=""))
}

plots_generate <- function(df, input, leg_make = TRUE){
    if (input$show_raw == "Yes") {
        p <- df %>% plot_ly(x = ~date, y = ~raw, color = ~areaName, 
                            showlegend = FALSE, type="scatter",mode="lines+markers",
                            alpha = 0.5)
        p %<>% add_trace(x = ~date, y = ~av, color = ~areaName, type="scatter",
                         mode="line", showlegend = leg_make, alpha = 1)
    } else {
        p <- df %>% plot_ly(x = ~date, y = ~av, color = ~areaName, type="scatter",
                            mode="line", showlegend = leg_make)
    }
    p
}

data_wrangle <- function(df, input) {
    make_raw <- function(s) paste("raw_", s, sep = "")
    df[is.na(df)] = 0
    
    if ("All" %in% input$nation)
        df %<>% get_total()
    if (input$normalise == "Yes")
        df %<>% mutate(my_data = normalise_cases(my_data, areaName))
    
    # Get the specified regions and take rolling averages, wide format is needed
    # for the rolling averages, then convert back to long
    df %<>% filter(areaName %in% input$nation) %>% spread(areaName, my_data)
    for (r in input$nation)
        df[[paste("av_", r, sep="")]] <- rollmean(df[[r]], k = input$roll_av_n, na.pad = T)
    df %<>% rename_with(make_raw, input$nation) %>%
        pivot_longer(cols = c(paste("av_", input$nation, sep=""), 
                              paste("raw_", input$nation, sep="")),
                     names_to = c(".value", "areaName"),
                     names_sep = "_") %>% 
        filter(as.Date(date) < as.Date(month_to_dateStr(input$date_range[2])) & 
                   as.Date(date) > as.Date(month_to_dateStr(input$date_range[1])))
}

#----------------------------- Application ------------------------------------#

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("UK COVID-19 data"),

    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("nation",
                               "Which regions?",
                               choices = region_list),
            selectInput("data_series",
                        "Which data series to plot?",
                        choices = c("Cases", "Deaths", "Both")),
            radioButtons("date_actual",
                         "By publish date or by death/specimen date?",
                         choices = c("Specimen/death date", "Publish date"),
                         inline = TRUE),
            radioButtons("normalise",
                         "Normalise data per capita?",
                         choices = c("Yes", "No"),
                         inline = TRUE),
            radioButtons("show_raw",
                         "Show raw data?",
                         choices = c("Yes", "No"),
                         inline = TRUE),
            sliderInput("roll_av_n",
                        "How many days to include in rolling average?",
                        min = 2,
                        max = 30,
                        value = 7),
            sliderInput("date_range",
                        "Range of dates to plot (months)",
                        min = 0,
                        max = 12,
                        value = c(0, 12))
        ),

        mainPanel(
            plotlyOutput("covidPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$covidPlot <- renderPlotly({
        if (!is.null(input$nation)) {
            if (input$date_actual != "Publish date")
                cov_data_use <- cov_data_all %>% mutate(cases = casesSpecimen, deaths = deathsDeath)
            else
                cov_data_use <- cov_data_all %>% mutate(cases = casesPublish, deaths = deathsPublish)
            if (input$data_series == "Cases" || input$data_series == "Both") {
                # Case plot
                df <- cov_data_use %>% 
                    transmute(date = date, my_data = cases, areaName = areaName)
                df %<>% data_wrangle(input)
                p1 <- df %>% plots_generate(input)
                if (input$normalise == "Yes")
                    p1 %<>% layout(xaxis = list(title = "Date"), yaxis = list(title = "New cases\nper 100,000"))
                else
                    p1 %<>% layout(xaxis = list(title = "Date"), yaxis = list(title = "New cases"))
            }
            
            if (input$data_series == "Deaths" || input$data_series == "Both") {
                # Deaths plot
                df <- cov_data_use %>% 
                    transmute(date = date, my_data = deaths, areaName = areaName)
                df %<>% data_wrangle(input)
                p2 <- df %>% plots_generate(input, leg_make = FALSE)
                if (input$normalise == "Yes")
                    p2 %<>% layout(xaxis = list(title = "Date"), yaxis = list(title = "New deaths\nper 100,000"))
                else
                    p2 %<>% layout(xaxis = list(title = "Date"), yaxis = list(title = "New deaths"))
            }
            
            # Plot layout
            if (input$data_series == "Both") {
                p <- subplot(p1, p2, nrows = 2, shareX=TRUE, titleY=TRUE)
                p %<>% layout(height= 800)
            } else if (input$data_series == "Cases")
                p <- p1
            else
                p <- p2
            p
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
