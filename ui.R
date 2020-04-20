library(shiny)

ui <- f7Page(title="U.S. COVID-19 Data Explorer",
             init=f7Init(skin="ios",
                         theme="light"),
             f7TabLayout(
               navbar = f7Navbar(
                 title = "U.S. Coronavirus Tracker",
                 hairline = T,
                 shadow = T,
                 left_panel = TRUE),
               panels = 
                 f7Panel(
                   title="About",
                   theme="light",
                   strong(paste0("Last Update: ",format(update_date,format="%m/%d/%y"))),
                   strong("Authors: "),
                   HTML("<a href='https://twitter.com/rdwinkelman'>Bob Winkelman, MS</a><br> Colin Waltz <br>"),
                   strong("Purpose: "),
                   HTML("In this shiny app, we present several plots demonstrating the spread of the Coronavirus (COVID-19) over time in the U.S. using the Johns Hopkins COVID-19 Dataset.
                     <br>
                     <br>
                     Additionally, we will also present some figures examining state testing data collected by the COVID tracking project."),
                   strong("How to Use: "),
                   HTML("1. Use the slider below to change data displayed according to the date<br>
                         2. Some graphs/figures provide more information if you hover specific points<br>"),
                   strong("References: "),
                   HTML("<a href='https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases'>1. Johns Hopkins COVID-19 Dataset</a>
                               <br>
                               <a href='https://covidtracking.com/'>2. COVID Tracking Project Website </a>
                               <br>
                               <a href='https://rpubs.com/rdwinkelman/covid19_us_spread_part_2'>3. Our RPubs Post w/ Code + Analyses </a>")
                 ),
               f7Tabs(id="tabs",
                      animated=T,
                      f7Tab(tabName = "Overall U.S.",
                            active=T,
                            swipeable=T,
                            hidden=F,
                            fluidRow(
                              f7Col(""),
                              f7Col(
                                h3("JHU Data: Overall U.S.", class = "center")),
                              f7Col("")
                            ),
                            f7Row(
                              f7Col(switchInput("cum_v_daily_1",
                                                "Cumulative<br>vs. Daily",
                                                value=T,
                                                offLabel="Daily",
                                                onLabel="Cumulative",
                                                labelWidth = "100px",
                                                handleWidth = "75px",
                                                size="mini",
                                                inline=T),
                                    switchInput("cases_v_deaths_1",
                                                "Cases vs.<br>Deaths",
                                                value=T,
                                                offLabel="Deaths",
                                                onLabel="Cases",
                                                labelWidth = "100px",
                                                handleWidth = "75px",
                                                size="mini",
                                                inline=T))
                            ),
                            f7Row(
                              f7Col(sliderInput("end_date",
                                                "Select Date:",
                                                min = min(jhu_state_confirmed_cases$date_fmt),
                                                max = max(jhu_state_confirmed_cases$date_fmt),
                                                value = max(jhu_state_confirmed_cases$date_fmt),
                                                timeFormat ="%m/%d/%y",
                                                width="95%")
                                    
                              )
                            ),
                            f7Row(
                              plotlyOutput("overallMapPlot")
                            ),
                            f7Row(
                              plotOutput("overallLinePlot")
                            )
                      ),
                      f7Tab(tabName = "State-Level",
                            active=F,
                            swipeable=T,
                            hidden=F,
                            fluidRow(
                              f7Col(""),
                              f7Col(
                                h3("JHU Data: State-Level", class = "center")),
                              f7Col("")
                            ),
                            f7Row(
                              f7Col(selectInput("states_selected_2",
                                                "",
                                                choices=c("All States",unique(jhu_state_confirmed_cases$state_fullname)),
                                                selected="All States",
                                                multiple=T,
                                                width="390px"))
                            ),
                            f7Row(
                              f7Col(switchInput("cum_v_daily_2",
                                                "Cumulative<br>vs. Daily",
                                                value=T,
                                                offLabel="Daily",
                                                onLabel="Cumulative",
                                                labelWidth = "70px",
                                                handleWidth = "60px",
                                                size="mini",
                                                inline=T),
                                    switchInput("raw_v_adjusted_2",
                                                "Pop.<br>Adjusted?",
                                                value=T,
                                                offLabel="Yes",
                                                onLabel="No",
                                                labelWidth = "60px",
                                                handleWidth = "30px",
                                                size="mini",
                                                inline=T),
                                    switchInput("cases_v_deaths_2",
                                                "Cases vs.<br>Deaths",
                                                value=T,
                                                offLabel="Deaths",
                                                onLabel="Cases",
                                                labelWidth = "60px",
                                                handleWidth = "35px",
                                                size="mini",
                                                inline=T)),
                            ),
                            f7Row(
                              f7Col(sliderInput("end_date_2",
                                                "Select Date:",
                                                min = as.Date("2020-02-24"),
                                                max = max(jhu_state_confirmed_cases$date_fmt),
                                                value = max(jhu_state_confirmed_cases$date_fmt),
                                                timeFormat ="%m/%d/%y",
                                                width="95%")
                                    
                              )
                            ),
                            f7Row(
                              plotlyOutput("stateLinePlotly")
                            ),
                            f7Row(
                              plotOutput("stateBarPlot")
                            )
                            # f7Row(
                            #   plotlyOutput("stateLinePlotly")
                            # ),
                            # f7Row(
                            #   plotOutput("stateBarPlot")
                            # )
                      ),
                      f7Tab(tabName="Testing Data",
                            active=F,
                            swipeable=T,
                            f7Row(
                              f7Col(""),
                              f7Col(
                                h3("Testing Data: COVID Tracking Project", class = "center")),
                              f7Col("")  
                            ),
                            f7Row(
                              f7Col(selectInput("states_selected_3",
                                                "",
                                                choices=c("All States",unique(jhu_state_confirmed_cases$state_fullname)),
                                                selected="All States",
                                                multiple=T,
                                                width="390px"))
                            ),
                            f7Row(
                              f7Col(switchInput("cum_v_daily_3",
                                                "Cumulative<br>vs. Daily",
                                                value=T,
                                                offLabel="Daily",
                                                onLabel="Cumulative",
                                                labelWidth = "100px",
                                                handleWidth = "75px",
                                                size="mini",
                                                inline=T),
                                    switchInput("raw_v_adjusted_3",
                                                "Pop.<br>Adjusted?",
                                                value=T,
                                                offLabel="Yes",
                                                onLabel="No",
                                                labelWidth = "100px",
                                                handleWidth = "75px",
                                                size="mini",
                                                inline=T))
                            ),
                            f7Row(
                              f7Col(sliderInput("end_date_3",
                                                "Select Date:",
                                                min = min(covid_tracking_states_w_pop$date_fmt),
                                                max = max(covid_tracking_states_w_pop$date_fmt),
                                                value = max(covid_tracking_states_w_pop$date_fmt),
                                                timeFormat ="%m/%d/%y",
                                                width="95%")
                              )
                            ),
                            f7Row(
                              plotlyOutput("stateTestingLinePlotly") 
                            ),
                            f7Row(
                              plotlyOutput("stateTestingPositiveVTotalScatter")
                            ),
                            f7Row(
                              plotlyOutput("stateTestingPctPositiveVTotalScatter")
                            )
                      )
               )
             )
)