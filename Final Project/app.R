#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tm)) install.packages("tm", repos = "http://cran.us.r-project.org")
if(!require(tidytext)) install.packages("tidytext", repos = "http://cran.us.r-project.org")
if(!require(SnowballC)) install.packages("SnowballC", repos = "http://cran.us.r-project.org")
if(!require(wordcloud)) install.packages("wordcloud", repos = "http://cran.us.r-project.org")
if(!require(wordcloud2)) install.packages("wordcloud2", repos = "http://cran.us.r-project.org")
if(!require(stopwords)) install.packages("stopwords", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(maptools)) install.packages("maptools", repos = "http://cran.us.r-project.org")
if(!require(viridis)) install.packages("viridis", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(ggmap)) install.packages("ggmap", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(fpp2)) install.packages("fpp2", repos = "http://cran.us.r-project.org")
if(!require(treemap)) install.packages("treemap", repos = "http://cran.us.r-project.org")
if(!require(gridBase)) install.packages("gridBase", repos = "http://cran.us.r-project.org")
if(!require(d3treeR)) install.packages("d3treeR", repos = "http://cran.us.r-project.org")
if(!require(highcharter)) install.packages("highcharter", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(shinyjs)) install.packages("shinyjs", repos = "http://cran.us.r-project.org")
if(!require(streamgraph)) devtools::install_github("hrbrmstr/streamgraph")

# library(shiny)
# library(shinythemes)
# library(shinydashboard)
# library(tidyverse)
# library(tm)
# library(tidytext)
# library(SnowballC)
# library(wordcloud)
# library(wordcloud2)
# library(stopwords)
# library(RColorBrewer)
# library(leaflet)
# library(viridis)
# library(plotly)
# library(ggmap)
# library(ggthemes)
# library(data.table)
# library(fpp2)
# library(treemap)
# library(gridBase)
# library(d3treeR)
# library(highcharter)
# library(caTools)

 ########################## IMPORT DATA ##############################
gtd1 <- fread("https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/gtd1.csv", stringsAsFactors = F)
gtd2 <- fread("https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/gtd2.csv", stringsAsFactors = F)
gtd3 <- fread("https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/gtd3.csv", stringsAsFactors = F)
gtd4 <- fread("https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/gtd4.csv", stringsAsFactors = F)
gtd5a <- fread("https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/gtd5a.csv", stringsAsFactors = F)
gtd5b <- fread("https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/gtd5b.csv", stringsAsFactors = F)
gtd6a <- fread("https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/gtd6a.csv", stringsAsFactors = F)
gtd6b <- fread("https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/gtd6b.csv", stringsAsFactors = F)
gtd7 <- fread("https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/gtd7.csv", stringsAsFactors = F)
gtd8 <- fread("https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/gtd8.csv", stringsAsFactors = F)
gtd9 <- fread("https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/gtd9.csv", stringsAsFactors = F)
gtd10 <- fread("https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/gtd10.csv", stringsAsFactors = F)

gtd <- rbind(gtd1, gtd2, gtd3, gtd4, gtd5a, gtd5b, gtd6a, gtd6a, gtd7, gtd8, gtd9, gtd10)


############################################### GLOBAL #########################################################

##Terrorist Attacks
year_tbl <- data.frame(table(gtd$iyear))
colnames(year_tbl) <- c("Year", "Count")

ttrend <- gtd %>% select(iyear, region_txt) %>% table()

f <- ts(ttrend, frequency = 1, start = 1970)
autoplot(f) + ggtitle("Trend in Terrorist Attacks")

###MAPS

country_terror <- gtd %>% dplyr:: select(iyear, country_txt, provstate, latitude, longitude) %>% 
  group_by(iyear) %>%  
  count(country_txt, provstate, latitude, longitude) %>% ungroup() 
country_terror <- country_terror[complete.cases(country_terror),]

country_deaths <- gtd %>% dplyr:: select(iyear, country_txt, provstate, nkill, latitude, longitude) %>% 
  group_by(iyear) %>%  
  count(country_txt, nkill, provstate, latitude, longitude) %>% ungroup() 
country_deaths <- country_deaths[complete.cases(country_deaths),]

country_gangs <- gtd %>% dplyr:: select(iyear, country_txt, provstate, gname, latitude, longitude) %>% 
  group_by(iyear) %>%  
  count(country_txt, gname, provstate, gname, latitude, longitude) %>% ungroup()
country_gangs <- country_gangs[complete.cases(country_gangs),]

## Fatalities
kill_region <- gtd %>% select(iyear, region_txt, nkill) %>%
  group_by(iyear, region_txt) %>%  
  summarise(N_kills = sum(nkill)) %>% ungroup()
kill_region <- kill_region[complete.cases(kill_region),] 

## Types of Attackas
attack_tbl <- data.frame(table(gtd$attacktype1_txt))
colnames(attack_tbl) <- c("Type", "Count")

attack_region <- gtd %>% 
  select(attacktype1_txt, region_txt) %>% 
  table() %>% 
  data.frame()

attacks_suicide_success <- gtd %>% 
  select(iyear, attacktype1_txt, suicide, success)%>%  
  group_by(iyear, attacktype1_txt)

##  Targets
targeted <- gtd %>% select(targtype1_txt) %>% table() %>% data.frame()
colnames(targeted) <- c("Target", "Count")

sub_targeted <- gtd %>% select(targsubtype1_txt) %>% table() %>% data.frame()
colnames(sub_targeted) <- c("Target", "Count")
levels(sub_targeted$Target)[1] <- "Unknown"

##Weapons

weapons <- gtd %>% select(weaptype1_txt) %>% table() %>% data.frame() 
colnames(weapons) <- c("Weapon", "Count")
levels(weapons$Weapon)[12] <- "Vehicle"

sub_weapons <- gtd %>% select(weapsubtype1_txt) %>% table() %>% data.frame() 
colnames(sub_weapons) <- c("Weapon", "Count")
levels(sub_weapons$Weapon)[1] <- "Unknown"

group_weapons <- gtd %>% 
  select(gname, weapsubtype1_txt) %>% 
  group_by(gname) %>% table() %>% 
  data.frame()
  
### Groups
terrorists_groups <- gtd %>% select(gname) %>% table() %>% data.frame()
colnames(terrorists_groups) <- c("Terrorist", "Count")
terrorists_groups <- terrorists_groups[order(-terrorists_groups$Count),]
terrorists_groups <- head(terrorists_groups[-1,], 10)

terrorists_success <- gtd %>% 
  select(gname, success) %>% 
  table() %>% 
  data.frame() 
 
############################################ UI DESIGN ##########################################################

ui <- fluidPage(
  navbarPage("Global Terrorism Database", theme = shinytheme("spacelab"),
             
      ####################### TAB 1: EXPLORATION ############################
           tabPanel("Exploration", icon = icon("chart-bar"),
                    navlistPanel("Explorer",
                                 tabPanel("Terrorist Activities", icon = icon("chevron-right"),
                                        fluidRow(
                                          h3("Overview"),
                                          plotlyOutput("plot1", height = 600, width = 1000)),
                                        br(), 
                                        br(),
                                        br(),
                                         fluidRow(
                                           h3("Trends"),
                                           selectInput("region", h3("Select to view individual regions trends"), 
                                                        choices = c("All", unique(as.character(gtd$region_txt)))),
                                            plotlyOutput("plot2", height = 600, width = 1000))
                                 ),
                                 br(),
                                 tabPanel("Fatalities", icon = icon("chevron-right"),
                                          fluidRow(
                                            h3("In this section we look at the number of total confirmed fatalities for attacks that happened. The counts include all victims and attackers who died as a direct result of the incidents."),
                                            br(),
                                            h4("Streamgraph"),
                                            streamgraphOutput("stream", height = 600, width = 1000),
                                            br(),
                                            br(),
                                            h4("Trend"),
                                            selectInput("region2", h5("Select a Region"), choices = unique(as.character(gtd$region_txt))),
                                            plotlyOutput("plot3", height = 600, width = 1000)
                                            ),
                                          
                                          br(),
                                          br(),
                                          h4("Killings by Region"),
                                          highchartOutput("plot4", height = 800, width = 800),
                                          br(),
                                          br(),
                                          br(),
                                          h4("Killings by Country"),
                                          highchartOutput("plot5", height = 800, width = 800)
                                          ),
                                 br(),
                                 tabPanel("Type of Attacks", icon = icon("chevron-right"),
                                          plotlyOutput("plot6", height = 600),
                                          plotlyOutput("plot7", height = 900),
                                          br(),
                                          br(),
                                          h3("Suicide and Success Rates."),
                                          #h4("The pie charts below tells you about:"),
                                          br(),
                                          h5("Select the year and attack type to view the rates."),
                                          fluidRow(
                                            column(3, selectInput("year", "Year", choices = unique(gtd$iyear))),
                                            column(3, selectInput("attack", "Attack", choices = unique(as.character(gtd$attacktype1_txt)))),
                                            br(),
                                            br(),
                                            box(title = "Suicide", "Cases where there is evidence that the perpetrator did not intend to escape from the attack alive.",
                                                plotlyOutput("plot8", height = 400)),
                                                box(title = "Success", "Cases on whether or not the attacks were successful. Note that this is not based on if the incident was successful on a whole but if any steps in carrying out the attack was succuessful." , plotlyOutput("plot9", height = 400)),    
                                          
                                         )
                                          
                                          ),
                                 br(),
                                 tabPanel("Targets/Victims", icon = icon("chevron-right"),
                                          p(strong("Note: This page takes a few more seconds to load than the others."), style = "color:#990000"),
                                          h3("Main Targets"),
                                          plotlyOutput("plot10", height = 800, width = 800),
                                          br(),
                                          br(),
                                          br(),
                                          h3("Sub-Targets: More Detailed"),
                                          plotlyOutput("plot11", height = 1500, width = 1200),
                                          br(),
                                          br(),
                                          br(),
                                          h3("Where Are The Targets From?"),
                                          highchartOutput("plot12", height = 1000, width = 1000),
                                          br(),
                                          br(),
                                          br(),
                                          h3("What Were The Nationalities Of These Targets With Respect To Their Jobs/Statuses In Society?"),
                                          p(strong("Click on the different targets to expand or view nationalities.")),
                                          d3tree2Output("plot13", height = 1000, width = 1000)
                                          ),
                                 br(),
                                 tabPanel("Weapons", icon = icon("chevron-right"),
                                          h3("What Are The Type of Weapons Used? "),
                                          br(),
                                          plotlyOutput("plot14", height = 800),
                                          br(),
                                          br(),
                                          br(),
                                          h3("More Detail On Weapons Used By Attackers"),
                                          br(),
                                          plotlyOutput("plot15", height = 800),
                                          br(),
                                          br(),
                                          br(),
                                          h3("What Weapons Were Used By Attackers"),
                                          br(),
                                          selectInput("group", "Select Perpetrator Name ", choices = c("Select", unique(as.character(gtd$gname)))),
                                          plotlyOutput("plot16", height = 800)),
                                 br(),
                                 tabPanel("Perpetrators", icon = icon("chevron-right"),
                                          h3("Popular Terrorist Groups: Top 10 "),
                                          p(strong("Note that there are 3000+ different groups."), style = "color:#660000"),
                                          plotlyOutput("plot17", height = 800),
                                          br(),
                                          br(),
                                          br(),
                                          h3("How Often Were The Groups Successful?"),
                                          selectInput("group2", "Select Perpretator Name", choices = unique(as.character(gtd$gname))),
                                          plotlyOutput("plot18", height = 600)
                                          )
                 
           )),
           
           ####### TAB 2: MAP #########
           navbarMenu("Maps", icon = icon("map"),
                      
                      tabPanel("Static Map",
                               tags$img(src = "https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/overall_map.png", width = 1200, height = 800)
                      ),
                      
                      tabPanel("Interactive Map",
                               leafletOutput("terror_map", height = 900, width = 1200),
                    
                               absolutePanel(id = "controls", class = "panel panel-default", fixed = F,
                                  draggable = F, top = 80, right = 20,
                                  width = 270, height = "auto",
                                
                                  selectInput("select_country", 
                                              label = h4("Select a Country to view locations where incidents happened."),
                                              choices = unique(as.character(country_terror$country_txt))),
                                  
                                  plotOutput("static_map", height = 280),
                                  plotOutput("histTerror", height = 200),
                                  plotOutput("histDeath", height = 200),
                                  br(),
                                  br()
                                  )
                               ),
                              
                      tabPanel("Animated Map",
                               tags$img(src = "https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/terror_anim.gif", width = 1000, height = 900)
                               )    
                                  
            ), 
           
           ####### TAB 3: TEXT MINING #########
           tabPanel("Text Mining", icon = icon("font"),
                    sidebarLayout(position = "right",
                        sidebarPanel(
                          h3("From the database, 5 columns were chosen to do text analysis and converted into a word bank. See the details of each word bank below."),
                          br(),
                          br(),
                          h4("SUMMARY", style = "color:#990000"),
                          br(),
                          p("A brief narrative summary of the incident, noting the where, who, what, how, and why.", style = "color:#990000"),
                          
                          br(),
                          br(),
                          
                          h4("MOTIVE",  style = "color:#990000"),
                          br(),
                          p("Motivation for the attacks.",  style = "color:#990000"),
                          
                          br(),
                          br(),
                          
                          h4("RANSOM NOTE",  style = "color:#990000"),
                          br(),
                          p("Non-monetary demands made by perpetrators.", style = "color:#990000"),
                          
                          br(),
                          br(),
                          
                          h4("WEAPON DETAILS",  style = "color:#990000"),
                          br(),
                          p("Details of weapons used such as method used to conceal weapons, origins of weapons and the like.",  style = "color:#990000"),
                          
                          
                          br(),
                          br(),
                          
                          h4("PROPERTY DAMAGE",  style = "color:#990000"),
                          br(),
                          p("Property that was damaged in an attacks.",  style = "color:#990000"),
                          
                          br(),
                          br(), 
                          br(),
                          
                          selectInput("word", 
                                        label = h4("Select a word bank from menu:"),
                                        choices = list("Summary" = 1, 
                                                         "Motive" = 2, 
                                                         "Ransom Note" = 3, 
                                                         "Weapon Detail" = 4, 
                                                         "Property Damage" = 5))
                      
                        ),
                        mainPanel(
                            wordcloud2Output("mined_words", height = 1000, width = 1000)
                        )
                    )
            ),
      ####### TAB 4: DATA TABLE #########
           tabPanel("Data", icon = icon("database"),
                    h2("GLOBAL TERRORISM DATABASE 1970 - 2018"),
                      br(),
                    br(),
                    h3("Notes:"),
                    p(),
                    tags$ul(
                      tags$li(h4("GTD documents more than 190,000 international and domestic terrorist attacks that occurred worldwide since 1970. Specifically in this dataset there are 191464 observations with 135 variables concerning type of incident, target, reasons for the attack, weapons used and the like from 1970 to 2018 amoung 205 different countries.")), 
                      tags$li(h4("The attacks in the GTD are attributed to more than 2,000 named perpetrator organizations and more than 700 additional generic groupings.")), 
                      tags$li(h4("The authors of the GTD acknowledged that between 1970s and 1980s, events were under-accounted so caution would be noted when inferring results as there are a lot of data missing during those decades."))
                    ), 
                    br(),
                    br(),
                    h4(strong("For the full database, go to the", a("GTD Website", href = "https://www.start.umd.edu/gtd/"), ".", "Only the filtered dataset is shown below.")),
                    br(),
                    br(),
                    
                    fluidRow(
                      column(4,
                             selectInput("year",
                                         "Year",
                                         c("All",
                                           unique(as.character(gtd$iyear))))
                      ),
                      column(4,
                             selectInput("region",
                                         "Region:",
                                         c("All",
                                           unique(as.character(gtd$region_txt))))
                      ),
                      column(4,
                             selectInput("country",
                                         "Country:",
                                         c("All",
                                           unique(as.character(gtd$country_txt))))
                      )
                  
                    ),
                    
                    DT::dataTableOutput("gt_database")
                    ),
                   
      ####### TAB 5: INFORMATION ABOUT APP #########
           tabPanel("About App", icon = icon("info-circle"),
                    h2("GLOBAL TERRORISM DATABASE"), 
                    h4("Around the world Terrorism tends to have significant impacts on human rights with devastating consequences."),
                    h4("Terrorism can: "),
                    p(),
                    tags$ul(
                     tags$li(strong("Undermine or weaken Governments.")),
                     tags$li(strong("Jeopardize national security.")),
                     tags$li(strong("Threaten peace and both social and economic development."))
                     ),
                    br(),
                    h4("The Global Terrorism Database (GTD) defines terrorism as:"),
                    tags$blockquote("The threatened or actual use of illegal force and violence by a non-state actor to attain a political, economic, religious, or social goal through fear, coercion, or intimidation.", cite = "Global Terrorism Database"),
                    h4("Terrorism can take many forms and have multiple reasons leading up to the attack(s) spurning from a political or ideological goal at the expense of the general population and will always have dire consequences."),
                    br(),
                    br(),
                    hr(),
                    h3("Purpose and Relevance"),
                    p("The app was build mainly for analysis using the Global Terrorism Database as a key source in order to create interative visualizations with R's Shiny or Shiny Dashboard."),
                    p("As per the requirements in preparing this project, the data is of open source made available for researchers.In addition to this, the data used to create the app is only for academic purposes. The GTD is well-respected and highly-regarded as a comprehensive data source on global terrorism."),
                    br(), 
                    p(strong("With respect to counter-terrorism:")),
                    p(),
                    p("Individuals or groups may use terrorism because they do not like the current organisation of a society and believes that it needs to change with the use of violence as there is no other choice. Due to the significant resources available to counter terrorism, it is important to analyse and aggregate the data available to better understand the various attributes."),
                    br(),
                    br(),
                    hr(),
                    h3("Data Sources"),
                    p("The data was orginally retrieved from Kaggle but a more updated version was found on the GTD website. With the updated version, the app was built. See the links below."),
                    p("Original Database found on", a("Kaggle.com", href= "https://www.kaggle.com/START-UMD/gtd"),"only ranging from 1979 - 2017"),
                    p("Updated version:", a("Access GTD", href = "https://www.start.umd.edu/gtd/"), "ranging from 1970 to 2018. You may have to sign up to access data."),
                    p("The website also helped in obtaining additional information that may not be found directly in the dataset."),
                    p("GTD Cookbook: ", a("Data Dictionary", href= "https://www.start.umd.edu/gtd/downloads/Codebook.pdf")),
                    p("GTI Study: ", a("PDF", href = "http://visionofhumanity.org/app/uploads/2019/11/GTI-2019web.pdf")," which is a comprehensive study analysing the impact of terrorism for 163 countries and which covers 99.7% of the world's population. The GTI is also based on the Global Terrorism Database."),
                    br(),
                    hr(),
                    h3("Shiny References"),
                    p("Shiny: ", a("Tutorial", href = "https://shiny.rstudio.com/tutorial/")),
                    p("Shiny Dashboard: ", a("Tutorial", href = "https://rstudio.github.io/shinydashboard/index.html")),
                    p("Shiny Navbar: ", a("Example", href = "https://shiny.rstudio.com/gallery/navbar-example.html")),
                    br(),
                    p("Source code for this app can be found on",
                      a(strong("Github."), 
                        href = "https://github.com/javernw/DATA608-Knowledge-and-Visual-Analytics/tree/master/Final%20Project")),
                    br(),
                    br(),
                    hr(),
                    h4("App Created By:"),
                    p(),
                    h5("Javern Wilson, Student, CUNY School of Professional Studies"),
                    h5("Degree: M.S. in Data Science"),
                    p("Reason: Final Project in course DATA608 - Knowledge and Visual Analytics")
                    )
           )
)

############################################### SERVER ########################################################
  
server <- function(input, output, session) {
  ############################################# TEXT MINING ###################################################
  
  output$mined_words <- renderWordcloud2({
    
    if(input$word == 1){
      summary_words <- fread("https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/summary_freq_words.csv")
      wordcloud2(summary_words, size=1.6, color='random-dark')
      
    }else if (input$word == 2){
      motive_words <- fread("https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/motive_freq_words.csv")
      wordcloud2(motive_words, size=1.6, color='random-dark')
      
    }else if(input$word == 3){
      ransom_words <- fread("https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/ransom_note_freq_words.csv")
      wordcloud2(ransom_words, size=1.6, color='random-dark')

    }else if(input$word == 4){
      weapon_words <- fread("https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/weapon_detail_freq_words.csv")
      wordcloud2(weapon_words, size=1.6, color='random-dark')
    
    }else if(input$word == 5){
      property_words <- fread("https://raw.githubusercontent.com/javernw/DATA608-Knowledge-and-Visual-Analytics/master/Final%20Project/property_damage_freq_words.csv")
      wordcloud2(property_words, size=1.6, color='random-dark')
    }
  })
  ########################################## TEXT MINING ###########################################
  
  
  
  ######################################### MAPS ##################################################
  output$terror_map <- renderLeaflet({
    
    country_terror <-  country_terror %>%  filter(country_terror$country_txt == input$select_country)
    country_gangs <-  country_gangs %>%  filter(country_gangs$country_txt == input$select_country)
    country_deaths <-  country_deaths %>%  filter(country_deaths$country_txt == input$select_country)
    mytext <- paste(
      "Country: ", country_terror$country_txt, "<br/>", 
      "Province/State:", country_terror$provstate, "<br/>",
      "Attacks: ", country_terror$n, "<br/>", sep="") %>%
      lapply(htmltools::HTML)
    
    mytext2 <- paste(
      "Country: ", country_deaths$country_txt, "<br/>", 
      "Province/State:", country_deaths$provstate, "<br/>",
      "Deaths: ", country_deaths$nkill, "<br/>", sep="") %>%
      lapply(htmltools::HTML)
    
    mytext3 <- paste(
      "Country: ", country_gangs$country_txt, "<br/>", 
      "Province/State:", country_gangs$provstate, "<br/>",
      "Gangs: ", country_gangs$n, "<br/>", sep="") %>%
      lapply(htmltools::HTML)
    
      country_terror %>%  leaflet(options =leafletOptions(minZoom = 4)) %>% 
      addTiles()  %>% 
      addProviderTiles(providers$OpenStreetMap) %>%
      setMaxBounds(min(country_terror$longitude, na.rm = T), min(country_terror$latitude, na.rm = T), max(country_terror$longitude, na.rm = T), max(country_terror$latitude, na.rm = T)) %>% 
      addCircleMarkers(~longitude, ~latitude, 
                       radius = ~n,
                       fillOpacity = 0.2,
                       label = mytext,
                       color = "#FF0000",
                       group = "Attacks",
                       labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>%
        
        addCircleMarkers(data = country_deaths, ~longitude, ~latitude, 
                         radius = ~nkill,
                         fillOpacity = 0.2,
                         label = mytext2,
                         color = "#CC6600",
                         group = "Deaths",
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>% 
        
        addCircleMarkers(data = country_gangs, ~longitude, ~latitude, 
                         radius = ~n,
                         fillOpacity = 0.2,
                         label = mytext3,
                         color = "#000099",
                         group = "Gangs",
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>% 
        
        addLayersControl(
          overlayGroups = c("Attacks", "Deaths", "Gangs"),
          options = layersControlOptions(collapsed = FALSE), position = "topleft") %>% hideGroup(c("Deaths", "Gangs"))
      
   
      
      
      })

  output$static_map <-  renderPlot({
    ggmap_hide_api_key()
    register_google("AIzaSyBWspUCr5mzazuurMMIeGf2z3WrouNhTfs")
   selected_country <- country_terror %>%  filter(country_terror$country_txt == input$select_country) 
    ggmap(get_map(location = selected_country$country_txt[1], zoom = 4)) + 
      geom_point(aes(x = longitude, y = latitude,  colour = country_txt), data = country_terror, size = 0.5) + 
      theme(legend.position="none")
    
  })
    
  
    output$histTerror <- renderPlot({
        country_terror %>%  filter(country_terror$country_txt == input$select_country) %>% 
          ggplot() +
          geom_bar(stat = "identity", fill = "#FF3333", aes(x = iyear, y = n)) +
          theme(legend.position='none') + 
        ggtitle("Terrorist Attacks 1970-2018") + ylab("Count")
  })
    
    output$histDeath <- renderPlot({
      country_deaths %>%  filter(country_deaths$country_txt == input$select_country) %>% 
        ggplot() +
        geom_bar(stat = "identity", fill = "#330000", aes(x = iyear, y = nkill)) +
        theme(legend.position='none') + 
        ggtitle("Fatalities 1970-2018") + ylab("Count")
    })
 
  ########################################### MAPS #######################################################
    
    
  ########################################## DATA EXPLOERER ##############################################
    
    output$gt_database <- DT::renderDataTable(DT::datatable({
      if (input$year != "All") {
        gtd <- gtd[gtd$iyear == input$year,]
      }
      
      if (input$region != "All") {
        gtd <- gtd[gtd$region_txt == input$region,]
      }
      if (input$country != "All") {
        gtd <- gtd[gtd$country_txt == input$country,]
      }
      gtd
      
    }, options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#A0A0A0', 'color': '#fff'});",
        "}")
    ))
    )
    
    # output$downloadData <- downloadHandler(
    #   filename = function() {
    #     paste("Global_Terrorism_Database_", Sys.Date(), ".csv", sep="")
    #   },
    #   content = function(file) {
    #     write.csv(old_gtd, file, row.names = FALSE)
    #   }
    # )
    
    
    ######################EXPLORATION###################
    
    ######## TERRORIST ACTIVITIES
    output$plot1 <- renderPlotly({
      ggplotly( ggplot(year_tbl, aes(x=Year, y=Count, text = paste("Year: ", Year, "</br></br>Incidents: ", Count))) +
        geom_bar(stat="identity", aes(fill = Count)) +
        theme_minimal() +
        ggtitle("Terrorist activity per year") +
        scale_fill_gradient(low = "yellow", high = "red") +
        theme(legend.position="none") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)), tooltip = c("text"))
    })
    
    
    output$plot2 <- renderPlotly({
      f <- ts(ttrend, frequency = 1, start = 1970)
      if(input$region != "All") {
        autoplot(f[,input$region], col = "#CC6600") + labs(title = paste("Terrorist Activities in the", colnames(f)[which(colnames(f) == input$region)], "Region"), y = "Attacks")
      }else{
      autoplot(f)+ ggtitle("Trend in Terrorist Attacks")}
    })
    
    ####### FATALITIES
   
    output$plot3 <- renderPlotly({
     kill_region <- kill_region %>% filter(region_txt == input$region2) %>% 
       ggplot(aes(x = iyear, y = N_kills, colour = region_txt)) +
         geom_line(lwd = 1) + geom_point() +
         ggtitle("Killings Per Year By Region") +
         ylab("Casualties") +
         xlab("Year")
      
    })
      
      output$stream <- renderStreamgraph({
        kill_region %>%
          streamgraph("region_txt", "N_kills", "iyear", interpolate="cardinal") %>%
          sg_fill_brewer("Spectral") %>%
          sg_legend(show=TRUE, label="Region: ")
        
      })
      
      
      
    
    
    
    
     
    
    kbr <- gtd %>% select(iyear, region_txt, nkill) %>% count(iyear, region_txt, nkill)
    kbr <- kbr[complete.cases(kbr),]
    kbr <- kbr %>% dplyr::filter(nkill > 0)
  
   
    output$plot4 <- renderHighchart({
      kbr %>%  hctreemap2(group_vars = c("region_txt"),
                          size_var = "nkill" ,
                          color_var = "nkill",
                          allowDrillToNode = TRUE,
                          layoutAlgorithm = "squarified",
                          levelIsConstant = FALSE,
                          levels = list(
                            list(level = 1, dataLabels = list(enabled = TRUE)),
                            list(level = 2, dataLabels = list(enabled = FALSE)),
                            list(level = 3, dataLabels = list(enabled = FALSE))
                          )) %>% 
        hc_colorAxis(minColor = brewer.pal(7, "Reds")[1],
                     maxColor = brewer.pal(7, "Reds")[7]) %>% 
        hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
                             Fatalities: {point.value:,.0f} <br>")
                           
        
     
      })
      
     kbc <- gtd %>% select(iyear, country_txt, nkill) %>% count(iyear, country_txt, nkill)
     kbc <- kbc[complete.cases(kbc),]
     kbc <- kbc %>% dplyr::filter(nkill > 0)
      output$plot5 <- renderHighchart({
      
        kbc %>%  hctreemap2(group_vars = c("country_txt"),
                                     size_var = "nkill" ,
                                     color_var = "nkill",
                                     allowDrillToNode = TRUE,
                                     layoutAlgorithm = "squarified",
                                     levelIsConstant = FALSE,
                                     levels = list(
                                       list(level = 1, dataLabels = list(enabled = TRUE)),
                                       list(level = 2, dataLabels = list(enabled = FALSE)),
                                       list(level = 3, dataLabels = list(enabled = FALSE))
                                     )) %>% 
                hc_colorAxis(minColor = brewer.pal(7, "Reds")[1],
                       maxColor = brewer.pal(7, "Reds")[7]) %>% 
          hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
                             Fatalities: {point.value:,.0f} <br>")
      })
      
    
    ########### TYPES OF ATTACKS
      output$plot6 <- renderPlotly({
      ggplotly(ggplot(attack_tbl, aes(x= reorder(Type, Count), y= Count, text = paste("Attack Type: ", Type, "</br></br>Frequency: ", Count))) +
        geom_bar(stat="identity", aes(fill = Count)) +
        theme_minimal() + 
        scale_fill_viridis(discrete = F) +
        ggtitle("Types of Attacks") +
        xlab("Attack Type") +
        theme(legend.position="none") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        coord_flip(),tooltip = c("text"))
      
      
      })
      
      ### Attack Types
      output$plot7 <- renderPlotly({
        ggplotly(ggplot(attack_region, aes(fill=attacktype1_txt, y=Freq, x=region_txt, text = paste("Region: ", region_txt, "</br></br>Attack Type: ", attacktype1_txt, "</br>Frequency: ", Freq))) + 
          geom_bar(position="stack", stat="identity") + 
          scale_fill_viridis(discrete = T, option = "C") + 
          theme(legend.position="none") + 
          ggtitle("Types of Attacks in Region") + 
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          xlab("Region") + ylab("Frequency"), tooltip = c("text"))
          
        
      })
      
      ### Suicide
      output$plot8 <- renderPlotly({
        
        attacks_suicide <-  attacks_suicide_success %>% 
          select(suicide) %>% 
          table() %>% data.frame() %>% 
          mutate(per = Freq/sum(Freq)) %>% 
          mutate(suicide = ifelse(suicide == 1, 'Yes', 'No'))
        
        attacks_suicide <- attacks_suicide %>%  filter(iyear == input$year, attacktype1_txt == input$attack)
        colors <- c('rgb(204,204,0)', 'rgb(102, 0, 51)')
        plot_ly(attacks_suicide, labels = ~suicide,  values = ~Freq, type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~paste(attacktype1_txt, ':', Freq),
                marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)), showlegend = F) %>% 
          layout(title = 'Suicide Attack?')
        
      })
      
      ### Success
      output$plot9 <- renderPlotly({
        
        attacks_success <-  attacks_suicide_success %>% 
          select(success) %>% 
          table() %>% data.frame() %>% 
          mutate(per = Freq/sum(Freq)) %>% 
          mutate(success = ifelse(success == 1, 'Success', 'Failure'))
        
        attacks_success <- attacks_success %>% filter(iyear == input$year, attacktype1_txt == input$attack) 
        colors <- c('rgb(211,94,96)', 'rgb(0, 102, 51)')
        plot_ly(attacks_success, labels = ~success,  values = ~Freq, type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~paste(attacktype1_txt, ':', Freq),
                marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)), showlegend = F) %>% 
          layout(title = 'Attacks: Success or Failure?')
        
        
      })
      ### Targets
      output$plot10 <- renderPlotly({ 
        ggplotly( ggplot(targeted, aes(x= reorder(Target, Count), y= Count, text = paste("Target: ", Target, "</br></br>Count: ", Count))) +
        geom_bar(stat="identity", aes(fill = Target)) +
        theme_minimal() + 
        ggtitle("Victims of Terrorist Attacks") +
        xlab("Target") +
        scale_fill_viridis(discrete = T, option = "inferno")+
        theme(legend.position="none") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        coord_flip(), tooltip = c("text"))
      
      })
      
      ### Sub Targets
      output$plot11 <- renderPlotly({
       
        ggplotly( ggplot(sub_targeted, aes(x= reorder(Target, Count), y= Count, text = paste("Sub-Target: ", Target, "</br></br>Count: ", Count))) +
          geom_bar(stat="identity", aes(fill = Count)) +
          theme_minimal() + 
          scale_fill_viridis(discrete = F, option = "inferno") +
          ggtitle("Victims of Terrorist Attacks") +
          xlab("Target") +
          theme(legend.position="none") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          coord_flip(), tooltip = c("text"))
      })
      
      ### Targets Nationality
      output$plot12 <- renderHighchart({
        
        tn <- gtd %>% select(natlty1_txt, nkill) %>% count(natlty1_txt, nkill) %>% dplyr::ungroup()
        tn$natlty1_txt[tn$natlty1_txt == ""] <- "Unknown"
        tn <- tn[complete.cases(tn),]
        tn <- tn %>% dplyr::filter(nkill > 0)
        
        tn %>%   hctreemap2(group_vars = c("natlty1_txt"),
                            size_var = "nkill" ,
                            color_var = "nkill",
                            allowDrillToNode = TRUE,
                            layoutAlgorithm = "squarified",
                            levelIsConstant = FALSE,
                            levels = list(
                              list(level = 1, dataLabels = list(enabled = TRUE)),
                              list(level = 2, dataLabels = list(enabled = FALSE)),
                              list(level = 3, dataLabels = list(enabled = FALSE))
                            )) %>% 
          hc_colorAxis(minColor = brewer.pal(7, "PuRd")[1],
                       maxColor = brewer.pal(7, "PuRd")[7]) %>% 
          hc_tooltip(pointFormat = "<b>{point.name}</b><br>
                             Nationalities: {point.value:,.0f} <br>")
        
      
      })
      
      output$plot13 <- renderD3tree2({
        p <- treemap(gtd, 
                     index=c("targtype1_txt", "natlty1_txt"), 
                     vSize = "nkill", 
                     type = "index",
                     palette = "Spectral",  
                     title="Target Nationality", 
                     align.labels=list(
                       c("center", "center"), 
                       c("right", "bottom"))
        )
        
        d3tree2(p, rootname = "Targets and Nationalisty")
      })
      
 ### Weapons       
        output$plot14 <- renderPlotly({
          
          ggplotly(
          ggplot(weapons, aes(x=reorder(Weapon, Count), y= Count, text = paste("Weapon: ", Weapon, "</br></br>Count: ", Count))) +
            geom_bar(stat="identity", fill = "#004C99") +
            theme_minimal() + 
            ggtitle("Weapons Used") +
            xlab("Weapons") +
            theme(legend.position="none") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)), tooltip = c("text"))
        })
        
        
        output$plot15 <- renderPlotly({
          ggplotly(
            ggplot(sub_weapons[-1,], aes(x=reorder(Weapon, Count), y= Count, text = paste("Weapon: ", Weapon, "</br></br>Count: ", Count))) +
            geom_bar(stat="identity", fill = "#2E6DA4") +
            theme_minimal() + 
            ggtitle("Specific Weapons Used") +
            xlab("Weapons") +
            theme(legend.position="none") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)), tooltip = c("text"))
        })
          
          
          
        output$plot16 <- renderPlotly({
          
          if(input$group != "Select"){
            group_weapons <-  group_weapons %>%  filter(gname == input$group)
          colnames(group_weapons) <- c("Group", "Weapon", "Count")
          levels(group_weapons$Weapon)[1] <- "Unknown"
          
          ggplotly(
            ggplot(group_weapons, aes(x=Weapon, y= Count, text = paste("Weapon: ", Weapon ,"</br></br>Group Name: ", Group, "</br>Count: ", Count))) +
              geom_bar(stat="identity", fill = "#660000") +
              theme_minimal() + 
              ggtitle("Weapons Used By The Terrorists") +
              xlab("Weapons") +
              theme(legend.position="none") +
              theme(axis.text.x = element_text(angle = 90, hjust = 1)), tooltip=c("text"))
          
          } 
         
        })
        
        output$plot17 <- renderPlotly({
          ggplotly(
          ggplot(terrorists_groups, aes(x=Terrorist, y= Count, text = paste("Group Name: ", Terrorist, "</br></br>Count: ", Count))) +
            geom_bar(stat="identity", aes(fill = Terrorist )) +
            theme_minimal() + 
            ggtitle("Top Ten Terrorist Groups") +
            xlab("Groups") +
            theme(legend.position="none") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)),tooltip = c("text"))
          
        })
        
        output$plot18 <- renderPlotly({
          
          terrorists_success <-  terrorists_success %>% filter(gname == input$group2) %>% 
            mutate(per = Freq/sum(Freq)) %>% 
            mutate(success = ifelse(success == 1, 'Success', 'Failure'))
          
          colors <- c('rgb(51, 0, 102)', 'rgb(128, 128, 128)')
          plot_ly(terrorists_success, labels = ~success,  values = ~Freq, type = 'pie',
                  textposition = 'inside',
                  textinfo = 'label+percent', 
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',
                  text = ~paste(gname, ":", Freq),
                  marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)), showlegend = F) %>% 
            layout(title ='Success Rate')
        })
     
  ############################################ ABOUT APP ###############################################
  
      
  ############################################ ABOUT APP ###############################################
    
}

shinyApp(ui, server)