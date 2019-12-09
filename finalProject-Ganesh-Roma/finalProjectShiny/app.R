#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
Sys.setlocale('LC_ALL','C')
library(profvis)
library(shiny)
library(tidyverse)
library(circlize)
library(magick)
library(shinycssloaders)
library(shinydashboard)
options(shiny.sanitize.errors = TRUE)
options(shiny.reactlog = TRUE)



numberOfPlayersValues <- list("2","3","4","5","6")
refreshIntervalValue<- list("0.01","0.05","0.1","0.25","0.5","1","2")
# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title ='Grand Slam Heroes'),
  dashboardSidebar(sidebarMenu(
    menuItem("Purpose", tabName = "introduction", icon = icon("dashboard")),
    menuItem("About the Data", tabName = "dataSet", icon = icon("table")),
    menuItem("Shiny Visual", tabName = "visual", icon = icon("eye")),
    menuItem("Shiny Performance", tabName = "performance", icon = icon("bolt")),
    menuItem("Quality Assurance", tabName = "qa", icon = icon("microscope"))
  )
  ),
  dashboardBody(
    
    # Application title
    tabItems(
      tabItem(tabName='performance',
              fluidRow(
                tabBox(
                  title = "Profiling and Scalability",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "500px", width ="800",
                  tabPanel("Profvis", img(src="Capture_combined.JPG")),
                  tabPanel(" Chrome flame graph", img(src="Capture_chrome_comb.JPG")),
                  tabPanel(" ReactLog", img(src="profiling.png"))
                ),
              ),
      ),
        tabItem(tabName='introduction',
                fluidRow(
                    title = "Detailed analysis of Grand Slam Hero's using Shiny",
                    # The id lets us use input$tabset1 on the server to find the current tab
                    tags$body(
                      h1("Detailed analysis of Grand Slam Hero's using Shiny"),
                    br(),h4("The Open Era is the current era of professional tennis. It began in 1968 when the Grand Slam tournaments allowed professional players to compete with amateurs, ending the division that had persisted since the dawn of the sport in the 19th century."),
                    br(), h4("The purpose of our project was to find out the best player in any era of tennis by birth year, classify them according to courts further drill down into gender and by year of tournament. We then next look under the hood to ensure the visual works as designed with good performance and quality"),
                    br(),
                    HTML('<center><img src="grandslam.jpg"></center>')
                    ),
                    ),
                fluidRow(
                  tags$body(
                  br(),"presented by - Roma and Ganesh",
                  br(), "Github Link:",
                  br(),"Rstudio Cloud Link: https://rstudio.cloud/project/704614",
                  br(),"Bookdown Link: https://bookdown.org/rdutta4/bookdown-grandslam/",
                  br(),"ShinyAppsIOLink: https://ganesh-viswanathan.shinyapps.io/finalProjectShiny/",
                  )
                  ),
        ),
      
      tabItem(tabName='qa',
              fluidRow(
                tabBox(
                  title = "Shiny Test Automation",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "500px", width ="800",
                  tabPanel("Recorded Test", img(src="TestSnap1.PNG")),
                  tabPanel(" Outcome and expected result", img(src="TestSnap2.PNG"))
                ),
              ),
      ),
      
      tabItem(tabName='dataSet',
              fluidRow(
              tabBox(
                title = "Peep into the Data sets",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "500px", width ="800",
                tabPanel("PlayerData", tableOutput('tableData1')),
                tabPanel("GrandSlamData", tableOutput('tableData2')),
                tabPanel("GrandSlamTimeline", tableOutput('tableData3'))
              ),
      ),
      ),
      
      # Sidebar with a slider input for number of bins
      tabItem(tabName='visual',
              fluidRow(
                column(3,
                       selectInput(inputId = "numberOfPlayers",label = "Top N rank players",choices = numberOfPlayersValues,selected='2'),
                       sliderInput(inputId = "yearValue", label = "Era of players-by Birth Date", value = 1977, min = 1968, max = 2000,step = 5),
                       tags$h4('Player Gender'),
                       checkboxInput(inputId="male",label = "Male", value = TRUE),
                       checkboxInput(inputId="female",label = "Female", value = TRUE),
                       tags$h4('Player Tourney'),
                       checkboxInput(inputId="wimbledon",label = "Wimbledon", value = TRUE),
                       checkboxInput(inputId="usOpen",label = "US Open", value = TRUE),
                       checkboxInput(inputId="ausOpen",label = "Australian Open", value = TRUE),
                       checkboxInput(inputId="frenchOpen",label = "French Open", value = TRUE),
                       hr(),
                       actionButton(inputId = "run", label = "Rerun")
                       
                ),
                
                # Show a plot of the generated distribution
                column(5,
                       selectInput(inputId = "refreshInterval",label = "Refresh Rate-Frames per second(fps)",choices = refreshIntervalValue,selected = '0.5'),
                       withSpinner(imageOutput("tennisImage",width = '100%'))
                ),
                column(4,
                       withSpinner(DT::dataTableOutput('table', width = "100%", height = "auto"))    
                )
              )
      )
    )
  )
)


dataPrepPlayers <- function(){
  path <- paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2019/2019-04-09/'
  )
  players <- read_csv(paste0(path, 'player_dob.csv'))
}

dataGrandSlamsTimeline <- function(){
  path <- paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2019/2019-04-09/'
  )
  grand_slams <- read_csv(paste0(path, 'grand_slam_timeline.csv'))
  head(grand_slams)
}

dataGrandSlams <- function(){
  path <- paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2019/2019-04-09/'
  )
  grand_slams <- read_csv(paste0(path, 'grand_slams.csv'))
  head(grand_slams)
}

dataPrepGrandSlam <- function() {
  # data preparation
  path <- paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2019/2019-04-09/'
  )
  players <- read_csv(paste0(path, 'player_dob.csv'))
  grand_slams <- read_csv(paste0(path, 'grand_slam_timeline.csv'))
  
  last_round <- function(outcome) {
    case_when(
      outcome == 'Won'             ~   1,
      outcome == 'Finalist'        ~   2,
      outcome == 'Semi-finalist'   ~   4,
      outcome == 'Quarterfinalist' ~   8,
      outcome == '4th Round'       ~  16,
      outcome == '3rd Round'       ~  32,
      outcome == '2nd Round'       ~  64,
      outcome == '1st Round'       ~ 128
    )
  }
  
  grand_slams <- grand_slams %>%
    mutate(
      last_round = last_round(outcome),
      gender = fct_recode(gender, 'female' = 'Female', 'male' = 'Male')
    ) %>%
    filter(!is.na(last_round))
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$tableData1 <- renderTable({ head(dataPrepPlayers()) })
  output$tableData2 <- renderTable({ dataGrandSlams() })
  output$tableData3 <- renderTable({ dataGrandSlamsTimeline() })
  
  processPlayers <- reactive({# select four female and male tennis players (aged < 39) with most won
    players <- dataPrepPlayers()
    grand_slams <- dataPrepGrandSlam()
    # grand slam tournaments
    dob = as.Date(ISOdate(input$yearValue, 1, 1))
    (best_players <- grand_slams %>%
        left_join(players, by = c('player' = 'name')) %>%
        filter(date_of_birth < dob, last_round == 1) %>%
        group_by(gender, player) %>%
        summarize(total = n()) %>%
        group_by(gender) %>%
        top_n(as.numeric(input$numberOfPlayers), total) %>%
        arrange(gender, total))
    best_players})
  
  processGender <- reactive({
    #Gender selection
    best_players <- processPlayers()
    if(input$male && !input$female){
      best_players <- best_players %>%
        filter(gender=='male')}
    if(!input$male && input$female){
      best_players <- best_players %>%
        filter(gender=='female')}
    best_players})
  
  processGrandSlam <-reactive({
    players <- dataPrepPlayers()
    grand_slams <- dataPrepGrandSlam()
    best_players <- processGender()
    grand_slams_best_players <- grand_slams %>%
      filter(player %in% pull(best_players, player), last_round <= 4) %>%
      select(player, gender, year, tournament, last_round)
    subVal=1
    if(!input$wimbledon && !input$usOpen && !input$ausOpen && !input$frenchOpen){
      subVal=1
    }else{
      if(!input$wimbledon){
        grand_slams_best_players <- grand_slams_best_players %>%
          filter(tournament!='Wimbledon')
        best_players <- best_players %>%
          filter(player %in% unique(grand_slams_best_players$player))
        subVal=subVal+1}
      if(!input$usOpen){
        grand_slams_best_players <- grand_slams_best_players %>%
          filter(tournament!='US Open')
        best_players <- best_players %>%
          filter(player %in% unique(grand_slams_best_players$player))
        subVal=subVal+1}
      if(!input$frenchOpen){
        grand_slams_best_players <- grand_slams_best_players %>%
          filter(tournament!='French Open')
        best_players <- best_players %>%
          filter(player %in% unique(grand_slams_best_players$player))
        subVal=subVal+1}
      if(!input$ausOpen){
        grand_slams_best_players <- grand_slams_best_players %>%
          filter(tournament!='Australian Open')
        best_players <- best_players %>%
          filter(player %in% unique(grand_slams_best_players$player))
        subVal=subVal+1}
    }
    list(best_players=best_players, grand_slams_best_players=grand_slams_best_players, subVal=subVal)})
  
  processInputAndRank <- eventReactive(input$run,{
    best_players <- processGrandSlam()$best_players
    grand_slams_best_players <- processGrandSlam()$grand_slams_best_players
    subVal <- processGrandSlam()$subVal
    
    # prepare adjacency list for chord diagram
    player_tournament <- grand_slams_best_players %>%
      filter(last_round == 1) %>%
      group_by(player, tournament) %>%
      summarize(total = n()) %>%
      ungroup() %>%
      transmute(player, to = tournament, total)
    
    
    
    
    player_year <- grand_slams_best_players %>%
      filter(last_round == 1) %>%
      group_by(player, year) %>%
      summarize(total = n()) %>%
      ungroup() %>%
      transmute(player, to = as.character(year), total)
    
    player_last_round <- grand_slams_best_players %>%
      group_by(player, last_round) %>%
      summarize(total = n()) %>%
      ungroup() %>%
      transmute(player, to = as.character(last_round), total)
    
    # build and save chord diagrams for n selected tennis players
    
    
    imap(best_players$player, function(player, idx) {
      adjacency_list <-
        bind_rows(player_tournament, player_year, player_last_round) %>%
        mutate(
          color = case_when(
            player == !!player & str_starts(to, 'A')      ~ '#e69f00',
            player == !!player & str_starts(to, 'F')      ~ '#56b4e9',
            player == !!player & str_starts(to, 'W')      ~ '#009e73',
            player == !!player & str_starts(to, 'U')      ~ '#f0e442',
            player == !!player & str_starts(to, '1')      ~ '#1072b2',
            player == !!player & str_starts(to, '2')      ~ '#d55e00',
            player == !!player & str_starts(to, '4')      ~ '#cc79a7',
            player == !!player & str_detect(to, '\\d{4}') ~ '#000000',
            TRUE ~ '#efefef80'
          ),
          to = case_when(
            to == 1 ~ 'Champion',
            to == 2 ~ 'Final',
            to == 4 ~ 'Semi-Final',
            TRUE ~ to
          ),
          rank = if_else(color == '#efefef80', 1, 2)
        )
      
      # prepare colors
      years <- sort(unique(player_year$to))
      year_colors <-  rep('#dedede', length(years))
      names(year_colors) <- years
      
      players <- best_players$player
      player_colors <- rep('#dedede', length(players))
      names(player_colors) <- players
      
      colors <- c(
        'Australian Open' = '#c54950', 'French Open' = '#2a9e46',
        'Wimbledon' = '#3766aa', 'US Open' = '#6b42b8',
        year_colors,
        'Champion' = '#c54950', 'Final' = '#2a9e46', 'Semi-Final' = '#3766aa',
        player_colors
      )
      
      player_years <- adjacency_list %>%
        filter(player == !!player, str_detect(to, '\\d{4}')) %>%
        pull(as.integer(to))
      
      colors[player_years] <- '#777777'
      colors[player] <- '#333333'
      
      # create image
      png(
        file = paste0('images/chord_diagram_', idx, '.png'),
        height = 7, width = 7,  units = 'in', res = 300
      )
      
      circos.par(
        gap.after = c(
          rep(2, 3), 9, rep(2, length(years) - 1), 9, rep(2, 2), 15,
          rep(2, length(unique(best_players$player))-subVal), 15
        ),
        start.degree = 90
      )
      
      
      par(
        col = '#333333', col.main = '#333333', mar = c(0, 0, 3.1, 0), bg = '#fef9f4'
      )
      
      chordDiagram(
        select(adjacency_list, player, to, total),
        order = names(colors),
        grid.col = colors,
        col = pull(adjacency_list, color),
        transparency = 0.4,
        annotationTrack = 'grid',
        preAllocateTracks = list(list(track.height = 0.2)),
        link.rank = pull(adjacency_list, rank),
      )
      
      circos.track(
        track.index = 1,
        panel.fun = function(x, y) {
          circos.text(
            CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
            facing = 'clockwise', niceFacing = TRUE, adj = c(-0.025, 0.5),
            cex = 0.6
          )
        },
        bg.border = NA
      )
      
      text(-1, -1, '#tidytuesday 15|2019', cex = 0.5)
      text(1, -1, '© 2019 final project', cex = 0.5)
      title('Grand Slam Heroes of the Modern Era')
      
      dev.off()
    })
    player_tournament <- player_tournament %>%
      group_by(player) %>%
      #list(best_players, player_year,player_tournament,player_last_round)
      list(player=best_players, len=length(best_players$player), tourn=player_tournament)
  },ignoreNULL =FALSE)
  
  processRefreshRate <- eventReactive({input$refreshInterval
    input$run},{
      frameLength = processInputAndRank()$len
      frames <- map(1:as.numeric(frameLength), function(idx) {
        file <- paste0('images/chord_diagram_', idx, '.png')
        img <- image_read(file)
        image_scale(img, '640x640')
      })
      animation <- image_animate(image_join(frames), fps = as.numeric(input$refreshInterval))
      
      image_write(
        image = animation,
        path = 'images/tennisOutput.gif',
        quality = 100
      )
    },ignoreNULL = FALSE)
  
  output$tennisImage <- renderImage({
    
    v <- processRefreshRate()
    list(src = "images/tennisOutput.gif",
         contentType = 'image/gif',
         width = '100%'
         #height = '100%'
         # alt = "This is alternate text"
    )
  })
  output$table <- DT::renderDataTable({ processInputAndRank()$tourn })
 
}



# Run the application
shinyApp(ui = ui, server = server)