library(shiny)
library(plotly)
library(nhlscrape)


SetDbPath("nhl.sqlite")
gids <- QueryDb("SELECT DISTINCT game_id FROM events")[,1]

ui <- fluidPage(

  titlePanel("Game Events"),

  sidebarPanel(
    selectInput("game_id", "Choose a Game:",
                choices = gids),

    uiOutput("eventList"),
    width = 3
  ),

  mainPanel(
    h3(textOutput("gameInfo")),
    plotlyOutput("plot")
  )

)

# Define server logic required to summarize and view the selected dataset
server <- function(input, output) {

  output$game_id <- renderText({
    input$game_id
  })

  output$eventList <- renderUI({
    events <- QueryDb(paste("SELECT DISTINCT result_event FROM events WHERE game_id=",
                            input$game_id,
                            sep=""))[,1]
    values <- lapply(events, function(x) {paste("'", x, "'", sep="")})
    checkboxGroupInput("events", "Select Events:", choiceNames=events, choiceValues=values)
  })

  output$gameInfo <- renderText({
    request <- paste("game/", input$game_id, "/boxscore", sep="")
    game_feed <- GetApiJson(request)
    info_str <- paste(game_feed$teams$away$team$name,
                      " at ",
                      game_feed$teams$home$team$name,
                      sep="")
    info_str
  })

  output$plot <- renderPlotly({
    bg <- png::readPNG("images/nhlIce.png")

    events_sql <-  paste(input$events, collapse=",")

    event_pts <- QueryDb(paste("SELECT DISTINCT coordinates_x,
    coordinates_y,
    result_description,
    about_period,
    about_periodTime,
    result_event FROM events WHERE game_id=",
                               input$game_id,
                               " AND result_event IN (",
                               events_sql, ")",
                               sep=""))
    xaxis <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      range = c(-100, 100),
      fixedrange = TRUE
    )
    yaxis <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      range = c(-42.5, 42.5),
      fixedrange = TRUE
    )

    plot_ly(event_pts, x = ~coordinates_x, y = ~coordinates_y, color= ~result_event,
            marker = list(size = 10, line = list(color = 'black', width = 2)),
            text = ~result_event,
            hovertemplate = ~paste(
              "<b>%{text}</b><br><br>",
              result_description,
              "<extra></extra>"
            )) %>%
      config(displayModeBar = F) %>%
        layout(
          width = 1000,
          height = 500,
          xaxis = xaxis,
          yaxis = yaxis,
          legend = list(orientation = 'h', y=-42.5),
          showlegend=TRUE,
          images = list(
            list(
              source = raster2uri(as.raster(bg)),
              xref = "x",
              yref = "y",
              x = -100,
              y = 42.5,
              sizex = 200,
              sizey = 85,
              sizing = "stretch",
              opacity = 1,
              layer = "below"
            )))
  })

}

shinyApp(ui, server)
