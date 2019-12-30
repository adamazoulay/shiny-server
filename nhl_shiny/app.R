library(shiny)
library(plotly)
library(nhlscrape)
library(MASS)


SetDbPath("nhl.sqlite")
gids <- QueryDb("SELECT DISTINCT game_id FROM events")[,1]
teams <- QueryDb("SELECT name FROM teams")[,1]

ui <- fluidPage(

  titlePanel("Game Events"),

  sidebarPanel(

    selectInput("team_name", "Choose a team:",
                choices = teams, selected = "Toronto Maple Leafs"),

    dateRangeInput('date_range',
                   label = 'Select a date range to find games:',
                   start = "2019-09-30", end = "2019-10-03"),

    selectInput("plot_type", "Choose a plot type:",
                choices = c("Scatter", "Heatmap")),

    uiOutput("eventList"),
    width = 3
  ),

  mainPanel(
    #h3(textOutput("gameInfo")),
    plotlyOutput("plot", width = "100%")
  )

)

# Define server logic required to summarize and view the selected dataset
server <- function(input, output) {


  output$eventList <- renderUI({
    team_id <- GetTeamId(input$team_name)
    gids <- paste(GetGameIdRange(team_id,
                                 format(input$date_range[1]),
                                 format(input$date_range[2])), collapse=",")
    events <- QueryDb(paste("SELECT DISTINCT result_event FROM events WHERE game_id IN (", gids, ")",
                            sep=""))[,1]
    values <- lapply(events, function(x) {paste("'", x, "'", sep="")})
    checkboxGroupInput("events", "Select Events:", choiceNames=events, choiceValues=values)
  })

  output$gameInfo <- renderText({
    "Temp"
  })

  observe({
  })

  output$plot <- renderPlotly({

    team_id <- GetTeamId(input$team_name)
    gids <- paste(GetGameIdRange(team_id,
                                 format(input$date_range[1]),
                                 format(input$date_range[2])), collapse=",")
    events_sql <-  paste(input$events, collapse=",")

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

    if (input$plot_type == "Scatter") {
      bg <- png::readPNG("images/nhlIce.png")

      event_pts <- QueryDb(paste("SELECT DISTINCT coordinates_x,
      coordinates_y,
      result_description,
      about_period,
      about_periodTime,
      game_id,
      result_event FROM events WHERE team_id=", team_id,
                                 " AND game_id IN (", gids, ")",
                                 " AND result_event IN (", events_sql, ")",
                                 sep=""))

      plot_ly(event_pts, x = ~coordinates_x, y = ~coordinates_y, color= ~result_event,
              marker = list(size = 10, line = list(color = 'black', width = 2)),
              text = ~result_event,
              hovertemplate = ~paste(
                "<b>%{text}</b><br><br>",
                result_description,
                "<br>Game id: ", game_id,
                "<br>Period: ", about_period,
                "<br>Time: ", about_periodTime,
                "<extra></extra>",
                sep=""
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
            list(source = raster2uri(as.raster(bg)),
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

    } else if (input$plot_type == "Heatmap") {
      bg <- png::readPNG("images/nhlIceHeat.png")

      event_pts <- GetHeatmapCoords(team_id, gids, events_sql)

      # Generate 2d histogram with the points
      nbins <- 25
      x.bin <- seq(floor(min(event_pts$coordinates_x)),
                   ceiling(max(event_pts$coordinates_x)),
                   length=nbins)
      y.bin <- seq(floor(min(event_pts$coordinates_y)),
                   ceiling(max(event_pts$coordinates_y)),
                   length=nbins)

      freq <-  as.data.frame(table(findInterval(event_pts$coordinates_x, x.bin),
                                   findInterval(event_pts$coordinates_y, y.bin)))
      freq2D <- diag(nbins)*0
      freq2D[cbind(freq[,1], freq[,2])] <- freq[,3]

      plot_ly(x = x.bin, y = y.bin) %>%
        config(displayModeBar = F) %>%
        layout(
          width = 1000,
          height = 500,
          xaxis = xaxis,
          yaxis = yaxis,
          legend = list(orientation = 'h', y=-42.5),
          showlegend=FALSE,
          images = list(
            list(source = raster2uri(as.raster(bg)),
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

    }
  })
}

shinyApp(ui, server)
