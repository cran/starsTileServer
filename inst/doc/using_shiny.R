## ---- include = FALSE---------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = NOT_CRAN
)

## ----setup--------------------------------------------------------------------
library(starsTileServer)

## ----download, eval=FALSE-----------------------------------------------------
#  require(ecmwfr)
#  request <-
#    list(
#      "dataset_short_name" = "reanalysis-era5-pressure-levels",
#      "product_type" = "reanalysis",
#      "variable" = c("temperature", "geopotential", "u_component_of_wind", "v_component_of_wind"),
#      "pressure_level" = c("875", "900", "925"),
#      "year" = "2000",
#      "month" = "04",
#      "day" = as.character(27:29),
#      "time" = sprintf("%02i:00", 0:23),
#      "area" = "64/-130/-64/144",
#      "format" = "netcdf",
#      "target" = "ecmwfData.nc"
#    )
#  # make sure you use your own uid and key ( https://cds.climate.copernicus.eu/#!/home )
#  wf_set_key("uid_to_replace", "key_to_replace", service = "cds")
#  ncfile <- wf_request(
#    user = "uid_to_replace",
#    request = request,
#    transfer = TRUE,
#    path = "~",
#    verbose = FALSE
#  )

## ---- echo=F------------------------------------------------------------------
if (!file.exists(tmpGridFile <- "~/ownCloudUva/test.nc")) {
  tmpGridFile <- tempfile(fileext = ".nc")
  download.file("https://surfdrive.surf.nl/files/index.php/s/Z6YoTyzyyAsmgGS/download", tmpGridFile, extra = "-q", method = "wget")
}

## ----stars--------------------------------------------------------------------
weatherData <- stars::read_stars(tmpGridFile)
sf::st_crs(weatherData) <- "+proj=longlat"

colFun <- function(x,
                   alpha = 1,
                   max = 20,
                   min = -20) {
  paste0(
    suppressWarnings(leaflet::colorNumeric(
      "RdYlBu",
      domain = c(as.numeric(min), as.numeric(max))
    )(x)),
    as.character(as.raw(as.numeric(alpha) * 255))
  )
}
attr(colFun, "colorType") <- "numeric"

## ----stards-------------------------------------------------------------------
# note the process is ran in the background, do not forget to close it as it might use quite a bit of memory.
# I have made the experience that callr seems to work poorly if the process is rather verbose
require(callr)
rp <- r_bg(args = list(grid = weatherData, colFun = colFun), function(grid, colFun) {
  starsTileServer::starsTileServer$new(grid, colFun)$run(port = 3746, docs = TRUE)
})

## ----startupPause, echo=F-----------------------------------------------------
Sys.sleep(3)
stopifnot(rp$is_alive())

## ----asdr---------------------------------------------------------------------
message(rp$read_error())

## ----app, echo=TRUE-----------------------------------------------------------
require(shiny)
require(leaflet)
require(stars)
weather <- stars::read_stars(tmpGridFile, proxy = T)

ui <- fluidPage(
  # Application title
  titlePanel("Web map"),
  sidebarLayout(
    # Sidebar with a slider input
    sidebarPanel(
      sliderInput("alpha", "Transparancy", 0, 1, .6, .01),
      selectInput("attr", "Attribute", choices = c("u", "v")),
      sliderInput(
        "time",
        "Time",
        value = min(st_get_dimension_values(weather, 4)),
        min = min(st_get_dimension_values(weather, 4)),
        max = max(st_get_dimension_values(weather, 4)),
        step = 3600,
        timezone = "+0000",
        animate = animationOptions(5000)
      ),
      selectInput("level", "level (mb)", choices = as.character(st_get_dimension_values(weather, 3))),
      sliderInput("colRange", "Range", -50, 50, c(-20, 20))
    ),
    # Show a plot of the generated distribution
    mainPanel(leafletOutput("map"))
  )
)

## ----adpp, echo=TRUE----------------------------------------------------------
server <- function(input, output, session) {
  # This reactive creates the URL to the tileserver, it include the different input variables in requests to the server
  # The debounce ensures the URL does not get updated to frequent
  url <- reactive({
    sprintf(
      "http://127.0.0.1:3746/map/%s/{z}/{x}/{y}?level=%s&alpha=%f&time=%s&min=%f&max=%f",
      input$attr,
      input$level,
      input$alpha,
      strftime(input$time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"),
      min(input$colRange),
      max(input$colRange)
    )
  }) %>% debounce(100)
  # This reactive downloads the color function from the server and prepares it for adding as a legend to the leaflet map
  colorFunction <- reactive({
    f <- readRDS(base::url(sprintf("http://127.0.0.1:3746/map/%s/colorfunctionnoalpha", input$attr)))
    at <- attributes(f)
    if (is.finite(max(colrange()))) {
      formals(f)$max <- max(colrange())
    }
    if (is.finite(min(colrange()))) {
      formals(f)$min <- min(colrange())
    }
    attributes(f) <- at
    f
  })
  colrange <- reactive(range(input$colRange))
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(-50, -30, 50, 50)
  })
  # This observer changes the tile layer as soon as the url is updated
  observe({
    leafletProxy("map") %>%
      clearGroup("wind") %>%
      addTiles(url(),
        group = "wind",
        options = tileOptions(useCache = TRUE, crossOrigin = TRUE)
      )
  })
  # This observe changes the legend as soon as it is updated
  observe({
    s <- seq(min(colrange()), max(colrange()), length.out = 20)
    leafletProxy("map") %>%
      clearControls() %>%
      addLegend(
        pal = colorFunction(),
        values = s,
        title = input$attr,
        position = "bottomleft"
      )
  })
}

## ----asdfrr-------------------------------------------------------------------
app <- shinyApp(ui, server)

## ----appimage, echo=FALSE-----------------------------------------------------
f <- tempfile(fileext = ".png")
suppressMessages(webshot::appshot(
  app,
  file = f,
  delay = 45,
  vwidth = 1000,
  vheight = 600
))
magick::image_read(f)

## ----result-------------------------------------------------------------------
rp$finalize()

