# paketit
list.of.packages <- c("shiny", "dplyr","leaflet","sf","bslib","shinyWidgets","ragg","shinycssloaders","lubridate","leaflet.extras")
# Käytä paketteja
lapply(list.of.packages, require, character.only = TRUE)

# Haetaan reitin data vaan kerran
if (!file.exists("./data/controls.RDS")){
  dir.create("./data")
  library(sf)
  library(dplyr)
  layers <- st_layers("https://www.randonneurs.fi/live/ruska2022/route.kml")
  layers
  # Driver: LIBKML 
  # Available layers:
  #   layer_name geometry_type features fields crs_name
  # 1                      Start                      2     11   WGS 84
  # 2                  Control 1                      2     11   WGS 84
  # 3                  Control 2                      1     11   WGS 84
  # 4                  Control 3                      1     11   WGS 84
  # 5                     Finish                      4     11   WGS 84
  # 6     Start Parkour option 1                      3     11   WGS 84
  # 7     Start Parkour option 2                      3     11   WGS 84
  # 8 Control 1 Parkour option 1                      3     11   WGS 84
  # 9 Control 1 Parkour option 2                      3     11   WGS 84
  
  # Rastit
  lst_route <- list()
  tmp <- st_read("https://www.randonneurs.fi/live/ruska2022/route.kml", layer = "Start")
  lst_route[[1]] <- st_zm(tmp)[1,] %>% mutate(name_fi = "lähtö")
  lst_route[[2]] <- st_zm(tmp)[2,] %>% mutate(name_fi = "1. parkourin loppu")
  tmp <- st_read("https://www.randonneurs.fi/live/ruska2022/route.kml", layer = "Control 1")
  lst_route[[3]] <- st_zm(tmp)[1,] %>% mutate(name_fi = "1. rasti")
  lst_route[[4]] <- st_zm(tmp)[2,] %>% mutate(name_fi = "2. parkourin loppu")
  tmp <- st_read("https://www.randonneurs.fi/live/ruska2022/route.kml", layer = "Control 2")
  lst_route[[5]] <- st_zm(tmp)[1,] %>% mutate(name_fi = "2. rasti")
  tmp <- st_read("https://www.randonneurs.fi/live/ruska2022/route.kml", layer = "Control 3")
  lst_route[[6]] <- st_zm(tmp)[1,] %>% mutate(name_fi = "3. rasti")
  tmp <- st_read("https://www.randonneurs.fi/live/ruska2022/route.kml", layer = "Finish")
  lst_route[[7]] <- st_zm(tmp)[1,] %>% mutate(name_fi = "3. pk alku")
  lst_route[[8]] <- st_zm(tmp)[2,] %>% mutate(name_fi = "3. pk p2")
  lst_route[[9]] <- st_zm(tmp)[3,] %>% mutate(name_fi = "3. pk p3")
  lst_route[[10]] <- st_zm(tmp)[4,] %>% mutate(name_fi = "maali")
  rastit <- do.call("rbind", lst_route)
  saveRDS(rastit, "./data/controls.RDS")
  
  # Parkourit
  lst_route <- list()
  tmp <- st_read("https://www.randonneurs.fi/live/ruska2022/route.kml", layer = "Start Parkour option 1")
  lst_route[[1]] <- st_zm(tmp)[1,] %>% mutate(name_fi = "1. Parkour 1")
  tmp <- st_read("https://www.randonneurs.fi/live/ruska2022/route.kml", layer = "Start Parkour option 2")
  lst_route[[2]] <- st_zm(tmp)[1,] %>% mutate(name_fi = "1. Parkour 2")
  tmp <- st_read("https://www.randonneurs.fi/live/ruska2022/route.kml", layer = "Control 1 Parkour option 1")
  lst_route[[3]] <- st_zm(tmp)[1,] %>% mutate(name_fi = "2. Parkour 1")
  tmp <- st_read("https://www.randonneurs.fi/live/ruska2022/route.kml", layer = "Control 1 Parkour option 2")
  lst_route[[4]] <- st_zm(tmp)[1,] %>% mutate(name_fi = "2. Parkour 2")
  parkourit <- do.call("rbind", lst_route)
  saveRDS(parkourit, "./data/parcours.RDS")

}

controls <- readRDS("./data/controls.RDS")
parcours <- readRDS("./data/parcours.RDS")


# Käyttöliittymä
ui <- fluidPage(theme = bslib::bs_theme(version = 5, bootswatch = "cosmo",
                                        base_font = font_google("Space Mono"),
                                        code_font = font_google("Space Mono")),

    titlePanel("Ruska2022 varjoseuranta"),
    uiOutput("time"),
    fluidRow(column(width = 6,
                    tags$p(tags$code("#ruska2022"), 
                           tags$a(href = "https://twitter.com/hashtag/ruska2022?f=live", "twitter"),"/",
                           tags$a(href = "https://www.instagram.com/explore/tags/ruska2022/", "instagram"),
                           # HTML("<br/>"), 
                           "|", tags$a(href = "https://www.randonneurs.fi/ruska-2022/", "kotisivu"),
                           # HTML("<br/>"), 
                           # HTML("<br/>"),
                           "|",
                          tags$a(href = "https://github.com/muuankarski/varjoseuranta", "Avoin lähdekoodi"))
                    ),
             column(width = 6,
                    actionButton(
                      inputId = "submit_loc",
                    label = "Päivitä data", class = "btn btn-outline-dark"),
             tags$div(style = "padding-top: 10px;"),
             checkboxInput("input_show_tail", 
                           label = "Reitti", 
                           value = FALSE)
                    )),
    tags$hr(),
    fluidRow(column(12,
    shinycssloaders::withSpinner(ui_element = leafletOutput("map", width = "95%", height = "700px"), 
                                 type = 6)
    ))
)

# Serverin logiikka
server <- function(input, output) {
  
  autoInvalidateTime <- reactiveTimer(1000)
  
  output$time <- renderUI({
    autoInvalidateTime()
    Sys.setenv(TZ='Europe/Helsinki')
    dt <- as.POSIXct("2022-09-17 23:59:59") - Sys.time()
    units(dt) <- "secs"
    tagList(
      HTML(paste0("<p class = 'font-monospace'>ajoaikaa jäljellä: <strong>", as.character(lubridate::seconds_to_period(round(as.numeric(dt)))),"</strong></p>"))
    )
  })
  
  get_data <- reactive({
    
    input$submit_loc

    urli <- "https://www.randonneurs.fi/live/ruska2022/current.kml"
    layers <- sf::st_layers(urli)
    
    datx <- try(dat1 <- sf::st_read(urli, layer = layers$name))
    if (!"try-error" %in% class(datx)){
    dat1 <- datx |> sf::st_zm() %>% 
        mutate(labName = gsub(" min ago", "m", Name),
               kuvaus = gsub("<a", "<br/><a", gsub("<p>|</p>|</h5>", "", gsub("<h5 align=center>", "<br/>Nro: ", gsub("h4", "strong", description))))) #%>% 
      
      
      urli <- "https://www.randonneurs.fi/live/ruska2022/iida-current.kml"
      layers <- sf::st_layers(urli)
      dat2 <- sf::st_read(urli, layer = layers$name) |> sf::st_zm() %>% 
        mutate(labName = gsub(" min ago", "m", Name),
               kuvaus = gsub("<a", "<br/><a", gsub("<p>|</p>|</h5>", "", gsub("<h5 align=center>", "<br/>Nro: ", gsub("h4", "strong", description)))))
      dat4 <- rbind(dat1,dat2)
    } else {
      
      dat4 <- structure(list(Name = "Data Puuttuu (1 min ago)", 
                    description = "<h4 align=\"center\">Petteri Punakuono</h4><h5 align=center>666</h5><p><a href=\"https://twitter.com/mkpaa\">Twitter</a> </p>", 
                    timestamp = structure(NA_real_, class = c("POSIXct", "POSIXt"
                    )), begin = structure(NA_real_, class = c("POSIXct", "POSIXt"
                    )), end = structure(NA_real_, class = c("POSIXct", "POSIXt"
                    )), altitudeMode = NA_character_, tessellate = -1L, extrude = 0L, 
                    visibility = -1L, drawOrder = NA_integer_, icon = NA_character_, 
                    labName = "Data Puuttuu (1 min ago)", kuvaus = "<strong align=\"center\">Data Puuttuu</strong><br/>", 
                    geometry = structure(list(structure(c(21.278333, 69.323611
                    ), class = c("XY", "POINT", "sfg"))), class = c("sfc_POINT", 
                                                                    "sfc"), precision = 0, bbox = structure(c(xmin = 21.278333, 
                                                                                                              ymin = 69.323611, xmax = 21.278333, ymax = 69.323611), class = "bbox"), crs = structure(list(
                                                                                                                input = "WGS 84", wkt = "GEOGCRS[\"WGS 84\",\n    DATUM[\"World Geodetic System 1984\",\n        ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"geodetic latitude (Lat)\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"geodetic longitude (Lon)\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    ID[\"EPSG\",4326]]"), class = "crs"), n_empty = 0L)), sf_column = "geometry", agr = structure(c(Name = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   description = NA_integer_, timestamp = NA_integer_, begin = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   end = NA_integer_, altitudeMode = NA_integer_, tessellate = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   extrude = NA_integer_, visibility = NA_integer_, drawOrder = NA_integer_, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   icon = NA_integer_, labName = NA_integer_, kuvaus = NA_integer_
                                                                                                                ), levels = c("constant", "aggregate", "identity"), class = "factor"), row.names = 1L, class = c("sf", 
                                                                                                                                                                                                                 "data.frame"))
    }

    return(dat4)
  })
  
  
  get_tail <- reactive({
    
    urli <- "https://www.randonneurs.fi/live/ruska2022/all.kml"
    layers <- st_layers(urli)
    tail <- st_read(urli, layer = layers$name) %>% st_zm()
    return(tail)
  })
  

  output$map <- renderLeaflet({

    dat <- get_data()

    base0 <- leaflet()
    base0 %>% 
      addCircleMarkers(data = dat,
                                        opacity = 0,
                                        fillOpacity = 0) %>%
      addTiles(
        urlTemplate = 
          "http://tiles.kartat.kapsi.fi/peruskartta/{z}/{x}/{y}.jpg",
        options = tileOptions(opacity = .4),
        group = "peruskartta") %>%
      addTiles(urlTemplate = "https://opencache.statkart.no/gatekeeper/gk/gk.open_gmaps?layers=topo4&zoom={z}&x={x}&y={y}",
               options = tileOptions(opacity = .7),
               group = "Norjan maastokartta") %>%
      leaflet.extras::addFullscreenControl()
  })
  
  observe({
    
    dat <- get_data()
    labs <- sprintf(
      "%s",
      dat$labName
      ) %>%
        lapply(htmltools::HTML)
    
    dat1 <- cbind(dat,st_coordinates(dat))
      
      popups <- sprintf(
        "%s<br/>
        <a target = '_blank' href='https://maps.google.com/?q=%s,%s'>Avaa GMapsissa</a><br/>",
        dat1$kuvaus,
        dat1$Y,
        dat1$X
      ) %>%
        lapply(htmltools::HTML)

    base0 <-  leafletProxy("map") %>% 
      clearGroup("riders") %>%
      clearGroup("parcours") %>%
      clearGroup("points") %>% 
      clearGroup("tail")
    if (input$input_show_tail){
      tail <- get_tail()
      base0 <- base0 %>% 
        addPolylines(data = tail, opacity = .5, group = "tail")
    }
    
    
    base0 <- base0 %>%
      addMarkers(data = controls, label = ~name_fi, group = "points", labelOptions = labelOptions(noHide = TRUE,
                                                                                           style = list("font-family" = "Space Mono",
                                                                                                        "font-size" = "1.2em",
                                                                                                        "line-height" = "1",
                                                                                                        "font-weight" = "700",
                                                                                                        "background-color" = "rgba(0,0,0,0)",
                                                                                                        "border-color" = "rgba(0,0,0,0)"))) %>%
      addPolylines(data = parcours, group = "parcous", color = "magenta", opacity = .6) %>%
      addCircleMarkers(data = dat, label = ~labs, 
                       popup = popups,
                 group = "riders", 
                 labelOptions = labelOptions(noHide = TRUE,
                                                               style = list("font-family" = "Space Mono",
                                                                                                "font-size" = "1em",
                                                                                                "line-height" = "1",
                                                                                                # "font-weight" = "700",
                                                                                                "background-color" = "rgba(255,255,255,0.3)",
                                                                                                "border-color" = "rgba(0,0,0,0)")),
                 popupOptions = popupOptions(closeButton = TRUE,className = "popup"
                 )
    )
      
    base0

  })

}

shinyApp(ui = ui, server = server)
