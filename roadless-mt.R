library(mapview)
library(sf)
library(magrittr)
library(leaflet)
library(leafem)
library(htmltools)

rmf <- 
  sf::st_layers("/vsizip/data/RMF_Boundaries_071211.zip") %$%
  name %>%
  purrr::map_dfr(
    \(x) sf::read_sf("/vsizip/data/RMF_Boundaries_071211.zip", layer = x)
  ) %>%
  sf::st_union() %>%
  sf::st_transform(mcor::mt_state_plane) %>%
  sf::st_buffer(20) %>%
  sf::st_buffer(-20)

pad_mt <-
  sf::read_sf("data/PADUS4_1_StateMT.gdb", 
              layer = "PADUS4_1Designation_State_MT") %>%
  dplyr::select(Mang_Type, Mang_Name, Unit_Nm, d_Des_Tp) %>%
  sf::st_transform(mcor::mt_state_plane) %>%
  sf::st_cast("MULTIPOLYGON") %>%
  sf::st_cast("POLYGON") %>%
  dplyr::filter(sf::st_intersects(Shape, sf::st_buffer(rmf, -200), sparse = FALSE)[,1]) %>%
  sf::st_intersection(sf::st_buffer(rmf, -5)) %>%
  sf::st_cast("MULTIPOLYGON") %>%
  sf::st_cast("POLYGON") %>%
  dplyr::mutate(area = sf::st_area(Shape)) %>%
  dplyr::filter(area > units::set_units(15000, "m^2")) %>%
  dplyr::group_by(Name = Unit_Nm, Type = d_Des_Tp) %>%
  dplyr::summarise(.groups = "drop") %>%
  sf::st_cast("MULTIPOLYGON") %>%
  sf::st_transform(4326) %>%
  dplyr::group_by(Type) %>%
  {
    magrittr::set_names(dplyr::group_split(.),
                        dplyr::group_keys(.)$Type
    )
    
  } %>%
  magrittr::extract(
    c(
      "Inventoried Roadless Area",
      "Wilderness Area",
      "Conservation Area",
      "Recreation Management Area", 
      "Research Natural Area"
    )
  )

rmf %<>%
  sf::st_transform(4326)

myColors <- 
  purrr::map(1:length(pad_mt),
             \(x){
               c(
                 "#88AADD",  # Pastel Blue
                 "#DD8899",  # Pastel Coral
                 "#88CCAA",  # Pastel Green
                 "#DDCC77",  # Pastel Mustard
                 "#CC99BB"   # Pastel Lilac
               )[[x]]
             })

purrr::walk(1:length(pad_mt),
            \(i){
              names(pad_mt)[[i]] <<-
                paste0("<span style='color:",myColors[[i]],"; font-weight:bold'>",names(pad_mt)[[i]],"</span>")
            })

myMap <-
  leaflet(options = leafletOptions(minZoom = 9, maxZoom = 15), height = "100%") %>% 
  addFeatures(data = sf::st_transform(rmf, 4326), 
              layerId = "Rocky Mountain Front",
              color = "black",
              fillColor = "white",
              weight = 4,
              opacity = 1)

purrr::walk(1:length(pad_mt),
            \(i){
              myMap <<- addFeatures(myMap,
                                    data = pad_mt[[i]], 
                                    # layerId = names(pad_mt)[[i]],
                                    # group = names(pad_mt)[[i]],
                                    group = names(pad_mt)[[i]],
                                    color = myColors[[i]],
                                    popup = pad_mt[[i]]$Name,
                                    weight = 2,
                                    opacity = 0.75)
            })

myMap %<>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron (minimal)") %>%
  addProviderTiles(providers$USGS.USTopo, group = "USGS Topo") %>%
  addProviderTiles(providers$USGS.USImageryTopo, group = "USGS Satellite") %>%
  # Layers control
  addLayersControl(
    baseGroups = c(
      "Minimal",
      "USGS Topo",
      "USGS Satellite"
    ),
    overlayGroups = 
      names(pad_mt),
    options = layersControlOptions(collapsed = FALSE)
  )

purrr::walk(2:length(pad_mt),
            \(i){
              myMap <<- hideGroup(myMap,
                                  names(pad_mt)[[i]])
            })

# CSS for full-page layout + custom layer title
style_block <- tags$style(HTML("
  html, body, #map {
    height: 100vh;
    margin: 0;
    padding: 0;
  }
  .leaflet-container {
    height: 100vh;
    width: 100vw;
  }
  .leaflet-control-layers-list::before {
    content: 'Rocky Mountain Front\\AInventoried Roadless Areas';
    white-space: pre;
    display: block;
    font-weight: bold;
    margin-bottom: 4px;
    font-size: 14px;
  }
"))

html_page <- tagList(
  tags$head(
    tags$title("Wild Montana — Roadless Areas"),
    style_block
  ),
  tags$body(
    myMap
  )
)
                

htmltools::save_html(html_page, 
                     file = "docs/index.html")
