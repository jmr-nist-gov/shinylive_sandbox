# Libraries ----
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
# library(ggplot2)
library(leaflet)
library(DT)
library(lubridate)
# library(dygraphs)
library(geojsonio)
library(geosphere)

# Functions and presets ----
source("app_functions.R")
default_date_format <- "%Y-%m-%d"

# Data ----
# Data are kept recent every Saturday by a scheduled task
aliquots      <- feather::read_feather("data/Aliquots.feather")
udfs_aliquots <- names(aliquots)
samples       <- feather::read_feather("data/Samples.feather")
udfs_samples  <- names(samples)
udfs_all      <- sort(c(udfs_aliquots, udfs_samples))
udfs_dates    <- udfs_all[grep("date", tolower(udfs_all))]
udfs_samples  <- sort(udfs_samples[-grep("[fp]k|uid", tolower(udfs_samples))])
udfs_aliquots <- sort(udfs_aliquots[-grep("[fp]k|uid", tolower(udfs_aliquots))])
NBR_df        <- aliquots |>
  filter(!POSITION1 == "") |>
  left_join(
    samples,
    by = c("FK_SAMPLEUID" = "PK_SAMPLEUID")
  ) |>
  relocate(
    GUSAMPLEID,
    GUALIQUOTID,
    SPECIES,
    COMMON_NAME,
    TISSUE_TYPE,
    CONTAINER_TYPE,
    .before = everything()
  ) |>
  mutate(region = tolower(state.name[match(STATE, state.abb)]),
         region = str_to_title(region),
         COLLECTION_YEAR = ifelse(
           is.na(COLLECTION_YEAR) | COLLECTION_YEAR == 0,
           year(min(DATE_COLLECTED, DATE_IN, DATE_OUT, na.rm = T)),
           COLLECTION_YEAR)
  ) |>
  mutate(COLLECTION_YEAR = as_date(sprintf("%s-12-31", COLLECTION_YEAR))) |>
  select(-STATE) |>
  mutate(
    across(
      any_of(c("SPECIES", "COMMON_NAME", "TISSUE_TYPE", "CONTAINER_TYPE", "CITY", "COUNTRY")),
      as.factor
    ),
    across(
      starts_with("AGE"),
      as.factor
    )
  )
list_projects <- sort(unique(NBR_df$PROJECT_ID))
list_subprojects <- sort(unique(NBR_df$SUB_PROJECT))

base_map      <- create_us_map(center_long = -120, center_lat = 50, start_zoom = 3)
states        <- geojson_read(file.path("data", "us-states.js"), what = "sp")
states$xy     <- as.data.frame(
  t(
    data.frame(
      lapply(
        states@polygons,
        function(x) {
          return(x@labpt)
        })))) |>
  setNames(c("orig.long", "orig.lat")) |>
  mutate(region = as.character(states$name))

# UI ----
ui <- page_sidebar(
  tags$head(
    tags$style(
      "

      "
    )
  ),
  lang = "en",
  window_title = "NIST Biorepository Collection Browser",
  # title = tags$span(
  #   tags$a(
  #     href = "https://www.nist.gov",
  #     img(id = "nist_logo", src = "NIST-Logo-Brand-White.svg")
  #   ),
  #   tags$a(
  #     href = "https://www.nist.gov/programs-projects/nist-biorepository",
  #     "Biorepository Collection Browser"
  #   )
  # ),
  title = "NIST Biorepository Collection Browser",
  sidebar = sidebar(
    actionButton(inputId = "browse", label = "Inspect", icon = icon("user-ninja")),
    selectizeInput(
      inputId = "select_project",
      label = "Project ID",
      choices = list_projects,
      multiple = TRUE
    ),
    selectizeInput(
      inputId = "select_sub_project",
      label = "Sub Project ID",
      choices = list_subprojects,
      multiple = TRUE
    ),
    textOutput(outputId = "narrative")
  ),
  layout_columns(
    fillable = TRUE,
    gap = '5px',
    tags$span(
      height = "100%",
      leafletOutput(outputId = "map", height = "47vh"),
      tags$span(
        style = "display: inline",
        class = "map_controls",
        input_switch(id = "map_type", label = "Map Type", value = FALSE),
        textOutput(outputId = "map_type_display")
      )
    ),
    # dygraphOutput(outputId = "timeline"),
    DTOutput(outputId = "table"),
    col_widths = c(12, 12),
    row_heights = c(6, 6)
  )
)

# Server ----
server <- function(input, output, session) {
  
  # Data reactives ----
  selected_projects      <- reactive(input$select_project)
  selected_subprojects   <- reactive(input$select_sub_project)
  applicable_projects    <- reactiveVal(NULL)
  applicable_subprojects <- reactiveVal(NULL)
  ## Filter for timeline_data() ----
  # An interactive timeline (via dygraphs) will eventually be provided for temporal filtering, but isn't yet implemented.
  timeline_data <- reactive({
    # Filter on subprojects selected
    if (!is.null(selected_subprojects())) {
      NBR_df |> filter(SUB_PROJECT %in% selected_subprojects())
      # Filter on projects selected
    } else if (!is.null(selected_projects())) {
      NBR_df |> filter(PROJECT_ID %in% selected_projects())
      # In case of null selection, include all
    } else {
      NBR_df
    }
  })
  ## Filter for selected_data() ----
  selected_data <- reactive({
    # if (!is.null(time_range())) {
    #   timeline_data() |>
    #     filter_at(.vars = timeline_choices,
    #               .vars_predicate = any_vars(between(.,
    #                                                  min(time_range()),
    #                                                  max(time_range()))
    #               )
    #     ) |>
    #     select(where(\(x) !all(is.na(x))))
    # } else {
    timeline_data() |>
      select(where(\(x) !all(is.na(x))))
    # }
  })
  selected_data_clean_names <- reactive({
    selected_data() |>
      rename_with(str_replace_all, pattern = "_", replacement = " ") |>
      rename_with(str_to_title) |>
      rename_with(str_replace_all, pattern = "id$", replacement = "ID") |>
      rename_with(str_replace_all, pattern = "Nrda", replacement = "NRDA") |>
      rename_with(str_replace_all, pattern = "Nist", replacement = "NIST") |>
      rename_with(str_replace_all, pattern = "Mesb", replacement = "MESB") |>
      rename_with(str_replace_all, pattern = "Sfei", replacement = "SFEI") |>
      rename(
        c(
          "GU Sample ID" = "GusampleID",
          "GU Aliquot ID" = "GualiquotID",
          "Current Amount" = "Currentamount",
          "Number of Thaws" = "Numberofthaws",
          "Sample Type" = "Sampletype"
        )
      ) |>
      select(-contains("^[PFpf][Kk]"))
  })
  # time_range_raw         <- reactive({
  #   if (is.null(input$overview_timeline_date_window)) {
  #     NULL
  #   } else {
  #     as.Date(input$overview_timeline_date_window)
  #   }
  # })
  # time_range             <- time_range_raw |> debounce(250)
  # am_busy                <- reactiveVal(FALSE)
  
  ## Fill timeline options (inactive) ----
  # timeline_choices <- names(NBR_df)[grep("DATE|COLLECT", names(NBR_df))]
  # updateSelectizeInput(session,
  #                      inputId = 'select_timeline_columns',
  #                      choices = timeline_choices,
  #                      selected = c("DATE_IN", "DATE_OUT"))
  
  ## Mapping layer ----
  leaflet_df <- reactive({
    selected_data() |>
      filter(!is.na(region)) |>
      group_by(region) |>
      summarise(n_samples = n_distinct(FK_SAMPLEUID),
                n_aliquots = n_distinct(PK_ALIQUOTUID))
  })
  
  # Observers ----
  ## Live inspection ----
  observeEvent(input$browse, browser())
  ## Project / SubProject selection ----
  observe({
    updateSelectizeInput(session, 'select_project',
                         choices = applicable_projects(),
                         selected = isolate(selected_projects()))
  })
  observe({
    updateSelectizeInput(session, 'select_sub_project',
                         choices = applicable_subprojects(),
                         selected = isolate(selected_subprojects()))
  })
  observeEvent(input$select_project, ignoreNULL = FALSE, {
    if (is.null(selected_projects())) {
      a_subpro <- list_subprojects
    } else {
      a_subpro <- NBR_df |>
        filter(PROJECT_ID %in% selected_projects()) |>
        pull(SUB_PROJECT) |>
        unique() |>
        sort()
    }
    if (!is.null(selected_subprojects())) a_subpro <- unique(c(selected_subprojects(), a_subpro))
    applicable_subprojects(a_subpro)
  })
  observeEvent(input$select_sub_project, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (is.null(selected_subprojects())) {
      a_projct <- list_projects
    } else {
      a_projct <- NBR_df |>
        filter(SUB_PROJECT %in% selected_subprojects()) |>
        pull(PROJECT_ID) |>
        unique() |>
        sort()
    }
    if (!is.null(selected_projects())) a_projct <- unique(c(selected_projects(), a_projct))
    applicable_projects(a_projct)
  })
  ## Map proxy update ----
  observe({
    # Clear the map
    leafletProxy("map") |>
      clearShapes() |>
      clearControls() |>
      clearMarkers()
    if (nrow(leaflet_df()) > 0) {
      map_data <- leaflet_df()
      # Generate labels (used in both map types)
      labels <- purrr::pmap(.l = map_data,
                            .f = ~HTML(sprintf('<p class = "map_hover_info"><strong>%s</strong><br/>%s samples<br/>%s aliquots<br/><small><em>Click to explore</em></small></p>',
                                               ..1, scales::comma(..2), scales::comma(..3))))
      if (input$map_type) {
        # Data massage
        bins <- rev(c(0, 250, 500, 1000, 2500, 5000, 10000, 20000, 50000))
        pal  <- colorBin("viridis", domain = map_data$n_aliquots, bins = bins)
        states_to_show        <- which(states$name %in% map_data$region)
        map_polygons          <- states
        map_polygons@data     <- map_polygons@data[states_to_show, ]
        map_polygons@polygons <- map_polygons@polygons[states_to_show]
        map_polygons$counts   <- map_data$n_aliquots
        map_polygons$labels   <- labels
        
        # Produce map via proxy
        leafletProxy('map') |>
          addPolygons(data = map_polygons,
                      layerId = ~name,
                      group = "polygons",
                      fillColor = ~pal(counts),
                      weight = 1,
                      opacity = 1,
                      color = 'white',
                      dashArray = "",
                      fillOpacity = 0.5,
                      highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 1,
                        bringToFront = T),
                      label = ~labels,
                      labelOptions = labelOptions(
                        direction = "auto")
          ) |>
          leaflet::addLegend(data = map_polygons,
                             group = "legend",
                             pal = pal,
                             values = ~counts,
                             opacity = 1,
                             title = "Aliquots",
                             position = "bottomright")
      } else {
        # Data massage
        HML_lat  <- 32.748928
        HML_long <- -79.901112
        map_data <- inner_join(map_data, states$xy, by = "region") |>
          mutate(dest.long = HML_long,
                 dest.lat = HML_lat,
                 labels = labels)
        map_polylines              <- gcIntermediate(map_data[, 4:5], map_data[, 6:7], sp = TRUE, addStartEnd = TRUE)
        map_polylines$counts       <- map_data$n_aliquots
        map_polylines$origins      <- map_data$region
        map_polylines$destinations <- "NIST Biorepository"
        map_polylines$labels       <- labels
        # Produce map via proxy
        leafletProxy('map') |>
          addPolylines(data = map_polylines,
                       layerId = ~paste0(origins, "_line"),
                       group = "polylines",
                       weight = 5,
                       label = ~labels,
                       color = 'green') |>
          addCircles(data = map_data,
                     layerId = ~paste0(region, "_circle"),
                     group = "polylines",
                     lng = ~orig.long,
                     lat = ~orig.lat,
                     radius = ~(n_aliquots*10),
                     label = ~labels,
                     color = 'green',
                     opacity = 1,
                     fillColor = 'white',
                     fillOpacity = 1) |>
          addRectangles(layerId = "HML",
                        lng1 = HML_long * 0.997,
                        lng2 =  HML_long * 1.003,
                        lat1 = HML_lat * 0.995,
                        lat2 = HML_lat * 1.005,
                        color = 'blue',
                        opacity = 1,
                        fillColor = 'white',
                        fillOpacity = 1,
                        label = HTML("<strong>NIST Biorepository</strong><br/>Charleston, SC"),
                        labelOptions = labelOptions(direction = 'right'))
      }
    } else {
      leafletProxy('map') |>
        addLabelOnlyMarkers(lng = -98.5795,
                            lat = 39.8283,
                            label = HTML("<strong>No geographic information available.</strong>"),
                            labelOptions = labelOptions(noHide = TRUE,
                                                        direction = "bottom",
                                                        textOnly = TRUE))
    }
  })
  
  # Outputs ----
  ## Map ----
  output$map <- renderLeaflet(base_map)
  output$map_type_display <- renderText(ifelse(req(input$map_type), "Choropleth", "Bubble"))
  ## Narrative ----
  output$narrative <- renderText("[Placeholder for short narrative of some sort.]")
  ## Data Table ----
  output$table <- renderDT(
    server = TRUE,
    expr = datatable(
      req(selected_data_clean_names()) |>
        select(-starts_with("FK"), -starts_with("PK")),
      extension = c('Buttons', 'ColReorder'),
      rownames = FALSE,
      filter = "top",
      fillContainer = TRUE,
      options = list(
        dom = "Bfrtip",
        colReorder = TRUE,
        buttons = list(
          list(extend = "csv", text = "Export to CSV", exportOptions = list(modifier = list(page = "all"))),
          "colvis"
        )
      )
    ) |>
      formatStyle('Species', `font-style` = "italic")
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
