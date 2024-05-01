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
source("modals.R")
default_date_format <- "%Y-%m-%d"
display_table_cols <- list(
  invisible = c(
    "GU Sample ID", "GU Aliquot ID", "Aliquot ID", "NRDA ID"
  ),
  w250 = c("Species", "Common Name"),
  w150 = c("Accession Date", "Tissue Type", "Container Type"),
  w100 = c("State", "Country", "City Island Comm", "Island", "Field ID", "Colony Name"),
  w075 = c("Project ID", "Sub Project", "Sex")
)

# Data ----
# Data are kept recent every Saturday by a scheduled task
aliquots      <- feather::read_feather(file.path("data", "Aliquots.feather"))
data_updated  <- file.info(file.path("data", "Aliquots.feather"))$mtime
udfs_aliquots <- names(aliquots)
samples       <- feather::read_feather(file.path("data", "Samples.feather"))
udfs_samples  <- names(samples)
udfs_all      <- sort(c(udfs_aliquots, udfs_samples))
udfs_dates    <- udfs_all[grep("date", tolower(udfs_all))]
udfs_samples  <- sort(udfs_samples[-grep("[fp]k|uid", tolower(udfs_samples))])
udfs_aliquots <- sort(udfs_aliquots[-grep("[fp]k|uid", tolower(udfs_aliquots))])
to_factor     <- c(
  "SPECIES", "COMMON_NAME", "TISSUE_TYPE", "CONTAINER_TYPE", "CITY", "COUNTRY",
  "PROJECT_ID", "SUB_PROJECT", "COLLECTION_NAME", "COLLECTION_TYPE", "region",
  "SEX"
)

NBR_df        <- aliquots |>
  filter(!POSITION1 == "") |>
  left_join(
    samples,
    by = c("FK_SAMPLEUID" = "PK_SAMPLEUID")
  ) |>
  relocate(
    PROJECT_ID,
    SUB_PROJECT,
    GUSAMPLEID,
    GUALIQUOTID,
    SPECIES,
    COMMON_NAME,
    SEX,
    TISSUE_TYPE,
    CONTAINER_TYPE,
    .before = everything()
  ) |>
  mutate(region = tolower(state.name[match(STATE, state.abb)]),
         region = str_to_title(region),
         COMMON_NAME = COMMON_NAME |>
           str_replace_all(" Seal", " seal") |>
           str_replace_all(" Whale", " whale"),
         SEX = str_to_title(SEX),
         TISSUE_TYPE = str_to_title(TISSUE_TYPE),
         AGE_CLASS = str_to_title(AGE_CLASS),
         COLLECTION_YEAR = ifelse(
           is.na(COLLECTION_YEAR) | COLLECTION_YEAR == 0,
           year(min(DATE_COLLECTED, DATE_IN, DATE_OUT, na.rm = T)),
           COLLECTION_YEAR)
  ) |>
  mutate(COLLECTION_YEAR = as_date(sprintf("%s-12-31", COLLECTION_YEAR))) |>
  mutate(across(where(is.character), \(x) ifelse(x == "", NA, x))) |>
  relocate(any_of(c("region", "COUNTRY", "DATE_IN")), .after = TISSUE_TYPE) |>
  select(-any_of(c("STATE", "DATE_OUT")))
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

# Styles ----
# css <- readLines("www/style.css")

# UI ----
ui <- page_sidebar(
  theme = bs_add_rules(bs_theme(version = 5), sass::sass_file("www/style.scss")),
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  # ),
  lang = "en",
  fillable = TRUE,
  fillable_mobile = TRUE,
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
    tags$span(
      class = "sidebar_bottom_left",
      actionButton(inputId = "about", label = "About", width = "100%"),
      p(id = "data_as_of", sprintf("Data last updated %s.", format(data_updated, '%Y-%m-%d')))
    ),
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
      leafletOutput(outputId = "map"),
      tags$span(
        style = "display: inline",
        class = "map_controls",
        input_switch(id = "map_type", label = "View as Choropleth", value = FALSE)
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
    timeline_data() # |>
    # mutate(across(everything(), \(x) ifelse(x == "", NA, x))) |>
    # select(where(\(x) !all(is.na(x))))
    # }
  })
  selected_data_clean_names <- reactive({
    selected_data() |>
      mutate(COLLECTION_YEAR = lubridate::year(COLLECTION_YEAR)) |>
      rename_with(str_replace_all, pattern = "_", replacement = " ") |>
      rename_with(str_to_title) |>
      rename_with(str_replace_all, pattern = "[Ii]d$", replacement = "ID") |>
      rename_with(str_replace_all, pattern = "Nrda", replacement = "NRDA") |>
      rename_with(str_replace_all, pattern = "Nist", replacement = "NIST") |>
      rename_with(str_replace_all, pattern = "Mesb", replacement = "MESB") |>
      rename_with(str_replace_all, pattern = "Sfei", replacement = "SFEI") |>
      rename(any_of(
        c(
          "GU Sample ID" = "GusampleID",
          "GU Aliquot ID" = "GualiquotID",
          "Current Amount" = "Currentamount",
          "Number of Thaws" = "Numberofthaws",
          "Sample Type" = "Sampletype",
          "State" = "Region",
          "Accession Date" = "Date In"
        )
      )) |>
      select(-matches("^[PFpf][Kk]"), -Position1) |>
      mutate(
        across(
          where(is.character),
          \(x) ifelse(x == "", NA, x)
        )
      )
  })
  table_data <- reactive(trim_table_cols(selected_data_clean_names(), display_table_cols))
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
  ## Map ----
  observeEvent(input$map_shape_click, {
    shape_click_id <- input$map_shape_click$id
    shape_click_id <- gsub("_line|_circle", "", shape_click_id)
    if (!shape_click_id == "HML") {
      state_data <- isolate(selected_data()) %>%
        filter(region == shape_click_id) %>%
        count(SPECIES, COMMON_NAME, AGE_CLASS, SEX, TISSUE_TYPE) %>%
        mutate(SPECIES = paste0("(", SPECIES, ")")) %>%
        unite(Species, COMMON_NAME, SPECIES, sep = " ") %>%
        arrange_all() %>%
        mutate_if(is.character, as.factor) %>%
        setNames(c("Species", "Age Class", "Sex", "Tissue Type", "n"))
      output$modal_state_info_dt <- renderDT(
        datatable(state_data,
                  rownames = FALSE,
                  escape = FALSE,
                  filter = "top",
                  options = list(autoWidth = TRUE,
                                 columnDefs = list(list(width = "40%", targets = 0)))
        )
      )
      showModal(modal_state_info(sum(state_data$n), shape_click_id, n_distinct(state_data$Species)))
    }
  })
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
  ## Narrative ----
  output$narrative <- renderText("[Placeholder for short narrative of some sort.]")
  ## Data Table ----
  output$table <- renderDT(
    server = TRUE,
    expr = datatable(
      req(table_data()$df),
      extension = c('Buttons', 'ColReorder'),
      rownames = FALSE,
      filter = "top",
      fillContainer = TRUE,
      options = list(
        dom = "Bfrtip",
        autoWidth = TRUE,
        colReorder = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = table_data()$cols$invisible),
          list(width = "250px", targets = table_data()$cols$w250),
          list(width = "100px", targets = table_data()$cols$w100),
          list(width = "150px", targets = table_data()$cols$w150),
          list(width = "75px", targets = table_data()$cols$w075),
          list(width = "50px", targets = "_all")
        ),
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
