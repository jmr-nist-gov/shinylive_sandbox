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

# Data ----
# Data are kept recent every Saturday by a scheduled task
aliquots      <- feather::read_feather("data/Aliquots.feather")
# udfs_aliquots <- names(aliquots)
samples       <- feather::read_feather("data/Samples.feather")
list_projects <- sort(unique(samples$PROJECT_ID))
list_subprojects <- sort(unique(samples$SUB_PROJECT))
# udfs_samples  <- names(samples)
# udfs_all      <- sort(c(udfs_aliquots, udfs_samples))
# udfs_dates    <- udfs_all[grep("date", tolower(udfs_all))]
# udfs_samples  <- sort(udfs_samples[-grep("[fp]k|uid", tolower(udfs_samples))])
# udfs_aliquots <- sort(udfs_aliquots[-grep("[fp]k|uid", tolower(udfs_aliquots))])
NBR_df        <- left_join(
  aliquots,
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
         COLLECTION_YEAR = ifelse(is.na(COLLECTION_YEAR),
                                  year(min(DATE_COLLECTED, DATE_IN, DATE_OUT, na.rm = T)),
                                  COLLECTION_YEAR)) %>%
  mutate(COLLECTION_YEAR = as_date(sprintf("%s-12-31", COLLECTION_YEAR))) %>%
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

# Functions necessary for app logic
source("app_functions.R")
default_date_format <- "%Y-%m-%d"

# UI ----
ui <- page_sidebar(
  tags$head(
    tags$style(
      ".dataTables_filter {display: inline-flex; float: right;};"
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
      choices = samples$PROJECT_ID |>
        unique() |>
        sort(),
      multiple = TRUE
    ),
    selectizeInput(
      inputId = "select_sub_project",
      label = "Sub Project ID",
      choices = samples$SUB_PROJECT |>
        unique() |>
        sort(),
      multiple = TRUE
    ),
    textOutput(outputId = "narrative")
  ),
  layout_columns(
    fillable = TRUE,
    gap = '5px',
    leafletOutput(outputId = "map"),
    # dygraphOutput(outputId = "timeline"),
    DTOutput(outputId = "table"),
    col_widths = c(6, 6),
    row_heights = c(12, 12)
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactives ----
  selected_projects      <- reactive(input$select_project)
  selected_subprojects   <- reactive(input$select_sub_project)
  applicable_projects    <- reactiveVal(NULL)
  applicable_subprojects <- reactiveVal(NULL)
  # Filter for timeline_data ----
  timeline_data <- reactive({
    # Filter on subprojects selected
    if (!is.null(selected_subprojects())) {
      NBR_df %>% filter(SUB_PROJECT %in% selected_subprojects())
      # Filter on projects selected
    } else if (!is.null(selected_projects())) {
      NBR_df %>% filter(PROJECT_ID %in% selected_projects())
      # In case of null selection, include all
    } else {
      NBR_df
    }
  })
  # Filter for selected_data ----
  selected_data <- reactive({
    # if (!is.null(time_range())) {
    #   timeline_data() %>%
    #     filter_at(.vars = timeline_choices,
    #               .vars_predicate = any_vars(between(.,
    #                                                  min(time_range()),
    #                                                  max(time_range()))
    #               )
    #     )
    # } else {
    timeline_data() |>
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
      rename_with(.cols = starts_with("PK")|starts_with("FK"), .fn = str_to_upper) |>
      select(where(\(x) !all(is.na(x))))
    # }
  })
  # time_range_raw         <- reactive({
  #   if (is.null(input$overview_timeline_date_window)) {
  #     NULL
  #   } else {
  #     as.Date(input$overview_timeline_date_window)
  #   }
  # })
  # time_range             <- time_range_raw %>% debounce(250)
  # am_busy                <- reactiveVal(FALSE)
  
  # # Fill timeline options (inactive) ----
  # timeline_choices <- names(NBR_df)[grep("DATE|COLLECT", names(NBR_df))]
  # updateSelectizeInput(session,
  #                      inputId = 'select_timeline_columns',
  #                      choices = timeline_choices,
  #                      selected = c("DATE_IN", "DATE_OUT"))
  
  # Mapping layer ----
  leaflet_df <- reactive({
    selected_data() %>%
      filter(!is.na(region)) %>%
      group_by(region) %>%
      summarise(n_samples = n_distinct(PK_SAMPLEUID),
                n_aliquots = n_distinct(PK_ALIQUOTUID))
  })
  base_map      <- create_us_map(center_long = -120, center_lat = 50, start_zoom = 3)
  states        <- geojson_read(file.path("data", "us-states.js"), what = "sp")
  states$xy     <- as.data.frame(
    t(
      data.frame(
        lapply(
          states@polygons,
          function(x) {
            return(x@labpt)
          })))) %>%
    setNames(c("orig.long", "orig.lat")) %>%
    mutate(region = as.character(states$name))
  
  # Observers ----
  observeEvent(input$browse, browser())
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
      a_subpro <- NBR_df %>%
        filter(PROJECT_ID %in% selected_projects()) %>%
        pull(SUB_PROJECT) %>%
        unique() %>%
        sort()
    }
    if (!is.null(selected_subprojects())) a_subpro <- unique(c(selected_subprojects(), a_subpro))
    applicable_subprojects(a_subpro)
  })
  observeEvent(input$select_sub_project, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (is.null(selected_subprojects())) {
      a_projct <- list_projects
    } else {
      a_projct <- NBR_df %>%
        filter(SUB_PROJECT %in% selected_subprojects()) %>%
        pull(PROJECT_ID) %>%
        unique() %>%
        sort()
    }
    if (!is.null(selected_projects())) a_projct <- unique(c(selected_projects(), a_projct))
    applicable_projects(a_projct)
  })
  
  # Outputs ----
  # output$map <- renderLeaflet()
  output$narrative <- renderText("[Placeholder for short narrative of some sort.]")
  output$table <- renderDT(
    server = TRUE,
    expr = datatable(
      req(selected_data()) |>
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
    ) %>%
      formatStyle('Species', `font-style` = "italic")
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
