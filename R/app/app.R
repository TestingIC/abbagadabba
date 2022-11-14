library('shiny')
library('abbagadabba')
library("rgbif")
library("rentrez")
library("reactable")
library("dplyr")

#Functions to help organize UI

navbar <- function() {
    navbarPage("Abbagadabba", occurencesPanel(), sequencesPanel(), allDataPanel())
}

occurencesPanel <- function() {
    tabPanel(title = "Occurences", fluidRow(
        column(2, "Filters", style = "background-color: blue; height: 88vh; overflow: scroll;"),
        column(10, "Data", style = "background-color: red; height: 88vh; overflow: scroll;")
    ))
}

sequencesPanel <- function() {
    tabPanel(title = "Sequences")
}

allDataPanel <- function() {
    tabPanel(title = "All Data",
    fluidRow(column(6, actionButton(inputId = "GetAllDataButton", label = "Get All Data"))),
    fluidRow(column(12, reactableOutput("AllDataTable")))
    )
}

ui <- fluidPage(title = "Abbagadabba Visualization", navbar(),

    )

server <- function(input, output, session) {
    observeEvent(input$GetAllDataButton, {
        occurences <- occ_data(limit=100)$data[c("scientificName", "year", "month", "occurrenceStatus", "basisOfRecord", "datasetKey")]

        duplicate_occurences <- occurences[duplicated(occurences$scientificName) | duplicated(occurences$scientificName, fromLast = TRUE), ]
        duplicate_occurence_names <- unique(duplicate_occurences$scientificName)

        unique_occurences <- occurences[!duplicated(occurences$scientificName), ]
        unique_occurences[unique_occurences$scientificName %in% duplicate_occurence_names, ][c("year", "month", "occurrenceStatus", "basisOfRecord", "datasetKey")] <- list(0, 0, "Dup", "Dup", "Dup")

        output$AllDataTable <- renderReactable({reactable(unique_occurences, filterable = TRUE, details = function(index) {
            if (unique_occurences[index, ]$scientificName %in% duplicate_occurence_names) {
                htmltools::div(style = "padding: 1rem", reactable(duplicate_occurences[duplicate_occurences$scientificName == unique_occurences[index, ]$scientificName, ], outlined = TRUE))
            }
        })})
    })
}

shinyApp(ui = ui, server = server)
