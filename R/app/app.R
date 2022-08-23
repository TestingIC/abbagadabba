library('shiny')
library('abbagadabba')
library('shinyjs')
library("taxize")
library("rgbif")
library("rentrez")
library("DT")
#library("shinyTree")

names = "WHYYY"

#Functions to help organize UI

navbar <- function() {
    navbarPage("Pages", namesPanel(), metadataPanel())
}

javascriptMap <- function() {
    tags$div(HTML("
        <link rel='stylesheet' href='https://unpkg.com/leaflet@1.7.1/dist/leaflet.css' integrity='sha512-xodZBNTC5n17Xt2atTPuE1HxjVMSvLVW9ocqUKLsCC5CXdbqCmblAshOMAS6/keqq/sMZMZ19scR4PsZChSR7A==' crossorigin='' />
        <script src='https://unpkg.com/leaflet@1.7.1/dist/leaflet.js' integrity='sha512-XQoYMqMTK8LvdxXYG3nZ448hOEQiglfqkJs1NOQV44cWnUrBc8PkAOcXy20w0vlaXaVUearIOBhiXZ5V3ynxwA==' crossorigin=''></script>
        <link rel='stylesheet' href='https://unpkg.com/@geoman-io/leaflet-geoman-free@latest/dist/leaflet-geoman.css'/>
        <script src='https://unpkg.com/@geoman-io/leaflet-geoman-free@latest/dist/leaflet-geoman.min.js'></script>"))
}

namesPanel <- function() {
    tabPanel("Occurences",
             fluidRow #Everything
             (
                 column
                 (12,
                     fluidRow #Scientific Name
                     (
                         titlePanel("Scientific Name"),

                         column #Input
                         (3,
                             wellPanel
                             (
                                 textInput("ScientificNameTextInput", "Name"),
                             )
                         ),
                         column #Output
                         (9,
                             navbarPage
                             ("",
                                 tabPanel
                                 ("New Names",
                                     fluidRow
                                     (
                                         DTOutput('ScientificNamesTable')
                                     )
                                 ),
                                 tabPanel
                                 ("Selected Names",
                                     fluidRow
                                     (
                                         DTOutput('SelectedScientificNamesTable')
                                     )

                                 )
                             ),
                         )
                     )
                 )
             ),

             fluidRow
             (
                 column
                 (width=12,

                     titlePanel("Map"),

                     fluidRow
                     (id="NameMapRow",
                         column
                         (width=12,
                             wellPanel
                             (

                                 tags$div(HTML("<div id='map' style='height:300px;'></div>")),
                                 includeScript(path = "./map.js")
                             )
                         )
                     ),

                     hr(),

                     titlePanel("Occurences"),

                     fluidRow
                     (
                         column
                         (width=3,
                             wellPanel
                             (
                                 actionButton("submitOccurences", "Get Occurences"),
                                 textOutput("GetOccurencesCommandText")
                             )
                         ),
                         column
                         (width=9,
                             wellPanel
                             (
                                 #DTOutput('NamesTableOutput')
                             )
                         )
                     )
                 )
             )
    )
}

metadataPanel <- function() {
    tabPanel("Get Metadata", fluid = TRUE,
             fluidRow(
                 column(6, selectInput("name_for_metadata", "Get meta data for:", c())),
                 column(6, actionButton("get_metadata_button", "Get metadata"))
             ),
             fluidRow(
                 column(6, numericInput("page_for_metadata", "Page", value=0, min=0)),
                 column(6, numericInput("limit_for_metadata_retrieval", "How many ids should be returned?", value=5, min=1))
             ),
             DTOutput('IDsTableOutput'),
             fluidRow(column(12, tableOutput("sequences_output"))),
    )
}

showNames <- function(input, output, session) {
    if (!is.null(input$polygon)) {
        names <<- rgbif::occ_data(taxonKey = taxonKeys, geometry = input$polygon)$data[c("key", "scientificName")]
        #names <<- unique(occ_data(taxonKey = taxonKeys, geometry = input$polygon)$data$scientificName)
    }
    else {
        names <<- rgbif::occ_data(taxonKey = taxonKeys)$data[c("key", "scientificName")]
    }

    updateSelectInput(session, "name_for_metadata", "Get meta data for:", names$scientificName)
    output$"NamesTableOutput" <- renderDT(names)

}

showMetadata <- function(input, output, session) {
    if (input$"name_for_metadata" != "" & !is.na(input$"page_for_metadata")) {
        withProgress(message = "Retrieving metadata", value = 0, {
            Sys.sleep(1)
            incProgress(1/5, detail = "Retrieving names")
            clean_names <- getNCBITaxonomy(input$"name_for_metadata")
            incProgress(2/5, detail = "Got names! Now cleaning them")
            good_names <- clean_names$ncbi_name[!is.na(clean_names$ncbi_name)] #Don't cut out na names
            incProgress(3/5, detail = "Cleaned names! Now retrieving IDS")
            seq_ids <- getNCBISeqID(good_names)
            incProgress(4/5, detail = "Got IDs! Now getting metadata")

            metadata <- getMetadata(seq_ids[((input$"page_for_metadata"*input$"limit_for_metadata_retrieval")+1):((input$"page_for_metadata"*input$"limit_for_metadata_retrieval")+input$"limit_for_metadata_retrieval")])
            #output$sequences_output <- renderTable(metadata)

            output$"IDsTableOutput" <- renderDT(metadata)

            incProgress(5/5, detail = "Got metadata!")
        })
    }
}



ui <- fluidPage(title = "Abbagadabba Visualization",
    javascriptMap(),

    useShinyjs(),
    tags$div(style="width: 100%; height: 400px; black; overflow-x: auto; overflow-y: auto; border: 3px solid black; border-radius: 10px; margin-bottom: 20px;", navbar()),
    tags$div(style="width: 100%; height: 350px; overflow-x: auto; overflow-y: auto; border: 3px solid black; border-radius: 10px;", DTOutput('NamesTableOutput'))
)

taxonKeys <- NULL

server <- function(input, output, session) {
    observe({
        x <- name_suggest(input$"ScientificNameTextInput", fields = c("key", "canonicalName", "rank","higherClassificationMap"))
        ScientificNamesTable <- x$data
        ScientificNamesTable <- ScientificNamesTable[order(ScientificNamesTable$key), ]

        h <- x$hierarchy

        outS <- sapply(h, function(y) {
            paste(y$name, collapse = ' > ')
        })

        names(outS) <- NULL

        ScientificNamesTable$taxonomic_hierarcy <- outS
        output$"ScientificNamesTable" <- renderDT(ScientificNamesTable)
    })

    observeEvent(input$ScientificNamesTable_rows_selected, {
        SelectedScientificNamesDataFrame <<- data.frame("key" = name_suggest(input$"ScientificNameTextInput")$data$key[input$ScientificNamesTable_rows_selected], "canonicalName" = name_suggest(input$"ScientificNameTextInput")$data$canonicalName[input$ScientificNamesTable_rows_selected], "rank" = name_suggest(input$"ScientificNameTextInput")$data$rank[input$ScientificNamesTable_rows_selected])
        output$"SelectedScientificNamesTable" <- renderDT(SelectedScientificNamesDataFrame)
        taxonKeys <<- name_suggest(input$"ScientificNameTextInput")$data$key[input$ScientificNamesTable_rows_selected]
    })

    #Append taxonomic hierarchy using rgbif::name_ussage

    observeEvent(input$"submitOccurences", {
        showNames(input, output, session)
    })

    observeEvent(input$"get_metadata_button", {
        showMetadata(input, output, session)
    })
}

shinyApp(ui = ui, server = server)



