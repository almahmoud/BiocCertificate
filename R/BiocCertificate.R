mandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}

appCSS <- paste(
    ".mandatory_star { color: red; }", "#error { color: red; }", sep = "\n"
)

.MANDATORY_DATA_NAMES <- c("eid", "ename", "edate", "elocation", "fullname")

#' @importFrom shinyjs show hide hidden enable disable inlineCSS toggleState
#' @export
BiocCertificate <- function(...) {
    fieldsMandatory <- c("eid", "edate", "elocation", "fullname")
    fieldsAll <- c(
        "eid", "ename", "edate", "elocation", "eurl", "fullname", "address"
    )
    ui <- fluidPage(
        shinyjs::useShinyjs(),
        inlineCSS(appCSS),
        titlePanel(
            windowTitle = "Bioconductor Certificate Form",
            title = div(
                img(
                    src = "images/bioconductor_logo_rgb_small.png",
                    align = "right",
                    style = "margin-right:10px"
                ),
                h1(id = "big-heading", "Certificate of Participation")
            )
        ),
        sidebarLayout(
            div(class = "sidebar",
                sidebarPanel(
                    div(
                        id = "presubmit",
                        textInput(
                            "eid",
                            mandatory("Event ID"),
                            placeholder = "Bioc####"
                        ),
                        actionButton(
                            "presubmit", "Populate", class = "btn-primary"
                        )
                    ),
                    br(),
                    div(
                        id = "form",
                        textInput(
                            "ename",
                            "Event Name",
                            placeholder = "Bioconductor ####"
                        ),
                        textInput(
                            "edate",
                            mandatory("Event Dates"),
                            placeholder = "MONTH ## - ##, YEAR"
                        ),
                        textInput(
                            "elocation",
                            mandatory("Event Location"),
                            placeholder = "City, State, Country"
                        ),
                        textInput(
                            "fullname",
                            mandatory("First and last name"),
                            placeholder = "Jane Doe"
                        ),
                        textAreaInput(
                            "address",
                            "Mailing address",
                            height = "100px",
                            placeholder = paste0(
                                "123 Fake St.\nSpringfield",
                                "\nZipcode\nUnited States"
                            )
                        ),
                        actionButton("submit", "Submit", class = "btn-primary")
                    ),
                    hidden(
                        div(
                            id = "eurl",
                            textInput("eurl", label = mandatory("Event URL"))
                        )
                    ),
                    hidden(
                        div(
                            id = "render_msg",
                            h3("Review the document for accuracy"),
                            downloadButton("download", "Download certificate")
                        )
                    ),
                    hidden(
                        span(id = "submit_msg", "Rendering..."),
                        div(
                            id = "error",
                            div(
                                br(), tags$b("Error: "), span(id = "error_msg")
                            )
                        )
                    ),
                    width = 6
                ) # sidebarPanel
            ), # div
            mainPanel(
                width = 4
            )
        ) # sidebarLayout
    ) # fluidPage

    server <- function(input, output, session) {
        observeEvent(input$presubmit, {
            eid <- input[["eid"]]
            ename <- gsub("bioc", "Bioconductor ", eid, ignore.case = TRUE)
            ename <- gsub("euro", "European ", ename, ignore.case = TRUE)
            updateTextInput(session, "ename", value = ename)
            eurl <- .genEurl(eid)
            updateTextInput(session, "eurl", value = eurl)
            disable(id = "presubmit")
        })
        observe({
            mandatoryFilled <- vapply(
                .MANDATORY_DATA_NAMES,
                function(x) {
                    BiocBaseUtils::isScalarCharacter(input[[x]])
                },
                logical(1L)
            )
            mandatoryFilled <- all(mandatoryFilled)
            toggleState(id = "submit", condition = mandatoryFilled)
        })
        formData <- reactive({
            data <- vapply(
                fieldsAll, function(x) input[[x]], character(1L)
            )
            as.data.frame(t(data))
        })
        observeEvent(input$submit, {
            tryCatch({
                fdata <<- formData()
                hide("form")
                hide("error")
                show("render_msg")
                hide("presubmit")
            }, error = function(e) {
                html("error_msg", e$message)
                show(id = "error", anim = TRUE, animType = "fade")
            }, finally = {
                enable("download")
                hide("submit_msg")
            })
        })
        output$download <- downloadHandler(
            filename = function() {
                paste0(gsub("\\s+", "_", input$fullname), "_certificate.pdf")
            },
            content = function(file) {
                certificate(.data = fdata, file = file)
            },
            contentType = "application/pdf"
        )
    }

    shinyApp(ui, server)
}
