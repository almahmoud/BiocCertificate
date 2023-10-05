#' @import shiny
mandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}

appCSS <- paste(
    ".mandatory_star { color: red; }", "#error { color: red; }", sep = "\n"
)

.MANDATORY_INPUT_FIELDS <- c("eid", "fullname")

.MANDATORY_DATA_NAMES <- c("eid", "ename", "edate", "elocation", "fullname")

#' Generate a Bioconductor conference attendance certificate
#'
#' @description
#' The function invokes a shiny app for attendees to generate their own
#' certificate of attendance. This is done with the provided event identifier
#' (`Event ID`). Attendees must enter the event identifier and their
#' full name in the app and click on submit. An iframe pop-up will show
#' the certificate along with a download / print button.
#'
#' @param \ldots Inputs to the main function are not used.
#'
#' @return Called for the side effect of displaying the shiny app.
#'
#' @importFrom shinyjs show hide hidden enable disable inlineCSS toggleState
#'   html
#' @export
BiocCertificate <- function(...) {
    options(
        shiny.host = "127.0.0.1",
        shiny.port = 8080
    )
    fieldsAll <- c("eid", "ename", "fullname", "address")
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
                        id = "template",
                        radioButtons(
                            "template", "Select format",
                            c("certificate", "letter"), "certificate"
                        )
                    ),
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
                            h3("Review the document for accuracy")
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
                    width = 3
                ) # sidebarPanel
            ), # div
            mainPanel(
                hidden(
                    div(
                        id = "viewer",
                        htmlOutput("pdfviewer"),
                    )
                ),
                width = 9
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
                .MANDATORY_INPUT_FIELDS,
                function(x) {
                    BiocBaseUtils::isScalarCharacter(input[[x]])
                },
                logical(1L)
            )
            if (identical(input$template, "letter"))
                hasAddress <- BiocBaseUtils::isScalarCharacter(
                    input[["address"]]
                )
            else
                hasAddress <- TRUE

            mandatoryFilled <- all(mandatoryFilled, hasAddress)
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
                fdata <- formData()
                hide("form")
                hide("error")
                show("render_msg")
                hide("presubmit")
            }, error = function(e) {
                html("error_msg", e$message)
                show(id = "error", anim = TRUE, animType = "fade")
            }, finally = {
                show("viewer")
                hide("submit_msg")
            })
        })
        output$pdfviewer <- renderText({
            local <- paste0(
                "http://", getOption("shiny.host"),
                ":", getOption("shiny.port"), "/"
            )
            cert_file <- paste0(
                local,
                certificate(
                    template = input$template,
                    .data = formData(),
                    file =  paste0(
                        gsub("\\s+", "_", input$fullname),
                        "_", input$template, ".pdf"
                    )
                )
            )
            message(cert_file)
            return(paste0(
                '<iframe style="height:900px; width:100%" src="',
                cert_file,
                '"></iframe>'
            ))
        })
    }
    shinyApp(ui, server)
}
