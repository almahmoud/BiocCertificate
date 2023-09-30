templatePath <- function(file = c("certificate", "letter")) {
    file <- match.arg(file)
    filename <- switch(
        file,
        certificate = "CertificateTemplate.txt",
        letter = "LetterTemplate.txt"
    )
    system.file(
        "resources", filename, package = "BiocCertificates", mustWork = TRUE
    )
}

.checkData <- function(data) {
    dataok <- all(
        c("fullname", "eventname", "eventdate", "eventlocation") %in%
            names(data)
    )
    if (!dataok)
        stop(
            "<internal> 'data' does not have required column names"
        )
    datavalid <- vapply(
        data, function(x) BiocBaseUtils::isScalarCharacter(x), logical(1L)
    )
    if (!all(datavalid))
        stop("Columns do not have valid data: ", names(data)[!datavalid])
    TRUE
}

certificate <- function(template = "certificate", data, knitDir) {
    .checkData(data)
    template <- templatePath(template)
    templateCert <- readLines(template)
    tmpRmd <- whisker::whisker.render(
        templateCert,
        data = data
    )
    outPDF <- paste0(
        gsub("\\s+", "_", data[["fullname"]]),
        "_certificate.pdf"
    )
    RmdFile <- tempfile(fileext = ".Rmd")
    writeLines(tmpRmd, RmdFile)
    # Creating the certificates using R markdown.
    rmarkdown::render(
        input = RmdFile, output_file = outPDF, quiet = TRUE,
        clean = FALSE
    )

    file.path(tempdir(), outPDF)
}
