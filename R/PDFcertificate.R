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
    dataok <- all(.MANDATORY_DATA_NAMES %in% names(data))
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

.growData <- function(.data) {
    ## TODO: auto fill based on Event ID
    elogo <- system.file(
        "resources", "bioconductor_logo_rgb.png",
        package = "BiocCertificates", mustWork = TRUE
    )
    cbind.data.frame(
        .data, bioclogo = elogo
    )
}

certificate <- function(template = "certificate", .data, file) {
    .data <- .growData(.data)
    .checkData(.data)
    template <- templatePath(template)
    templateCert <- readLines(template)
    tmpRmd <- whisker::whisker.render(
        templateCert,
        data = .data
    )
    RmdFile <- tempfile(fileext = ".Rmd")
    writeLines(tmpRmd, RmdFile)
    rmarkdown::render(
        input = RmdFile, output_file = file, quiet = TRUE,
        clean = FALSE
    )
}
