#' @export
BiocCertificate <- function(...) {
    atemp <- system.file(
        "resources", "AttendanceTemplate.Rmd",
        package = "BiocCertificates", mustWork = TRUE
    )
    adat <- list(
        fullname = "Lorus Ipsum",
        address = "dolor sit amet,\nconsectetur adipiscing elit.",
        event = "Cras eu blandit",
        location = "felis, in ornare sapien.",
        eventurl = "Cras vel urna ac libero"
    )
    bioctemp <- whisker::whisker.render(
        template = readLines(atemp),
        data = adat
    )
    tempRmd <- tempfile(fileext = ".Rmd")
    writeLines(bioctemp, tempRmd)
    outfile <- file.path(
        getwd(),
        paste0(
            gsub("\\s+", "_", adat[["fullname"]]),
            "_certificate.html"
        )
    )
    rmarkdown::render(input = tempRmd, output_file = outfile)
}
