# demoData <- data.frame(
#     eid = "eurobioc2023",
#     ename = "European Bioconductor 2023",
#     fullname = "Marcel Ramos Pérez"
# )

eventData <- function(eid) {
    edata <- system.file(
        "resources", "events.yml",
        package = "BiocCertificate", mustWork = TRUE
    )
    edata <- yaml::read_yaml(edata)
    eid <- tolower(eid)
    if (!eid %in% names(edata[["events"]]))
        stop("Event ID not supported; contact organizers")
    edata <- as.data.frame(edata[["events"]][[eid]])
    edata[["esticker"]] <- .cache_url_file(edata[["stickerdl"]])
    edata
}

.BiocCertificate_cache <- function() {
    tools::R_user_dir("BiocCertificate", "cache")
}

.get_cache <- function() {
    BiocFileCache(cache = .BiocCertificate_cache())
}

#' @importFrom BiocFileCache BiocFileCache bfcquery bfcdownload bfcneedsupdate
#'   bfcrpath
.cache_url_file <- function(url) {
    bfc <- .get_cache()
    bquery <- bfcquery(bfc, url, "rname", exact = TRUE)
    if (identical(nrow(bquery), 1L) && bfcneedsupdate(bfc, bquery[["rid"]]))
        tryCatch({
            bfcdownload(
                x = bfc, rid = bquery[["rid"]], rtype = "web", ask = FALSE
            )
        }, error = warning)

    bfcrpath(
        bfc, rnames = url, exact = TRUE, download = TRUE, rtype = "web"
    )
}

.genEurl <- function(key) {
    eurl <- paste0("https://", key, ".bioconductor.org")
    if (!RCurl::url.exists(eurl))
        stop("Event URL does not exist; check 'key'")
    eurl
}
