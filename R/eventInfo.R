demodata <- list(
    fullname = "Lorus Ipsum",
    address = "dolor sit amet,\\ consectetur adipiscing elit.",
    ename = "Cras eu blandit",
    edate = "2023-09-27",
    elocation = "felis, in ornare sapien.",
    eurl = "Cras vel urna ac libero"
)

.genEurl <- function(key) {
    eurl <- paste0("https://", key, ".bioconductor.org")
    if (!RCurl::url.exists(eurl))
        stop("Event URL does not exist; check 'key'")
    eurl
}
