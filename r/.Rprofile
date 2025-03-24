
local({
    options(
        ## Default CRAN mirror.
        repos = c(CRAN = "https://cloud.r-project.org"),
        ## Increase download timeout for Bioconductor genomes.
        timeout = 3600,
        ## Parallelize package installation.
        Ncpus = parallel::detectCores(),
        ## Package initialization.
        usethis.description = list(
          "Authors@R" = utils::person(
            "Pariksheet", "Nanda",
            email = "pan79@pitt.edu",
            role = c("aut", "cre"),
            comment = c(ORCID = "0000-0001-9726-4552")),
          Language = "en",
          License = "Apache License (>= 2)"
        )
    )
    ## macOS packages typically need OpenSSL@1.1 instead of OpenSSL@3. 
    brew <- Sys.getenv("BREW_ROOT", NA)
    if (!is.na(brew)) {
        PKG_CONFIG_PATH <- Sys.getenv("PKG_CONFIG_PATH")
        pc_openssl <-
            Sys.glob(file.path(brew, "Cellar/openssl@1.1/*/lib/pkgconfig"))
        if (PKG_CONFIG_PATH == "")
            PKG_CONFIG_PATH <- pc_openssl
        else
            PKG_CONFIG_PATH <- paste0(pc_openssl, ":", PKG_CONFIG_PATH)
        Sys.setenv(PKG_CONFIG_PATH = PKG_CONFIG_PATH)
    }
})
