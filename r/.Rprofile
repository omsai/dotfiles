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
    ## spack r@trunk needs to know where its dependent packages are
    ## for devtools.
    ##
    ## In section 6.3.6 External Software of the R Installation and
    ## Administration manual, "LD_LIBRARY_PATH is the best choice for
    ## a user to set."
    ## https://cran.r-project.org/doc/manuals/r-devel/R-admin.html#External-software
    if (grepl("opt/spack", R.home())) {
        spack_install_prefix <-
            R.home() |>
            dirname() |>
            dirname() |>
            dirname()
        libdirs <-
            Sys.glob(
                file.path(
                    spack_install_prefix,
                    c("curl-*",
                      "libjpeg-*"),
                    "lib"))
        if (length(libdirs)) {
            LD_LIBRARY_PATH <- paste(
                Sys.getenv("LD_LIBRARY_PATH"),
                paste(libdirs, collapse = ":"),
                sep = ":")
            Sys.setenv(LD_LIBRARY_PATH = LD_LIBRARY_PATH)
        }
    }
})
