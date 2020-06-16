# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
regionalparks.acs::run_app() # add parameters here (if any)


# rsconnect::deployApp(appDir = ".", appId = 2464148,  account = "metrotransitmn", server = "shinyapps.io", appName = "regional-parks-acs", appTitle = "regional-parks-acs", lint = FALSE, metadata = list(asMultiple = FALSE, asStatic = FALSE, ignoredFiles = "README.Rmd|.github/.gitignore|.github/workflows/R-CMD-check.yaml|data-raw/block_group.R|data-raw/census_tract.R|data-raw/park_trail_geog.R|data-raw/table_ct.R|dev/01_start.R|dev/02_dev.R|dev/03_deploy.R|dev/run_dev.R|man/pipe.Rd|man/run_app.Rd|tests/testthat.R|tests/testthat/test-golem-recommended.R"), logLevel = "verbose")
