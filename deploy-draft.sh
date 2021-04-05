
#!/bin/sh -l


Rscript -e "rsconnect::setAccountInfo(name='metrotransitmn', token='${SHINYAPPSIO_TOKEN}', secret='${SHINYAPPSIO_SECRET}')"
Rscript -e "rsconnect::deployApp(appDir = '.', account = 'metrotransitmn', server = 'shinyapps.io', appName = 'regional-parks-buffer', appTitle = 'regional-parks-buffer', lint = FALSE, metadata = list(asMultiple = FALSE, asStatic = FALSE, ignoredFiles = 'SmallAreaEstimatesBlockGroup.xlsx|SmallAreaEstimatesTract.xlsx|.github/workflows/R-CMD-check-deploy.yaml|.github/workflows/R-CMD-check.yaml|dev/01_start.R|dev/02_dev.R|dev/03_deploy.R|dev/run_dev.R|data-raw'), logLevel = 'verbose')"
