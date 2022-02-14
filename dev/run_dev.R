# Set options here
options(
  shiny.launch.browser = TRUE,
  scipen = 9999,
  warn = -1,
  verbose = FALSE,
  golem.app.prod = TRUE
) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

regionalparks.acs::render_markdowns()

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app()

#choose data source =population estimates
#choose data source = population characteristics
#choose variable to map = housing, % cost burdened
#THEN ALL VARIABLES QUIT?!?

