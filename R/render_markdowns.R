#' Render all helper modals, introduction, and notes documents
#'
#' @param .output_dir character, directory location for output markdowns
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' render_markdowns()
#' }
#' @importFrom rmarkdown render
render_markdowns <- function(.output_dir = "inst/app/www") {
  
  
  rmarkdown::render("inst/guides/reference_doc.Rmd",
                    output_format = "github_document", # -----
                    output_file = "reference_doc.md", 
                    output_dir = .output_dir,
                    params = list(
                      actor_id = "roten",
                      data_date = Sys.Date(),
                      sha = system("git rev-parse --short HEAD",
                                   intern = TRUE
                      )
                    )
  )
  
  
  rmarkdown::render("inst/guides/intro_doc.Rmd",
                    output_format = "github_document", # -----
                    output_file = "intro_doc.md",
                    output_dir = .output_dir,
                    params = list(
                      actor_id = "esch",
                      data_date = Sys.Date(),
                      sha = system("git rev-parse --short HEAD",
                                   intern = TRUE
                      )
                    )
  )
  
  rmarkdown::render("inst/guides/StatusHelp.Rmd",
                    output_format = "github_document", # -----
                    output_file = "StatusHelp.md", 
                    output_dir = .output_dir,
                    params = list(
                      actor_id = "esch",
                      data_date = Sys.Date(),
                      sha = system("git rev-parse --short HEAD",
                                   intern = TRUE
                      )
                    )
  )
  
  rmarkdown::render("inst/guides/BufferHelp.Rmd",
                    output_format = "github_document", # -----
                    output_file = "BufferHelp.md", 
                    output_dir = .output_dir,
                    params = list(
                      actor_id = "esch",
                      data_date = Sys.Date(),
                      sha = system("git rev-parse --short HEAD",
                                   intern = TRUE
                      )
                    )
  )
  
  rmarkdown::render("inst/guides/DataSourceHelp.Rmd",
                    output_format = "github_document", # -----
                    output_file = "DataSourceHelp.md",
                    output_dir = .output_dir,
                    params = list(
                      actor_id = "esch",
                      data_date = Sys.Date(),
                      sha = system("git rev-parse --short HEAD",
                                   intern = TRUE
                      )
                    )
  )
  
}
