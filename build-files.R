library(tidyverse)

file_names <- c("sp-htf.qmd", "sp-vbda.qmd", "sp-education.qmd", "ew-haf.qmd",
                "ew-clt.qmd", "bw-mumi.qmd", "bw-permits.qmd", "bw-advisory.qmd")
titles <- c("Develop housing trust fund",
            "Expand VBDA role",
            "Design education campaign",
            "Create housing fund for workers",
            "Leverage VSCLT",
            "Pursue innovative development models",
            "Streamline permitting",
            "Explore zoning advisory board"
            )

a_names <- c("a2-workforce.qmd", "a3-noah.qmd")
a_titles <- c("Workforce housing affordability analysis",
              "Naturally occurring affordable housing analysis"
              )

create_qmd_file <- function(file_name, title) {
  content <- paste("# ", title, "\n", "\n", "...", sep = "")
    write(content, file_name)
}

map2(file_names, titles, create_qmd_file)

map2(a_names, a_titles, create_qmd_file)
