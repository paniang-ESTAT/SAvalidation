args <- commandArgs(trailingOnly = TRUE)

template <- paste0(args[1], "/L2Template.qmd")

quarto::quarto_render(template, quiet=TRUE, 
                      execute_params = list(sdir = gsub("\\", "/", dirname(args[1]), fixed = TRUE)))

quarto::quarto_serve(template, render = FALSE)

file.remove(gsub(".qmd", ".html", template))
unlink(gsub(".qmd", "_data", template), recursive=TRUE)
unlink(gsub(".qmd", "_files", template), recursive=TRUE)
