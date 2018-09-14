# Add an html dependency, without overwriting existing ones
appendDependencies <- function(x, value) {
  if (inherits(value, "html_dependency"))
    value <- list(value)

  old <- attr(x, "html_dependencies", TRUE)

  htmltools::htmlDependencies(x) <- c(old, value)
  x
}

# Add dashboard dependencies to a tag object
addDeps <- function(x, theme) {

  # put all necessary ressources here
  gentelella_css <- "custom.min.css"
  gentelella_js <- "custom.min.js"
  bootstrap_css <- "bootstrap.min.css"
  bootstrap_js <- "bootstrap.min.js"
  fontawesome_css <- "font-awesome.min.css"
  animate_css <- "animate.min.css"

  dashboardDeps <- list(
    # bootstrap 3 deps
    # htmltools::htmlDependency(
    #   name = "bootstrap",
    #   version = "3.3.7",
    #   src = c(file = system.file("bootstrap-3.3.7", package = "gentelellaShiny")),
    #   script = bootstrap_js,
    #   stylesheet = bootstrap_css
    # ),
    # gentelella
    htmltools::htmlDependency(
      name = "gentelella",
      version = "1.5.0",
      src = c(file = system.file("gentelella-1.5.0", package = "gentelellaShiny")),
      script = gentelella_js,
      stylesheet = gentelella_css
    )#,
    # fontawesome
    # htmltools::htmlDependency(
    #   name = "fontawesome",
    #   version = "4.6.3",
    #   src = c(file = system.file("fontawesome-4.6.3", package = "gentelellaShiny")),
    #   stylesheet = fontawesome_css
    # ),
    # animate
    # htmltools::htmlDependency(
    #   name = "animate",
    #   version = "3.5.0",
    #   src = c(file = system.file("animate-3.5.0", package = "gentelellaShiny")),
    #   stylesheet = animate_css
    # )
  )
  appendDependencies(x, dashboardDeps)
}
