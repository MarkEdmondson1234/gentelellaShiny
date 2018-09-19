# Add an html dependency, without overwriting existing ones
appendDependencies <- function(x, value) {
  if (inherits(value, "html_dependency"))
    value <- list(value)

  old <- attr(x, "html_dependencies", TRUE)

  htmltools::htmlDependencies(x) <- c(old, value)
  x
}

# Add dashboard dependencies to a tag object
addDeps <- function(x) {

  # put all necessary ressources here
  gentelella_css <- "custom.min.css"
  gentelella_js <- "custom.min.js"
  bootstrap_css <- "bootstrap.min.css"
  bootstrap_js <- "bootstrap.min.js"
  fontawesome_css <- "font-awesome.min.css"
  animate_css <- "animate.min.css"
  wizard_js <- "wizard.js"
  chart_js <- "chart.min.js"
  init_js <- "init.js"
  progress_js <- "progress.min.js"
  progress_css <- "progress.min.css"

  dashboardDeps <- list(
    # bootstrap 3 deps
    htmltools::htmlDependency(
      name = "bootstrap",
      version = "3.3.7",
      src = c(file = system.file("bootstrap-3.3.7", package = "gentelellaShiny")),
      script = bootstrap_js,
      stylesheet = bootstrap_css
    ),
    # progress
    htmltools::htmlDependency(
      name = "progress",
      version = "1.0.0",
      src = c(file = system.file("progress-1.0.0", package = "gentelellaShiny")),
      script = progress_js,
      stylesheet = progress_css
    ),
    # easypiechart js
    #htmltools::htmlDependency(
    #  name = "easypiechart",
    #  version = "2.1.6",
    #  src = c(file = system.file("easypiechart-2.1.6", package = "gentelellaShiny")),
    #  script = chart_js
    #)
    # smart wizard
    #htmltools::htmlDependency(
    #  name = "smartwizard",
    #  version = "3.3.1",
    #  src = c(file = system.file("smartwizard-3.3.1", package = "gentelellaShiny")),
    #  script = wizard_js
    #)
    # gentelella (does not work for some reason... except init_js)
     htmltools::htmlDependency(
       name = "gentelella",
       version = "1.5.0",
       src = c(file = system.file("gentelella-1.5.0", package = "gentelellaShiny")),
       script = init_js
     )
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
