#' profileElements UI
#'
#' Shiny Module for use with \link{profileElements}
#'
#' @param id Shiny id
#'
#' @return Shiny UI
#' @export
profileElementsUI <- function(id){

  ns <- shiny::NS(id)

}

#' profileElements
#'
#' Shiny Module for use with \link{profileElementsUI}
#'
#' Call via \code{shiny::callModule(profileElements, "your_id")}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
#' @return Something
#' @export
profileElements <- function(input, output, session){

    ns <- session$ns

}


#' Render the profile box
#'
#' @param profile_name The user name
#' @param profile_url The user profile image url
#'
#' @return profile box
#' @export
profile_box <- function(profile_name,
                        profile_url){

  withTags({

    div(class = "profile",
        div(class = "profile_pic",
            img(src = profile_url, class="img-circle profile_img")
            ),
        div(class = "profile_info",
            span("Welcome,"),
            h2(profile_name)
            )
        )
  })
}


#' Render the top navbar profile
#'
#' @param profile_name The user name
#' @param profile_url The user profile image url
#'
#' @return profile top nav
#' @export
profile_nav <- function(profile_name,
                        profile_url,
                        menu_items = NULL
                        ){

  withTags(
    ul(class = "nav navbar-nav navbar-right",
       li(
         a(href="javascript:;", class="user-profile dropdown-toggle", `data-toggle`="dropdown", `aria-expanded`="false",
           img(src=profile_url),profile_name,
           span(class=" fa fa-angle-down")
           ),
         ul(class="dropdown-menu dropdown-usermenu pull-right",
            tagList(menu_items)
         )
       ))


  )
}
