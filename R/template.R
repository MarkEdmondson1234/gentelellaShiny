#' Tracking tags for your app
#'
#' @param tag_template Tracking tag template
#' @param tag_code If GA, the UA code.  If GTM, the GTM code.
#'
#' tag_code depends on what tag_template is:
#'
#' \itemize{
#'   \item ga: The UA code (UA-xxxxxx-x)
#'   \item gtm The GTM code (GTM-xxxxx)
#'   \item freeform The JS code
#'  }
#' If you select a tag template, you can put UA-xxxx-x as tag_code
#'
#' @return JavaScript for tracking
#' @export
trackingTags <- function(tag_template = c("ga","gtm","freeform"),
                         tag_code){

  tag_template <- match.arg(tag_template)

  out <- switch(tag_template,
                ga = sprintf("<script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
                             (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
                             m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
                             })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

                             ga('create', '%s', 'auto');
                             ga('send', 'pageview');

                             </script>", tag_code),
                gtm = sprintf("<!-- Google Tag Manager -->
<noscript><iframe src=\"//www.googletagmanager.com/ns.html?id=%s\"
                height=\"0\" width=\"0\" style=\"display:none;visibility:hidden\"></iframe></noscript>
                <script>(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
                new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
                j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
                '//www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
                })(window,document,'script','dataLayer','%s');</script>
                <!-- End Google Tag Manager -->", tag_code, tag_code),
                freeform = tag_code



                )

  HTML(out)

}
