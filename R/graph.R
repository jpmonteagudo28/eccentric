#' Graph Z-Score Distributions and Animate Histograms by Sample
#'
#' Creates histograms of z-score distributions from a data frame and optionally compiles them into an animated GIF.
#' Each column of the data frame is treated as a separate sample distribution.
#'
#' Red vertical dashed lines are drawn at ±`ci` to represent critical values. Additional text annotations show
#' sample size and rejection rate, if provided.
#'
#' @param data A data frame where each column represents a vector of z-scores from a different sample.
#' @param ci A numeric scalar indicating the critical z-score value (e.g., 1.96 for 95% confidence).
#' Must not be `NULL`.
#' @param xlim A numeric vector of length 2 specifying the x-axis limits of the histogram. Default is `c(-4, 4)`.
#' @param compile_gif Logical. If `TRUE`, compiles all histogram plots into an animated GIF using ImageMagick. Default is `FALSE`.
#' @param fps Numeric. Frames per second for the GIF animation. Default is `2`.
#' @param filename Character string. Name of the output GIF file (if `compile_gif = TRUE`). Default is `"output.gif"`.
#' @param samples Optional. Label to indicate the number of distributions represented (used in plot title). Should be a scalar or `NULL`.
#' @param n Optional. A vector of sample sizes for each column of `data`. Used in plot annotations. Defaults to `NULL`.
#' @param a Optional. A vector of rejection rates corresponding to each column of `data`. Used in plot annotations. Defaults to `NULL`.
#'
#' @return This function is invoked for its side effects:
#' \itemize{
#'   \item Saves individual `.png` histogram plots to the working directory.
#'   \item Optionally compiles those plots into a `.gif` using ImageMagick if `compile_gif = TRUE`.
#' }
#' No R object is returned.
#'
#' @details
#' Histograms are generated using `ggplot2`, with consistent formatting for serif fonts and minimal gridlines.
#' ImageMagick (via the `magick` package) is used to stitch together frames for animation.
#'
#' The file names for individual frames follow the format `"frame_###.png"`, and are automatically deleted after GIF creation.
#'
#' @examples
#' # Example data
#' df <- data.frame(
#'   sample1 = rnorm(100),
#'   sample2 = rnorm(100, mean = 0.5)
#' )
#'
#' # Basic plot without GIF
#' graph_data(df, ci = 1.96, samples = 2, n = c(100, 100), a = c(0.05, 0.07))
#'
#' # Create animated GIF
#' graph_data(df, ci = 1.96, samples = 2, n = c(100, 100), a = c(0.05, 0.07), compile_gif = TRUE)
#'
#' @export

graph_data <- function( data
                        , ci = NULL
                        , xlim = c(-4,4)
                        , compile_gif = FALSE
                        , fps = 2
                        , filename = "output.gif"
                        , samples = NULL
                        , n = NULL
                        , a = NULL){ ## is the percent of z-scores outside of critical value for each sample size

  stopifnot(
    is.data.frame(data)
    , is.numeric(ci)
    , is.numeric(fps)
    , is.character(filename)
  )

  for(i in seq_along(data)){
    col_data <- data[[i]]
    col_name <- names(data)[i]

    data_plot <- ggplot2::ggplot( data.frame(x = col_data)
                                  ,ggplot2::aes(x = x)
                                ) +
                 ggplot2::geom_histogram( ggplot2::aes(y = ggplot2::after_stat(density))
                                         ,binwidth = .25
                                         ,na.rm = TRUE
                                         ,fill = "gray70"

                                        ) +
                 ggplot2::geom_vline( xintercept = c(-ci,ci)
                                      ,color = "red"
                                      ,linetype = "longdash"
                                    ) +
                 ggplot2::scale_x_continuous(limits = xlim) +
                 ggplot2::scale_y_continuous(limits = c(0,0.95)) +
                 ggplot2::theme_minimal() +
                 ggplot2:: theme(
                                 plot.background = ggplot2::element_rect( fill = "white"
                                                                         ,color = NA
                                                                        ),
                                 plot.title = ggplot2::element_text( family = "serif"
                                                                    ,size = 18
                                                                    ,hjust = .5
                                                                   ),
                                 axis.title = ggplot2::element_text( family = "serif"
                                                                    ,size = 11
                                                                    ,hjust = .5
                                                                   ),
                                 axis.text = ggplot2::element_text( family = "serif"
                                                                    ,size = 12
                                                                  ),
                                 panel.grid.major = ggplot2::element_line( colour = "gray85"
                                                                          ,linewidth = .15
                                                                          ,linetype = "solid"
                                                                         ),
                                 panel.grid.minor = ggplot2::element_blank()
                                ) +
                 ggplot2::labs( x = "Z-score"
                               ,y = ""
                               ,title = paste0("Histogram of Z-scores for ", samples," Distributions")
                              ) +
                 ggplot2::annotate( "text"
                                    ,x = (xlim[1]+ 0.1)
                                    ,y = 0.92
                                    ,hjust = 0
                                    ,family = "serif"
                                    ,size = 4
                                    ,label = paste0("N = ", ifelse(is.null(n[i]),"NA",n[i]))
                                  ) +
                 ggplot2::annotate( "text"
                                    ,x = xlim[1]+ 0.1
                                    ,y = 0.85
                                    ,hjust = 0
                                    ,family = "serif"
                                    ,size = 4
                                    ,label = paste0("Rejection Rate = ", ifelse(is.null(a[i]),"NA",a[i]))
      )

    plot_filename <- paste0("frame_", sprintf("%03d", i), ".png")

    ggplot2::ggsave( filename = plot_filename
                     ,plot = data_plot
                     ,width = 6
                     ,height = 6
                     ,dpi = 150
    )
  }

  message("Individual plots saved to working directory")

  if(compile_gif == TRUE){
    message("Using ImageMagick to create gif...")
    animated_pngs <- magick::image_read(gtools::mixedsort(list.files(pattern = "frame_\\d+\\.png$")))
    animated_pngs <- magick::image_scale(animated_pngs, "600x600")
    animated_gif <- magick::image_animate(animated_pngs, fps = fps, loop = 0)
    magick::image_write(animated_gif, path = filename)
    do.call(file.remove, list(list.files(pattern = "frame_\\d+\\.png$")))
    message("Finished creating gif, saved in the working directory")
  }

}
