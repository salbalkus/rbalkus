#' Salvador Balkus' ggplot formatting function
#'
#' This function modifies the ggplot to have the proper colors, fonts, and other formats.
#'
#' @param palette_ordered Whether to use a color palette ordered by color lightness.
#' @param continuous_axes Whether axes are continuous; if so, cap axis at endpoints.
#' @param map Whether the ggplot is a map, to remove axes and preserve geographical scaling.
#' @param enlarge_legend_items If True, enlarges legend items. Use for scatter plots or other plots with small dots.
#' @param colors Whether the variable mapped to color is continuous. Options: "continuous" or "discrete"
#' @export

format_sbalkus <- function(palette_ordered=TRUE, continuous_axes=TRUE, map=FALSE, enlarge_legend_items=FALSE, colors="discrete"){
  palette <- palette_sbalkus_unordered
  palette_dark <- palette_sbalkus_unordered_dark
  
  if(palette_ordered){
    palette <- palette_sbalkus_ordered
    palette_dark <- palette_sbalkus_ordered_dark
  }
  
  output = list(theme_sbalkus())
  if(colors=="discrete"){
    output = append(output, list(
      ggplot2::scale_color_manual(values=palette_dark), 
      ggplot2::scale_fill_manual(values=palette)
    ))
    if(enlarge_legend_items){
        output = append(output, list(
          ggplot2::guides(color = guide_legend(nrow=1, byrow=TRUE, override.aes = list(size = 8)), fill=guide_legend(nrow=1,byrow=TRUE))
        ))
      }
  } else if(colors=="diverging"){
    output = append(output, list(
      ggplot2::scale_colour_gradient2(
        low = palette[1],
        mid = "white",
        high = palette[3],
        midpoint = 0,
        space = "Lab",
        na.value = palette[5],
        guide = "colorbar",
        aesthetics = "color"
      ),
      ggplot2::scale_fill_gradient2(
        low = palette[1],
        mid = "white",
        high = palette[3],
        midpoint = 0,
        space = "Lab",
        na.value = palette_dark[5],
        guide = "colorbar",
        aesthetics = "fill"
      )
    ))
    
  } else {
    output = append(output, list(
      ggplot2::scale_colour_gradient(
        high = "#00aaf5",
        low = "#002e5c",
        space = "Lab",
        na.value = "#d1d1d1",
        guide = "colorbar",
        aesthetics = "color"
      ),
      ggplot2::scale_fill_gradient(
        high = "#00aaf5",
        low = "#002e5c",
        space = "Lab",
        na.value = "#d1d1d1",
        guide = "colorbar",
        aesthetics = "fill"
      )
    ))
  }
  
  if(continuous_axes){
    output = append(output, list(
      lemon::coord_capped_cart(bottom='none', left='bottom'),
      ggplot2::expand_limits(y = 0)
    )
    )
  }
  if(map){
    output=append(output, list(ggplot2::theme(axis.line=ggplot2::element_blank(), axis.text=ggplot2::element_text(color="white"), axis.title=ggplot2::element_text(color="white"), axis.ticks=ggplot2::element_blank(), legend.margin = ggplot2::margin(t = -60, r = 0, b = 0, l = 0))))
  }
  output
}

