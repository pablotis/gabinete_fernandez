
utils::globalVariables(c("x", "y", "value"))

.dbg <- TRUE

msg <- function(...) {
  
  if (.dbg) message(...)
  
}

geom_rrect <- function(mapping = NULL, data = NULL, # nocov start
                       stat = "identity", position = "identity",
                       radius = grid::unit(6, "pt"),
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRrect,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      radius = radius,
      na.rm = na.rm,
      ...
    )
  )
}

GeomRrect <- ggplot2::ggproto(
  "GeomRrect", ggplot2::Geom,
  
  default_aes = ggplot2::aes(
    fill = "grey35", size = 0.5, linetype = 1, alpha = NA#, colour = NA
  ),
  
  required_aes = c("xmin", "xmax", "ymin", "ymax"),
  
  draw_panel = function(self, data, panel_params, coord,
                        radius = grid::unit(6, "pt")) {
    
    coords <- coord$transform(data, panel_params)
    
    lapply(1:length(coords$xmin), function(i) {
      
      grid::roundrectGrob(
        coords$xmin[i], coords$ymax[i],
        width = (coords$xmax[i] - coords$xmin[i]),
        height = (coords$ymax[i] - coords$ymin)[i],
        r = radius,
        default.units = "native",
        just = c("left", "top"),
        gp = grid::gpar(
          col = coords$colour[i],
          fill = alpha(coords$fill[i], coords$alpha[i]),
          lwd = coords$size[i] * .pt,
          lty = coords$linetype[i],
          lineend = "butt"
        )
      )
      
    }) -> gl
    
    grobs <- do.call(grid::gList, gl)
    
    ggname("geom_rrect", grid::grobTree(children = grobs))
    
  },
  
  draw_key = ggplot2::draw_key_polygon
  
) # nocov end

# Waffles mappings from css names to unicode chars was out of date
# This variation updates it from the latests css from github
.fa_unicode_init <- function() {
  
  xdf <- readRDS(system.file("extdat/fadf.rds", package = "waffle"))
  xdf[xdf[["type"]] != "regular", ]
  
}

.fa_unicode <- .fa_unicode_init()

.display_fa <- function(fdf) {
  vb <- stringr::str_match(fdf[["glyph"]], '(viewBox="[^"]+")')[,2]
  stringr::str_replace(
    fdf[["glyph"]],
    vb,
    sprintf('%s width="24" height="24"', vb)
  ) -> fdf[["glyph"]]
  DT::datatable(fdf[,c("name", "type", "glyph")], escape = FALSE)
}

#' Search Font Awesome glyph names for a pattern
#'
#' @param pattern pattern to search for in the names of Font Awesome fonts
#' @export
fa_grep <- function(pattern) {
  res <- which(grepl(pattern, .fa_unicode[["name"]]))
  if (length(res)) {
    .display_fa(.fa_unicode[res, ])
  } else {
    message("No Font Awesome font found with that name pattern.")
  }
}

#' List all Font Awesome glyphs
#'
#' @export
fa_list <- function() {
  .display_fa(.fa_unicode)
}

#' Install Font Awesome 5 Fonts
#'
#' @export
install_fa_fonts <- function() {
  message(
    "The TTF font files for Font Awesome 5 fonts are in:\n\n",
    system.file("fonts", package = "waffle"),
    "\n\nPlease navigate to that directory and install them on ",
    "your system."
  )
}

#' Font Awesome 5 Solid
#'
#' @description `fa5_solid` is shorthand for "`FontAwesome5Free-Solid`"
#' @docType data
#' @export
fa5_solid <- "FontAwesome5Free-Solid"

#' Font Awesome 5 Brand
#'
#' @description `fa5_brand` is shorthand for "`FontAwesome5Brands-Regular`"
#' @docType data
#' @export
fa5_brand <- "FontAwesome5Brands-Regular"



picto_scale <- function(aesthetic, values = NULL, ...) {
  
  values <- if (is_missing(values)) "circle" else force(values)
  
  pal <- function(n) {
    vapply(
      if (n > length(values)) rep(values[[1]], n) else values,
      function(.x) .fa_unicode[.fa_unicode[["name"]] == .x, "unicode"],
      character(1),
      USE.NAMES = FALSE
    )
  }
  
  discrete_scale(aesthetic, "manual", pal, ...)
}

#' Used with geom_pictogram() to map Font Awesome fonts to labels
#'
#' @param ... dots
#' @param values values
#' @param aesthetics aesthetics
#' @export
scale_label_pictogram <- function(..., values, aesthetics = "label") {
  picto_scale(aesthetics, values, ...)
}

#' Legend builder for pictograms
#'
#' @param data,params,size legend key things
#' @keywords internal
#' @export
draw_key_pictogram <- function(data, params, size) {
  
  # msg("==> draw_key_pictogram()")
  #
  # print(str(data, 1))
  # print(str(params, 1))
  
  if (is.null(data$label)) data$label <- "a"
  
  textGrob(
    label = data$label,
    x = 0.5, y = 0.5,
    rot = data$angle %||% 0,
    hjust = data$hjust %||% 0,
    vjust = data$vjust %||% 0.5,
    gp = gpar(
      col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
      fontfamily = data$family %||% "",
      fontface = data$fontface %||% 1,
      fontsize = (data$size %||% 3.88) * .pt,
      lineheight = 1.5
    )
  )
}

#' Pictogram Geom
#'
#' There are two special/critical `aes()` mappings:
#' - `label` (so the geom knows which column to map the glyphs to)
#' - `values` (which column you're mapping the filling for the squares with)
#'
#' @md
#' @param mapping Set of aesthetic mappings created by `aes()` or
#'   `aes_()`. If specified and `inherit.aes = TRUE` (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply `mapping` if there is no plot mapping.
#' @param n_rows how many rows should there be in the waffle chart? default is 10
#' @param flip If `TRUE`, flip x and y coords. n_rows then becomes n_cols.
#'     Useful to achieve waffle column chart effect. Defaults is `FALSE`.
#' @param make_proportional compute proportions from the raw values? (i.e. each
#'        value `n` will be replaced with `n`/`sum(n)`); default is `FALSE`.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to `ggplot()`.
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    `fortify()` for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame.`, and
#'    will be used as the layer data.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. `borders()`.
#' @param ... other arguments passed on to `layer()`. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `color = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#' @export
geom_pictogram <- function(mapping = NULL, data = NULL,
                           n_rows = 10, make_proportional = FALSE, flip = FALSE,
                           ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  
  layer(
    data = data,
    mapping = mapping,
    stat = "waffle",
    geom = "pictogram",
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      n_rows = n_rows,
      make_proportional = make_proportional,
      flip = flip,
      ...
    )
  )
}

#' @rdname geom_pictogram
#' @export
GeomPictogram <- ggplot2::ggproto(
  `_class` = "GeomPictogram",
  `_inherit` = GeomText,
  
  #  required_aes = c("x", "y", "label", "colour"),
  
  default_aes = aes(
    fill = NA, alpha = NA, colour = "black",
    size = 9, angle = 0, hjust = 0.5, vjust = 0.5,
    family = "FontAwesome5Free-Solid", fontface = 1, lineheight = 1
  ),
  
  
  draw_group = function(self, data, panel_params, coord,
                        n_rows = 10, make_proportional = FALSE, flip = FALSE,
                        radius = grid::unit(0, "npc")) {
    
    # msg("Called => GeomPictogram::draw_group()")
    
    coord <- ggplot2::coord_equal()
    grobs <- GeomText$draw_panel(data, panel_params, coord, parse = FALSE, check_overlap = FALSE)
    
    # msg("Done With => GeomPictogram::draw_group()")
    
    ggname("geom_pictogram", grid::grobTree(children = grobs))
    
  },
  
  
  draw_panel = function(self, data, panel_params, coord,
                        n_rows = 10, make_proportional = FALSE, flip = FALSE, ...) {
    
    # msg("Called => GeomPictogram::draw_panel()")
    # print(str(data, 1))
    
    coord <- ggplot2::coord_equal()
    grobs <- GeomText$draw_panel(data, panel_params, coord, parse = FALSE, check_overlap = FALSE)
    
    # msg("Done With => GeomPictogram::draw_panel()")
    
    ggname("geom_pictogram", grid::grobTree(children = grobs))
    
  },
  
  draw_key = draw_key_pictogram
  
)

geom_rtile <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       radius = grid::unit(6, "pt"),
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRtile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      radius = radius,
      na.rm = na.rm,
      ...
    )
  )
}

GeomRtile <- ggplot2::ggproto("GeomRtile", GeomRrect,
                              
                              extra_params = c("na.rm", "width", "height"),
                              
                              setup_data = function(data, params) {
                                data$width <- data$width %||% params$width %||% ggplot2::resolution(data$x, FALSE)
                                data$height <- data$height %||% params$height %||% ggplot2::resolution(data$y, FALSE)
                                
                                transform(data,
                                          xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
                                          ymin = y - height / 2, ymax = y + height / 2, height = NULL
                                )
                              },
                              
                              default_aes = ggplot2::aes(
                                fill = "grey20", colour = NA, size = 0.1, linetype = 1, alpha = NA
                              ),
                              
                              required_aes = c("x", "y"),
                              
                              draw_key = ggplot2::draw_key_polygon
                              
)


draw_key_waffle <- function(data, params, size, ...) { # nocov start
  
  # msg("Called => draw_key_waffle()")
  #
  # print(str(data, 1))
  # print(str(params, 1))
  # print(str(size, 1))
  # print(str(list(...), 1))
  
  grid::roundrectGrob(
    r = min(params$radius, unit(3, "pt")),
    default.units = "native",
    width = 0.9, height = 0.9,
    name = "lkey",
    gp = grid::gpar(
      col = params[["color"]][[1]] %l0% params[["colour"]][1] %l0% data[["colour"]][[1]] %l0% "#00000000",
      fill = alpha(data$fill %||% data$colour %||% "grey20", data$alpha),
      lty = data$linetype %||% 1
    )
  )
} # nocov end

#' Waffle (Square pie chart) Geom
#'
#' There are two special/critical `aes()` mappings:
#' - `fill` (so the geom knows which column to map the fills to)
#' - `values` (which column you're mapping the filling for the squares with)
#'
#' @md
#' @param mapping Set of aesthetic mappings created by `aes()` or
#'   `aes_()`. If specified and `inherit.aes = TRUE` (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply `mapping` if there is no plot mapping.
#' @param n_rows how many rows should there be in the waffle chart? default is 10
#' @param flip If `TRUE`, flip x and y coords. n_rows then becomes n_cols.
#'     Useful to achieve waffle column chart effect. Defaults is `FALSE`.
#' @param make_proportional compute proportions from the raw values? (i.e. each
#'        value `n` will be replaced with `n`/`sum(n)`); default is `FALSE`.
#' @param radius radius for round squares
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to `ggplot()`.
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    `fortify()` for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame.`, and
#'    will be used as the layer data.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. `borders()`.
#' @param geom geom to use (default is "waffle")
#' @param ... other arguments passed on to `layer()`. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `color = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#' @export
#' @examples
#' data.frame(
#'   parts = factor(rep(month.abb[1:3], 3), levels=month.abb[1:3]),
#'   vals = c(10, 20, 30, 6, 14, 40, 30, 20, 10),
#'   fct = c(rep("Thing 1", 3), rep("Thing 2", 3), rep("Thing 3", 3))
#' ) -> xdf
#'
#' ggplot(xdf, aes(fill = parts, values = vals)) +
#'   geom_waffle() +
#'   facet_wrap(~fct)
geom_waffle <- function(mapping = NULL, data = NULL,
                        n_rows = 10, make_proportional = FALSE, flip = FALSE,
                        na.rm = NA, show.legend = NA,
                        radius = grid::unit(0, "npc"),
                        inherit.aes = TRUE, ...) {
  
  # msg("Called => geom_waffle::geom_waffle()")
  # msg("Done With => geom_waffle::geom_waffle()")
  
  layer(
    stat = StatWaffle,
    data = data,
    mapping = mapping,
    geom = GeomWaffle,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.param = FALSE,
    params = list(
      na.rm = na.rm,
      n_rows = n_rows,
      make_proportional = make_proportional,
      flip = flip,
      radius = radius,
      ...
    )
  )
}

#' @rdname geom_waffle
#' @export
GeomWaffle <- ggplot2::ggproto(
  `_class` = "GeomWaffle",
  `_inherit` = GeomRtile,
  
  default_aes = ggplot2::aes(
    fill = NA, alpha = NA, colour = NA,
    size = 0.125, linetype = 1, width = NA, height = NA
  ),
  
  draw_group = function(self, data, panel_params, coord,
                        n_rows = 10, make_proportional = FALSE, flip = FALSE,
                        radius = grid::unit(0, "npc")) {
    
    # msg("Called => GeomWaffle::draw_group()")
    
    coord <- ggplot2::coord_equal()
    grobs <- GeomRtile$draw_panel(data, panel_params, coord, radius)
    
    # msg("Done With => GeomWaffle::draw_group()")
    
    ggname("geom_waffle", grid::grobTree(children = grobs))
    
  },
  
  
  draw_panel = function(self, data, panel_params, coord,
                        n_rows = 10, make_proportional = FALSE, flip = FALSE,
                        radius = grid::unit(0, "npc")) {
    
    # msg("Called => GeomWaffle::draw_panel()")
    
    coord <- ggplot2::coord_equal()
    
    # grid::gList(
    grobs <- GeomRtile$draw_panel(data, panel_params, coord, radius)
    # ) -> grobs
    
    # msg("Done With => GeomWaffle::draw_panel()")
    
    ggname("geom_waffle", grid::grobTree(children = grobs))
    
  },
  
  draw_key = draw_key_waffle
  
)

#' Veritical, left-aligned layout for waffle plots
#'
#' Left-align the waffle plots by x-axis. Use the \code{pad} parameter in
#' \code{waffle} to pad each plot to the max width (num of squares), otherwise
#' the plots will be scaled.
#'
#' @param ... one or more waffle plots
#' @export
#' @examples
#' parts <- c(80, 30, 20, 10)
#' w1 <- waffle(parts, rows=8)
#' w2 <- waffle(parts, rows=8)
#' w3 <- waffle(parts, rows=8)
#' # print chart
#' ## iron(w1, w2, w3)
iron <- function(...) {
  grob_list <- list(...)
  grid.newpage()
  grid.draw(do.call("rbind_gtable_max", lapply(grob_list, ggplotGrob)))
}

# # @rdname geom_waffle
# # @export
# stat_waffle<- function(mapping = NULL, data = NULL,
#                        n_rows = 10, make_proportional = FALSE,
#                        na.rm = NA, show.legend = NA,
#                        inherit.aes = TRUE, ...) {
#
#   layer(
#     stat = StatWaffle,
#     data = data,
#     mapping = mapping,
#     geom = "waffle",
#     position = "identity",
#     show.legend = show.legend,
#     inherit.aes = inherit.aes,
#     params = list(
#       na.rm = na.rm,
#       n_rows = n_rows,
#       make_proportional = make_proportional,
#       ...
#     )
#   )
# }
#
# # @rdname geom_waffle
# # @export
# StatWaffle <- ggplot2::ggproto(
#   `_class` = "StatWaffle",
#   `_inherit` = ggplot2::Stat,
#
#   required_aes = c("fill", "values"),
#
#   compute_layer = function(self, data, params, panels) {
#
#     if (inherits(data[["fill"]], "factor")) {
#       flvls <- levels(data[["fill"]])
#     } else {
#       flvls <- levels(factor(data[["fill"]]))
#     }
#
#     p <- split(data, data$PANEL)
#
#     lapply(p, function(.x) {
#
#       parts_vec <- unlist(sapply(1:length(.x[["fill"]]), function(i) {
#         rep(as.character(.x[["fill"]][i]), .x[["values"]][i])
#       }))
#
#       pgrp_vec <- unlist(sapply(1:length(.x[["fill"]]), function(i) {
#         rep(.x$group, .x[["values"]][i])
#       }))
#
#       expand.grid(
#         y = 1:params$n_rows,
#         x = seq_len((ceiling(sum(.x[["values"]]) / params$n_rows)))#,
#         # stringsAsFactors = FALSE
#       ) -> tdf
#
#       parts_vec <- c(parts_vec, rep(NA, nrow(tdf)-length(parts_vec)))
#
#       # tdf$parts <- parts_vec
#       tdf[["values"]] <- NA
#       tdf[["fill"]] <- parts_vec
#       tdf[["PANEL"]] <- .x[["PANEL"]][1]
#       tdf[["group"]] <- 1:nrow(tdf)
#
#       tdf <- tdf[sapply(tdf[["fill"]], function(x) !is.na(x)),]
#
#     }) -> p
#
#     p <- plyr::rbind.fill(p)
#     p[["fill"]] <- factor(p[["fill"]], levels=flvls)
#
#     # print(str(p))
#
#     p
#
#   },
#
#   compute_panel = function(self, data, scales, na.rm = FALSE,
#                            n_rows = 10, make_proportional = FALSE) {
#
#     # message("Called STAT compute_panel()")
#
#     ggproto_parent(Stat, self)$compute_panel(data, scales,
#                                              n_rows = 10,
#                                              make_proportional = FALSE)
#
#   }
#
# )

#' @rdname geom_waffle
#' @export
stat_waffle <- function(mapping = NULL, data = NULL, geom = "waffle",
                        n_rows = 10, make_proportional = FALSE, flip = FALSE,
                        radius = grid::unit(0, "npc"),
                        na.rm = NA, show.legend = NA,
                        inherit.aes = TRUE, ...) {
  
  # msg("Called => stat_waffle::stat_waffle()")
  # msg("Done With => stat_waffle::stat_waffle()")
  
  layer(
    stat = StatWaffle,
    data = data,
    mapping = mapping,
    geom = geom,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.param = FALSE,
    params = list(
      na.rm = na.rm,
      n_rows = n_rows,
      make_proportional = make_proportional,
      flip = flip,
      radius = radius,
      ...
    )
  )
}

#' @rdname geom_waffle
#' @export
StatWaffle <- ggplot2::ggproto(
  
  `_class` = "StatWaffle",
  `_inherit` = ggplot2::Stat,
  
  extra_params = c("na.rm", "n_rows", "make_proportional", "flip", "radius"),
  
  required_aes = c("fill", "values", "colour", "label"),
  
  setup_params = function(data, params) {
    # msg("Called => StatWaffle::setup_params()")
    # msg("Done With => StatWaffle::setup_params()")
    params
  },
  
  setup_data = function(data, params) {
    
    # msg("Called => StatWaffle::setup_data()")
    #
    # print(str(data, 1))
    # print(str(params, 1))
    
    use <- if ("label" %in% names(data)) "label" else "fill"
    
    if (inherits(data[[use]], "factor")) {
      flvls <- levels(data[[use]])
    } else {
      flvls <- levels(factor(data[[use]]))
    }
    
    if (inherits(data[["colour"]], "factor")) {
      clvls <- levels(data[["colour"]])
    } else {
      clvls <- levels(factor(data[["colour"]]))
    }
    
    if (!("colour" %in% names(data))) {
      if ("colour" %in% names(params)) {
        data[["colour"]] <- params[["colour"]]
      } else {
        data[["colour"]] <- "white"
      }
      clvls <- levels(factor(data[["colour"]]))
    } else {
      if (any(is.na(as.character(data[["colour"]])))) {
        data[["colour"]] <- "white"
        clvls <- levels(factor(data[["colour"]]))
      } else {
        data[["colour"]] <- as.character(data[["colour"]])
      }
    }
    
    # msg("       => StatWaffle::setup_data() : colour")
    # print(str(data, 1))
    
    p <- split(data, data$PANEL)
    
    lapply(p, function(.x) {
      
      if (params[["make_proportional"]]) {
        .x[["values"]] <- .x[["values"]] / sum(.x[["values"]])
        .x[["values"]] <- round_preserve_sum(.x[["values"]], digits = 2)
        .x[["values"]] <- as.integer(.x[["values"]] * 100)
      }
      
      parts_vec <- unlist(sapply(1:length(.x[[use]]), function(i) {
        rep(as.character(.x[[use]][i]), .x[["values"]][i])
      }))
      
      pgrp_vec <- unlist(sapply(1:length(.x[[use]]), function(i) {
        rep(.x[["group"]], .x[["values"]][i])
      }))
      
      # print(str(.x, 1))
      
      colour_vec <- unlist(sapply(1:length(.x[[use]]), function(i) {
        rep(.x[["colour"]][i], .x[["values"]][i])
      }))
      
      expand.grid(
        y = 1:params$n_rows,
        x = seq_len((ceiling(sum(.x[["values"]]) / params$n_rows)))#,
        # stringsAsFactors = FALSE
      ) -> tdf
      
      parts_vec <- c(parts_vec, rep(NA, nrow(tdf)-length(parts_vec)))
      colour_vec <- c(colour_vec, rep(NA, nrow(tdf)-length(colour_vec)))
      
      # tdf$parts <- parts_vec
      tdf[["values"]] <- NA
      tdf[["colour"]] <- colour_vec
      tdf[[use]] <- parts_vec
      tdf[["PANEL"]] <- .x[["PANEL"]][1]
      tdf[["group"]] <- 1:nrow(tdf)
      
      tdf <- tdf[sapply(tdf[[use]], function(x) !is.na(x)),]
      
    }) -> p
    
    p <- plyr::rbind.fill(p)
    p[[use]] <- factor(p[[use]], levels=flvls)
    p[["colour"]] <- factor(p[["colour"]], levels = clvls)
    
    # print(str(p, 1))
    #
    # msg("Done With => StatWaffle::setup_data()")
    # data
    
    wdat <- p
    
    if (params$flip) {
      x_temp <- wdat$x
      wdat$x <- wdat$y
      wdat$y <- x_temp
      x_temp <- NULL
    }
    
    wdat$width <- wdat$width %||% params$width %||% ggplot2::resolution(wdat$x, FALSE)
    wdat$height <- wdat$height %||% params$height %||% ggplot2::resolution(wdat$y, FALSE)
    
    transform(
      wdat,
      xmin = x - width / 2,
      xmax = x + width / 2,
      width = NULL,
      ymin = y - height / 2,
      ymax = y + height / 2,
      height = NULL
    ) -> p
    
    p
    
  },
  
  compute_layer = function(self, data, params, layout) {
    # msg("Called => StatWaffle::compute_layer()")
    # print(str(data, 1))
    # print(str(params, 1))
    # msg("Done With => StatWaffle::compute_layer()")
    data
  },
  
  finish_layer = function(self, data, params) {
    # msg("Called => StatWaffle::finish_layer()")
    # msg("Done With => StatWaffle::finish_layer()")
    data
  },
  
  compute_panel = function(self, data, scales, ...) {
    # msg("Called => StatWaffle::compute_panel()")
    # msg("Done With => StatWaffle::compute_panel()")
    data
  }
  
)

round_preserve_sum <- function(x, digits = 0) {
  up <- 10^digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}


is_missing_arg <- function(x) identical(x, quote(expr = ))

# VIA: http://stackoverflow.com/q/13294952/1457051

rbind_gtable_max <- function(...) {
  
  gtl <- list(...)
  
  stopifnot(all(sapply(gtl, is.gtable)))
  
  bind2 <- function (x, y) {
    
    stopifnot(ncol(x) == ncol(y))
    
    if (nrow(x) == 0) return(y)
    if (nrow(y) == 0) return(x)
    
    y$layout$t <- y$layout$t + nrow(x)
    y$layout$b <- y$layout$b + nrow(x)
    x$layout <- rbind(x$layout, y$layout)
    
    x$heights <- insert_unit(x$heights, y$heights)
    x$rownames <- c(x$rownames, y$rownames)
    x$widths <- unit.pmax(x$widths, y$widths)
    x$grobs <- append(x$grobs, y$grobs)
    
    x
    
  }
  Reduce(bind2, gtl)
  
}

cbind_gtable_max <- function(...) {
  
  gtl <- list(...)
  
  stopifnot(all(sapply(gtl, is.gtable)))
  
  bind2 <- function (x, y) {
    
    stopifnot(nrow(x) == nrow(y))
    
    if (ncol(x) == 0) return(y)
    if (ncol(y) == 0) return(x)
    
    y$layout$l <- y$layout$l + ncol(x)
    y$layout$r <- y$layout$r + ncol(x)
    x$layout <- rbind(x$layout, y$layout)
    
    x$widths <- insert_unit(x$widths, y$widths)
    x$colnames <- c(x$colnames, y$colnames)
    x$heights <- unit.pmax(x$heights, y$heights)
    x$grobs <- append(x$grobs, y$grobs)
    
    x
    
  }
  
  Reduce(bind2, gtl)
  
}

insert_unit <- function (x, values, after = length(x)) {
  
  lengx <- length(x)
  
  if (lengx == 0) return(values)
  if (length(values) == 0) return(x)
  
  if (after <= 0) {
    unit.c(values, x)
  } else if (after >= lengx) {
    unit.c(x, values)
  } else {
    unit.c(x[1L:after], values, x[(after + 1L):lengx])
  }
  
}

# Name ggplot grid object
# Convenience function to name grid objects
#
# @keyword internal
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

"%||%" <- function(a, b) { if (!is.null(a)) a else b }
"%l0%" <- function(a, b) { if (length(a)) a else b }

.pt <- ggplot2::.pt

#' Make waffle (square pie) charts
#'
#' Given a named vector or a data frame, this function will return a ggplot object that
#' represents a waffle chart of the values. The individual values will be
#' summed up and each that will be the total number of squares in the grid.
#' You can perform appropriate value transformation ahead of time to get the
#' desired waffle layout/effect.
#'
#' If a data frame is used, the first two columns should contain the desired names
#' and the values, respectively.
#'
#' If the vector is not named or only partially named, capital letters will be
#' used instead.
#'
#' It is highly suggested that you limit the number of elements
#' to plot, just like you should if you ever got wasted and decided that a
#' regular pie chart was a good thing to create and then decide to be totally
#' evil and make one to pollute this beautiful world of ours.
#'
#' Chart title and x-axis labels are optional, especially if you'll just be
#' exporting to another program for use/display.
#'
#' If you specify a string (vs `FALSE`) to `use_glyph` the function
#' will map the input to a Font Awesome glyph name and use that glyph for the
#' tile instead of a block (making it more like an isotype pictogram than a
#' waffle chart). You'll need to install Font Awesome 5 and use
#' the [`extrafont` package](`https://github.com/wch/extrafont`) to
#' be able to use Font Awesome 5 glyphs. Sizing is also up to the user since
#' fonts do not automatically scale with graphic resize.
#'
#' Glyph idea inspired by Ruben C. Arslan (@@_r_c_a)
#'
#' @md
#' @note You MUST use the Font Awesome 5 fonts bundled with the package.
#'       See [install_fa_fonts()].
#' @param parts named vector of values or a data frame to use for the chart
#' @param rows number of rows of blocks
#' @param keep keep factor levels (i.e. for consistent legends across waffle plots)
#' @param xlab text for below the chart. Highly suggested this be used to
#'     give the "1 sq == xyz" relationship if it's not obvious
#' @param title chart title
#' @param colors exactly the number of colors as values in `parts.`
#'     If omitted, Color Brewer "Set2" colors are used.
#' @param size width of the separator between blocks (defaults to `2`)
#' @param flip flips x & y axes
#' @param reverse reverses the order of the data
#' @param equal by default, waffle uses `coord_equal`; this can cause
#'     layout problems, so you an use this to disable it if you are using
#'     ggsave or knitr to control output sizes (or manually sizing the chart)
#' @param pad how many blocks to right-pad the grid with
#' @param use_glyph use specified glyph; if using built-in Font Awesome, can be
#'        the glyph name; otherwise, it must be the unicode glyph from the custom
#'        font the caller is using.
#' @param glyph_size size of the Font Awesome font
#' @param glyph_font,glyph_font_family if `use_glyph` is not `FALSE`,
#'        the `gplyph_font` will be looked up in the font database and
#'        the `glpyph_font_family` used as the `family` parameter to ggplot for
#'        font display since fonts in R, Python and anythign that relies on
#'        legacy font C libraries are woefully messed up. You may need to adjust
#'        either of these "font" parameters depending on your system & OS version
#'        due to the fact that font names are different even between OS versions
#'        (sometimes).\cr
#'        \cr
#'        The package comes with Font Awesome and helpers for it. Use of any other fonts
#'        requires the caller to be familiar with using fonts in R. NOT ALL FONTS
#'        will work with ggplot2 and definitely not under all graphics devices
#'        for ggplot2.
#' @param legend_pos position of legend
#' @export
#' @examples
#' parts <- c(80, 30, 20, 10)
#' waffle(parts, rows=8)
#'
#' parts <- data.frame(
#'   names = LETTERS[1:4],
#'   vals = c(80, 30, 20, 10)
#' )
#'
#' waffle(parts, rows=8)
#'
#' # library(extrafont)
#' # waffle(parts, rows=8, use_glyph="shield")
#'
#' parts <- c(One=80, Two=30, Three=20, Four=10)
#' chart <- waffle(parts, rows=8)
#' # print(chart)
waffle <- function(parts, rows=10, keep=TRUE, xlab=NULL, title=NULL, colors=NA,
                   size=2, flip=FALSE, reverse=FALSE, equal=TRUE, pad=0,
                   use_glyph = FALSE,
                   glyph_size = 12,
                   glyph_font = "Font Awesome 5 Free Solid",
                   glyph_font_family = "FontAwesome5Free-Solid",
                   legend_pos = "right") {
  
  if (inherits(parts, "data.frame")) {
    stats::setNames(
      unlist(parts[, 2], use.names = FALSE),
      unlist(parts[, 1], use.names = FALSE)
    ) -> parts
  }
  
  # fill in any missing names
  part_names <- names(parts)
  if (length(part_names) < length(parts)) {
    part_names <- c(part_names, LETTERS[1:length(parts) - length(part_names)])
  }
  
  names(parts) <- part_names
  
  # use Set2 if no colors are specified
  if (all(is.na(colors))) colors <- suppressWarnings(brewer.pal(length(parts), "Set2"))
  
  # make one big vector of all the bits
  parts_vec <- unlist(sapply(1:length(parts), function(i) {
    rep(names(parts)[i], parts[i])
  }))
  
  if (reverse) parts_vec <- rev(parts_vec)
  
  # setup the data frame for geom_rect
  dat <- expand.grid(y = 1:rows, x = seq_len(pad + (ceiling(sum(parts) / rows))))
  
  # add NAs if needed to fill in the "rectangle"
  dat$value <- c(parts_vec, rep(NA, nrow(dat) - length(parts_vec)))
  
  if (!inherits(use_glyph, "logical")) {
    
    if (length(use_glyph) == 1L) {
      
      if (grepl("wesom", glyph_font)) {
        fontlab <- .fa_unicode[.fa_unicode[["name"]] == use_glyph, "unicode"]
        dat$fontlab <- c(
          rep(fontlab, length(parts_vec)),
          rep("", nrow(dat) - length(parts_vec)
              # rep(NA, nrow(dat) - length(parts_vec)
          )
        )
      } else {
        dat$fontlab <- c(
          rep(use_glyph, length(parts_vec)),
          rep("", nrow(dat) - length(parts_vec)
              # rep(NA, nrow(dat) - length(parts_vec)
          )
        )
      }
      
    } else if (length(use_glyph) == length(parts)) {
      
      if (grepl("wesom", glyph_font)) {
        fontlab <- .fa_unicode[.fa_unicode[["name"]] %in% use_glyph, "unicode"]
        # fontlab <- .fa_unicode[use_glyph]
        dat$fontlab <- c(
          fontlab[as.numeric(factor(parts_vec, levels = names(parts)))],
          rep("", nrow(dat) - length(parts_vec))
          # rep(NA, nrow(dat) - length(parts_vec))
        )
      } else {
        dat$fontlab <- c(
          use_glyph[as.numeric(factor(parts_vec, levels = names(parts)))],
          # rep(NA, nrow(dat) - length(parts_vec))
          rep("", nrow(dat) - length(parts_vec))
        )
      }
      
    } else if (length(use_glyph) == length(parts_vec)) {
      
      if (grepl("wesom", glyph_font)) {
        fontlab <- .fa_unicode[.fa_unicode[["name"]] %in% use_glyph, "unicode"]
        dat$fontlab <- c(fontlab, rep(NA, nrow(dat) - length(parts_vec)))
      } else {
        dat$fontlab <- c(use_glyph, rep(NA, nrow(dat) - length(parts_vec)))
      }
      
    } else {
      stop("'use_glyph' must have length 1, length(parts), or sum(parts)")
    }
  }
  
  dat$value <- ifelse(is.na(dat$value), " ", dat$value)
  
  if (" " %in% dat$value) part_names <- c(part_names, " ")
  if (" " %in% dat$value) colors <- c(colors, "#00000000")
  
  dat$value <- factor(dat$value, levels = part_names)
  
  gg <- ggplot(dat, aes(x = x, y = y))
  
  if (flip) gg <- ggplot(dat, aes(x = y, y = x))
  
  gg <- gg + theme_bw()
  
  # make the plot
  
  if (inherits(use_glyph, "logical")) {
    
    gg <- gg + geom_tile(aes(fill = value), color = "white", size = size)
    
    gg <- gg + scale_fill_manual(
      name = "",
      values = colors,
      label = part_names,
      na.value = "white",
      drop = !keep
    )
    
    gg <- gg + guides(fill = guide_legend(override.aes = list(colour = "#00000000")))
    
    gg <- gg + theme(legend.background =
                       element_rect(fill = "#00000000", color = "#00000000"))
    
    gg <- gg + theme(legend.key =
                       element_rect(fill = "#00000000", color = "#00000000"))
    
  } else {
    
    if (extrafont::choose_font(glyph_font, quiet = TRUE) == "") {
      stop(
        sprintf(
          "Font [%s] not found. Please install it and use extrafont to make it available to R",
          glyph_font
        ),
        call. = FALSE
      )
    }
    
    load_fontawesome()
    
    gg <- gg + geom_tile(
      color = "#00000000", fill = "#00000000", size = size,
      alpha = 0, show.legend = FALSE
    )
    
    gg <- gg + geom_point(
      aes(color = value), fill = "#00000000", size = 0,
      show.legend = TRUE
    )
    
    gg <- gg + geom_text(
      aes(color = value, label = fontlab),
      family = glyph_font_family,
      size = glyph_size,
      show.legend = FALSE
    )
    
    gg <- gg + scale_color_manual(
      name = NULL,
      values = colors,
      labels = part_names,
      drop = !keep
    )
    
    gg <- gg + guides(color =
                        guide_legend(override.aes = list(shape = 15, size = 7)))
    
    gg <- gg + theme(legend.background =
                       element_rect(fill = "#00000000", color = "#00000000"))
    
    gg <- gg + theme(legend.key = element_rect(color = "#00000000"))
  }
  
  gg <- gg + labs(x = xlab, y = NULL, title = title)
  gg <- gg + scale_x_continuous(expand = c(0, 0))
  gg <- gg + scale_y_continuous(expand = c(0, 0))
  
  if (equal) gg <- gg + coord_equal()
  
  gg <- gg + theme(panel.grid = element_blank())
  gg <- gg + theme(panel.border = element_blank())
  gg <- gg + theme(panel.background = element_blank())
  gg <- gg + theme(panel.spacing = unit(0, "null"))
  
  gg <- gg + theme(axis.text = element_blank())
  gg <- gg + theme(axis.title.x = element_text(size = 10))
  gg <- gg + theme(axis.ticks = element_blank())
  gg <- gg + theme(axis.line = element_blank())
  gg <- gg + theme(axis.ticks.length = unit(0, "null"))
  
  gg <- gg + theme(plot.title = element_text(size = 18))
  
  gg <- gg + theme(plot.background = element_blank())
  gg <- gg + theme(panel.spacing = unit(c(0, 0, 0, 0), "null"))
  
  gg <- gg + theme(legend.position = legend_pos)
  
  gg
  
}

#' Waffle chart theme cruft remover that can be used with any other theme
#'
#' Removes:
#'
#' - panel grid
#' - all axis text
#' - all axis ticks
#' - all axis titles
#'
#' @md
#' @export
theme_enhance_waffle<- function() {
  
  ret <- theme(panel.grid = element_blank())
  ret <- ret + theme(axis.text = element_blank())
  ret <- ret + theme(axis.text.x = element_blank())
  ret <- ret + theme(axis.text.y = element_blank())
  ret <- ret + theme(axis.title = element_blank())
  ret <- ret + theme(axis.title.x = element_blank())
  ret <- ret + theme(axis.title.x.top = element_blank())
  ret <- ret + theme(axis.title.x.bottom = element_blank())
  ret <- ret + theme(axis.title.y = element_blank())
  ret <- ret + theme(axis.title.y.left = element_blank())
  ret <- ret + theme(axis.title.y.right = element_blank())
  
  ret
  
}

#' A package to make waffle charts (square pie charts) in R.
#'
#' For glyphs:\cr
#' \cr
#' Font Awesome by Dave Gandy - http://fontawesome.io\cr
#' License: SIL OFL 1.1\cr
#' URL: http://scripts.sil.org/OFL
#'
#' @name waffle-package
#' @docType package
#' @import gridExtra
#' @import stringr
#' @import curl
#' @import DT
#' @import htmlwidgets
#' @importFrom gtable is.gtable
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot geom_tile scale_fill_manual guides geom_tile ggplotGrob
#' @importFrom ggplot2 geom_point geom_text scale_color_manual guides theme labs
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous coord_equal theme_bw
#' @importFrom ggplot2 aes guide_legend element_rect element_blank element_text layer
#' @importFrom ggplot2 discrete_scale alpha
#' @importFrom grid arrow unit grid.newpage grid.draw unit.c unit.pmax unit.pmin
#' @importFrom grid textGrob gpar grobTree roundrectGrob
#' @importFrom extrafont ttf_import font_import choose_font
#' @importFrom stats setNames
#' @importFrom utils tail
#' @importFrom rlang is_missing
NULL

load_fontawesome <- function() {
  suppressWarnings(
    suppressMessages(
      extrafont::font_import(
        paths = system.file("fonts", package = "waffle"),
        recursive = FALSE,
        prompt = FALSE
      )
    )
  )
}