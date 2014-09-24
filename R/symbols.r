
#' Various handy symbols to use in a command line UI
#'
#' @usage
#' signs
#'
#' @format A named list, see \code{names(signs)} for all sign names.
#'
#' @details
#'
#' On Windows they have a fallback to less fancy symbols.
#'
#' @aliases signs
#' @export signs
#'
#' @examples
#' cat(signs$tick, " SUCCESS\n", signs$cross, " FAILURE\n", sep="")
#'
#' ## All signs
#' cat(paste(format(names(signs), width=20),
#'   unlist(signs)), sep = "\n")

signs <- list(
  "check" = '\u2714',
  "cross" = '\u2716',
  "star" = '\u2605',
  "block" = '\u2587',
  "white_square" = '\u25FB',
  "black_square" = '\u25FC',
  "circle" = '\u25EF',
  "fisheye" = '\u25C9',
  "dotted_circle" = '\u25CC',
  "bullseye" = '\u25CE',
  "circled_o" = '\u24DE',
  "circled_x" = '\u24E7',
  "circled_I" = '\u24be',
  "circled_?" = '?\u20DD',
  "black_circle" = '\u25CF',
  "dot" = '\u2024',
  "line" = '\u2500',
  "ellipsis" = '\u2026',
  "pointer" = '\u276F',
  "info" = '\u2139',
  "warning" = '\u26A0',
  "menu" = '\u2630',
  "smiley" = '\u263A',
  "mustache" = '\u0DF4',
  "heart" = '\u2665',
  "arrow_up" = '\u2191',
  "arrow_down" = '\u2193',
  "arrow_left" = '\u2190',
  "arrow_right" = '\u2192',
  "radio_on" = '\u25C9',
  "radio_off" = '\u25EF',
  "checkbox_on" = '\u2612',
  "checkbox_off" = '\u2610',
  "checkbox_circle_on" = '\u24E7',
  "checkbox_circle_off" = '\u24BE'
)

signs_win <- list(
  "check" = '\u221A',
  "cross" = '\u256b',
  "star" = '*',
  "block" = '\u2588',
  "white_square" = '[ ]',
  "black_square" = '[\u2588]',
  "circle" = '( )',
  "fisheye" = '(\u00f2)',
  "dotted_circle" = '( )',
  "bullseye" = '( )',
  "circled_o" = '(\u25CB)',
  "circled_x" = '(\u256b)',
  "circled_I" = '(|)',
  "circled_?" = '(?)',
  "black_circle" = '\u00f2',
  "dot" = '.',
  "line" = '\u2500',
  "ellipsis" = '...',
  "pointer" = '>',
  "info" = 'i',
  "warning" = '\u203C',
  "menu" = '\u2261',
  "smiley" = '\u263A',
  "mustache" = '\u250C\u2500\u2510',
  "heart" = signs$heart,
  "arrow_up" = signs$arrowUp,
  "arrow_down" = signs$arrowDown,
  "arrow_left" = signs$arrowLeft,
  "arrow_right" = signs$arrowRight,
  "radio_on" = '(\u00f2)',
  "radio_off" = '( )',
  "checkbox_on" = '[\u256b]',
  "checkbox_off" = '[ ]',
  "checkbox_circle_on" = '(\u256b)',
  "checkbox_circle_off" = '( )'
)
