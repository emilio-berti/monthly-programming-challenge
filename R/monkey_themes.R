monkey_theme <- theme(
  panel.background = element_blank(),
  axis.line = element_line(),
  panel.grid.major = element_line(colour = "gainsboro")
)

monkey_bare <- theme(
  panel.background = element_blank(),
  axis.line = element_line(),
  panel.grid.major = element_line(colour = "gainsboro"),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5)
)

monkey_map <- theme(
  panel.background = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank()
)

monkey_map_species <- theme(
  panel.background = element_blank(),
  legend.position = "none",
  plot.title = element_text(hjust = 0.5, face = "bold.italic"),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank()
)

fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "10^", l)
  # remove '+' if present
  if(!is_empty(grep("+", l))){
    l <- gsub("\\+", "", l)
  }
  # return this as an expression
  parse(text=l)
}
