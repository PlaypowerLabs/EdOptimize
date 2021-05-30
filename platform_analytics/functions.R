box <- function (..., title = NULL, footer = NULL, status = NULL, solidHeader = FALSE, 
          background = NULL, width = 6, height = NULL, collapsible = FALSE, 
          collapsed = FALSE) 
{
  boxClass <- "box"
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    validateStatus(status)
    boxClass <- paste0(boxClass, " box-", status)
  }
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  if (!is.null(background)) {
    validateColor(background)
    boxClass <- paste0(boxClass, " bg-", background)
  }
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", validateCssUnit(height))
  }
  titleTag <- NULL
  if (!is.null(title)) {
    titleTag <- h3(class = "box-title", style = 'text-align:center;', title)
  }
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- status %OR% "default"
    collapseIcon <- if (collapsed) 
      "plus"
    else "minus"
    collapseTag <- div(class = "box-tools pull-right", tags$button(class = paste0("btn btn-box-tool"), 
                                                                   `data-widget` = "collapse", shiny::icon(collapseIcon)))
  }
  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- div(class = "box-header", titleTag, collapseTag)
  }
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), div(class = boxClass, style = if (!is.null(style)) 
      style, headerTag, div(class = "box-body", ...), if (!is.null(footer)) 
        div(class = "box-footer", footer)))
}

validColors <- c("red", "yellow", "aqua", "blue", "light-blue", "green",
                 "navy", "teal", "olive", "lime", "orange", "fuchsia",
                 "purple", "maroon", "black")

validateColor <- function(color) {
  if (color %in% validColors) {
    return(TRUE)
  }
  
  stop("Invalid color: ", color, ". Valid colors are: ",
       paste(validColors, collapse = ", "), ".")
}

tagAssert <- function(tag, type = NULL, class = NULL, allowUI = TRUE) {
  if (!inherits(tag, "shiny.tag")) {
    print(tag)
    stop("Expected an object with class 'shiny.tag'.")
  }
  
  # Skip dynamic output elements
  if (allowUI &&
      (hasCssClass(tag, "shiny-html-output") ||
       hasCssClass(tag, "shinydashboard-menu-output"))) {
    return()
  }
  
  if (!is.null(type) && tag$name != type) {
    stop("Expected tag to be of type ", type)
  }
  
  if (!is.null(class)) {
    if (is.null(tag$attribs$class)) {
      stop("Expected tag to have class '", class, "'")
      
    } else {
      tagClasses <- strsplit(tag$attribs$class, " ")[[1]]
      if (!(class %in% tagClasses)) {
        stop("Expected tag to have class '", class, "'")
      }
    }
  }
}

hasCssClass <- function(tag, class) {
  if (is.null(tag$attribs) || is.null(tag$attribs$class))
    return(FALSE)

  classes <- strsplit(tag$attribs$class, " +")[[1]]
  return(class %in% classes)
}


infoBox <- function (title, value = NULL, subtitle = NULL, icon = shiny::icon("bar-chart"), 
                     color = "aqua", width = 4, href = NULL, fill = FALSE) 
{
  validateColor(color)
  tagAssert(icon, type = "i")
  colorClass <- paste0("bg-", color)
  boxContent <- div(class = "info-box", class = if (fill) 
    colorClass, span(class = "info-box-icon", class = if (!fill) 
      colorClass, icon), div(class = "info-box-content", span(class = "info-box-text", 
                                                              title), if (!is.null(value)) 
                                                                span(class = "info-box-number", value), if (!is.null(subtitle)) 
                                                                  p(subtitle)))
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), boxContent)
}

infoBoxOutput <- function (outputId, width = 4) 
{
  shiny::uiOutput(outputId, class = paste0("col-sm-", width))
}

is.promising <- function(x) {
  inherits(x, "promise") || inherits(x, "Future")
}

renderInfoBox <- function (expr, env = parent.frame(), quoted = FALSE) 
{
  vbox_fun <- shiny::exprToFunction(expr, env, quoted)
  shiny::renderUI({
    vbox <- vbox_fun()
    if (is.promising(vbox)) {
      vbox %...T>% tagAssert(type = "div") %...>% {
        .$children[[1]]
      }
    }
    else {
      tagAssert(vbox, type = "div")
      vbox$children[[1]]
    }
  })
}

break_col_func <- function(data, columns){
  colfunc <- colorRampPalette(c('#f7fbff','#deebf7','#c6dbef','#9ecae1','#6baed6'))
  colfunc_1 <- colorRampPalette(c('#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c'))
  
  brks <- lapply(columns, function(x) quantile(data[[x]], probs = seq(.05, .95, .05), na.rm = TRUE))
  brks <- setNames(brks, columns)
  clrs <- lapply(columns, function(x) colfunc(length(brks[[x]]) + 1))
  clrs <- lapply(columns, function(x){
    if(x %in% c("leaving.Student", "bouncing.Student", "leaving.Teacher", "bouncing.Teacher")){
      colfunc_1(length(brks[[x]]) + 1)
    }
    else{
      colfunc(length(brks[[x]]) + 1)
    }
  })
  clrs <- setNames(clrs, columns)
  
  list(brks = brks, clrs = clrs)
}

break_col_by_row_func <- function(data, rows, columns, ncols){
  
  colfunc <- colorRampPalette(c('#f7fbff','#deebf7','#c6dbef','#9ecae1','#6baed6'))
  brks <- lapply(rows, function(x) quantile(as.numeric(data[which(data$Feature == x), 2:ncols]), probs = seq(.05, 0.95, .05), na.rm = TRUE))
  clrs <- colfunc(length(brks[[1]]) + 1)
  
  values <- rep(list(list()), length(rows))
  colors <- rep(list(list()), length(rows))
  
  for(i in 1:length(rows)){
    values[[i]] <- as.numeric(data[which(data$Feature == rows[i]), 2:ncols])
    for(j in 1:(ncols-1)){
      dist <- 9999999999
      for(k in 1:length(brks[[i]])){
        x <- unname(abs(values[[i]][j] - brks[[i]][k]))
        if(x < dist){
          l <- k
          dist <- x
        }
      }
      colors[[i]][j] <- clrs[l]
    }
  }
  
  col_df <- as.data.frame(matrix(unlist(colors), nrow = length(rows), ncol = (ncols - 1), byrow = TRUE, dimnames = list(rows, columns)))
  val_df <- as.data.frame(matrix(unlist(values), nrow = length(rows), ncol = (ncols - 1), byrow = TRUE, dimnames = list(rows, columns)))
  
  list(colors_data = col_df, values_data = val_df)
}

integer_breaks <- function(n = 5, ...) {
  breaker <- pretty_breaks(n, ...)
  function(x) {
    breaks <- breaker(x)
    union(floor(breaks), ceiling(breaks))
    # breaks[breaks == union(floor(breaks), ceiling(breaks))]
  }
}


MydateRangeInput <- function(inputId, label, start = NULL, end = NULL,
                                 min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month",
                                 minviewmode="months", # added manually
                                 weekstart = 0, language = "en", separator = " to ", width = NULL) {

  # If start and end are date objects, convert to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(start, "Date"))  start <- format(start, "%Y-%m-%d")
  if (inherits(end,   "Date"))  end   <- format(end,   "%Y-%m-%d")
  if (inherits(min,   "Date"))  min   <- format(min,   "%Y-%m-%d")
  if (inherits(max,   "Date"))  max   <- format(max,   "%Y-%m-%d")

  htmltools::attachDependencies(
    div(id = inputId,
        class = "shiny-date-range-input form-group shiny-input-container",
        style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),

        shinyInputLabel(inputId, label),
        # input-daterange class is needed for dropdown behavior
        div(class = "input-daterange input-group",
            tags$input(
              class = "input-sm form-control",
              type = "text",
              `data-date-language` = language,
              `data-date-weekstart` = weekstart,
              `data-date-format` = format,
              `data-date-start-view` = startview,
              `data-date-min-view-mode` = minviewmode, # added manually
              `data-min-date` = min,
              `data-max-date` = max,
              `data-initial-date` = start
            ),
            span(class = "input-group-addon", separator),
            tags$input(
              class = "input-sm form-control",
              type = "text",
              `data-date-language` = language,
              `data-date-weekstart` = weekstart,
              `data-date-format` = format,
              `data-date-start-view` = startview,
              `data-date-min-view-mode` = minviewmode, # added manually
              `data-min-date` = min,
              `data-max-date` = max,
              `data-initial-date` = end
            )
        )
    ),
    datePickerDependency
  )
}

shinyInputLabel <- function(inputId, label = NULL) {
  tags$label(
    label,
    class = "control-label",
    class = if (is.null(label)) "shiny-label-null",
    `for` = inputId
  )
}

datePickerDependency <- htmlDependency(
  "bootstrap-datepicker", "1.6.4", c(href = "shared/datepicker"),
  script = "js/bootstrap-datepicker.min.js",
  stylesheet = "css/bootstrap-datepicker3.min.css",
  # Need to enable noConflict mode. See #1346.
  head = "<script>
(function() {
  var datepicker = $.fn.datepicker.noConflict();
  $.fn.bsDatepicker = datepicker;
})();
</script>"
)