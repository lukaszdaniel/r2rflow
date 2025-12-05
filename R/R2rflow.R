#' @import codetools
NULL

#' @title Convert R code to rflow file
#'
#' @description Takes a file or text argument and prints out rflow xml
#' @author Lukasz Daniel
#' @param file A file containing R code.
#' @param text A text containing R code.
#' @param max.level Non-negative integer or NA value. Maximum level to which diagram may be expanded. NA means no limit.
#' @param connect.top If TRUE, top-level elements should be connected by edges.
#' @param output Output file (.rflow). If missing and file given, same filename but .rflow extension.
#' @return XML-formatted R code
#' @export
R2rflow <- function(text = NULL, file = NULL, output = NULL, max.level = NA, connect.top = FALSE) {

  if(!is.na(max.level) && (!is.integer(max.level) || (max.level < 0L)))
    
    stop("'max.level' argument must be non-negative integer or NA value", domain = "R-R2rflow")

  if (missing(text) && (missing(file) || nchar(file) == 0L)) {
    stop("No R code to convert", domain = "R-R2rflow")
  }
  else if(!missing(text) && !missing(file)) {
    stop("Cannot specify both 'text' and 'file' arguments", domain = "R-R2rflow")
  }
  else if(!missing(text) && missing(output)) {
    stop("'output' argument must be specified", domain = "R-R2rflow")
  }
  else if(!missing(file) && missing(output)) {
    output <- sub("(\\.[RrSq])?$", "\\.rflow", file)
  }

  codes <- parse(text = text, file = file)
  env <- new.env()

  functionSet <- function(v, e) assign(v, e, envir = env)
  functionGet <- function(v, e) get(v, envir = env)

  w <- makeCodeWalker(call = functionCall, leaf = functionLeaf,
                      set = functionSet, get = functionGet)

  edge <- NULL
  w$set("ypos", length(edge))
  w$set("nodeID", 0L)
  w$set("output", output)
  w$set("level", 0L)
  w$set("max.level", max.level)

  Preamble(output)
  cat('<rflow>\n', append = TRUE, file = output)
  cat(' <graph version="0.7" width="894" height="742" locationtype="a" offsetx="0" offsety="0">\n', append = TRUE, file = output)
  Settings(output)

  # Walk code
  if(is.na(max.level) || (max.level > 0L)) {
    w$set("level", w$get("level") + 1L)
    for(i in seq_along(codes)) {
      edge <- c(edge, w$get("nodeID") + 1L)
      w$set("ypos", length(edge))
      walkCode(codes[[i]], w)
    }
    w$set("level", w$get("level") - 1L)
  } else {
    edge <- c(edge, w$get("nodeID") + 1L)
    w$set("ypos", length(edge))
    FreeNodeModel(codes, w)
  }

  if(connect.top) PrintEdges(edge, file = output)
  cat(' </graph>\n', append = TRUE, file = output)
  Task(output)
  cat('</rflow>\n', append = TRUE, file = output)
}


functionCall <- function(e, w) {

  w$set("nodeID", w$get("nodeID") + 1L)

  if(!is.na(w$get("max.level")) && (w$get("level") >= w$get("max.level"))) {
    FreeNodeModel(e, w)
    return()
  }

  head <- e[[1]]

  if ((head == "<-") &&
      (length(e[[3]]) == 4L) &&
      (e[[3]][[1]] == "function"))
  {
    FunctionNodeModel(e, w)
  }
  else if (head == "if") {
    IfNodeModel(e, w)
  }
  else if (head == "for") {
    ForNodeModel(e, w)
  }
  else if (head == "while") {
    WhileNodeModel(e, w)
  }
  else if (head == "repeat") {
    RepeatNodeModel(e, w)
  }
  else if (CurlyBracket(head)) {
    SubflowNodeModel(e, w)
  }
  else {
    FreeNodeModel(e, w)
  }
}

functionLeaf <- function(e, w) {
  w$set("nodeID", w$get("nodeID") + 1L)
  FreeNodeModel(e, w)
  NULL
}

process_subflow <- function(w, body_exprs, output) {

  x <- 688L
  y <- 700L
  cat("  <subflow>\n", append = TRUE, file = output)
  # SubflowNodeModel has custom offsetx="100" offsety="50". For now put 0,0.
  cat(sprintf('   <graph version="0.7" width="%d" height="%d" locationtype="a" offsetx="0" offsety="0">\n', x, y), append = TRUE, file = output)

  # Tunnel IN
  w$set("nodeID", w$get("nodeID") + 1L)
  In <- w$get("nodeID")
  edge <- In
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "in", w = w)

  # Process body
  w$set("level", w$get("level") + 1L)
  for (expr in body_exprs) {
    if (!missing(expr) && !CurlyBracket(deparseTreeExp(expr))) {
      edge <- c(edge, w$get("nodeID") + 1L)
      w$set("ypos", length(edge))
      walkCode(expr, w)
    }
  }
  w$set("level", w$get("level") - 1L)

  # Tunnel OUT
  w$set("nodeID", w$get("nodeID") + 1L)
  Out <- w$get("nodeID")
  edge <- c(edge, Out)
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "out", w = w)

  # Edges
  if(length(body_exprs) > 0L) PrintEdges(edge, file = output)
  cat("   </graph>\n", append = TRUE, file = output)
  cat("  </subflow>\n", append = TRUE, file = output)
}

FunctionNodeModel <- function(e, w) {

  xpos <- 440L
  ypos <- w$get("ypos") * 80L

  output <- w$get("output")

  cat(sprintf('<node id="%d" date="%s" uid="%s" x="%d" y="%d">\n', w$get("nodeID"), crdate(), uid(), xpos, ypos), append = TRUE, file = output)
  cat(sprintf(' <command>%s</command>\n', deparseTreeExp(e)), append = TRUE, file = output)
  cat(" <property/>\n", append = TRUE, file = output)
  cat(' <option type="com.ef_prime.rflow.node.base.FunctionNodeModel">\n', append = TRUE, file = output)
  cat(sprintf('  <entry key="function">%s</entry>\n', deparseTreeExp(e[[2]])), append = TRUE, file = output)
  cat(sprintf('  <entry key="args">(%s)</entry>\n', extract_args2(deparseTreeExp(e[[3]][[2]]))), append = TRUE, file = output)

  body_exprs <- extract_body_list(e[[3]][[3]])
  process_subflow(w, body_exprs, output)

  cat(" </option>\n", append = TRUE, file = output)
  cat("</node>\n", append = TRUE, file = output)
}

IfNodeModel <- function(e, w) {

  xpos <- 440L
  ypos <- w$get("ypos") * 80L
  output <- w$get("output")

  cat(sprintf('<node id="%d" date="%s" uid="%s" x="%d" y="%d">\n', w$get("nodeID"), crdate(), uid(), xpos, ypos), append = TRUE, file = output)
  cat(sprintf(' <command>%s</command>\n', deparseTreeExp(e)), append = TRUE, file = output)
  cat(' <property/>\n', append = TRUE, file = output)
  cat(' <option type="com.ef_prime.rflow.node.base.IfNodeModel">\n', append = TRUE, file = output)
  cat(sprintf('  <entry key="condition">%s</entry>\n', deparseTreeExp(e[[2]])), append = TRUE, file = output)

  if_body <- extract_body_list(e[[3]])
  process_subflow(w, if_body, output)

  if (length(e) == 4L) {
    else_body <- extract_body_list(e[[4]])
    process_subflow(w, else_body, output)
  } else {
    # No else: create empty in/out
    process_subflow(w, list(), output)
  }

  cat(' </option>\n', append = TRUE, file = output)
  cat('</node>\n', append = TRUE, file = output)

}

ForNodeModel <- function(e, w) {

  xpos <- 440L
  ypos <- w$get("ypos") * 80L
  output <- w$get("output")
  cat(sprintf('<node id="%d" date="%s" uid="%s" x="%d" y="%d">\n', w$get("nodeID"), crdate(), uid(), xpos, ypos), append = TRUE, file = output)
  cat(sprintf(' <command>%s</command>\n', deparseTreeExp(e)), append = TRUE, file = output)
  cat(" <property/>\n", append = TRUE, file = output)
  cat(' <option type="com.ef_prime.rflow.node.base.LoopNodeModel">\n', append = TRUE, file = output)
  cat('  <entry key="loop">for</entry>\n', append = TRUE, file = output)
  cat(sprintf('  <entry key="condition">%s</entry>\n', paste(deparseTreeExp(e[[2]]), " in ", deparseTreeExp(e[[3]]), sep = "", collase = "")), append = TRUE, file = output)

  body_exprs <- extract_body_list(e[[4]])
  process_subflow(w, body_exprs, output)

  cat(" </option>\n", append = TRUE, file = output)
  cat("</node>\n", append = TRUE, file = output)
}

WhileNodeModel <- function(e, w) {

  xpos <- 440L
  ypos <- w$get("ypos") * 80L
  output <- w$get("output")
  cat(sprintf('<node id="%d" date="%s" uid="%s" x="%d" y="%d">\n', w$get("nodeID"), crdate(), uid(), xpos, ypos), append = TRUE, file = output)
  cat(sprintf(' <command>%s</command>\n', deparseTreeExp(e)), append = TRUE, file = output)
  cat(" <property/>\n", append = TRUE, file = output)
  cat(' <option type="com.ef_prime.rflow.node.base.LoopNodeModel">\n', append = TRUE, file = output)
  cat('  <entry key="loop">while</entry>\n', append = TRUE, file = output)
  cat(sprintf('  <entry key="condition">%s</entry>\n', deparseTreeExp(e[[2]])), append = TRUE, file = output)

  body_exprs <- extract_body_list(e[[3]])
  process_subflow(w, body_exprs, output)

  cat(" </option>\n", append = TRUE, file = output)
  cat("</node>\n", append = TRUE, file = output)
}

RepeatNodeModel <- function(e, w) {

  xpos <- 440L
  ypos <- w$get("ypos") * 80L
  output <- w$get("output")
  cat(sprintf('<node id="%d" date="%s" uid="%s" x="%d" y="%d">\n', w$get("nodeID"), crdate(), uid(), xpos, ypos), append = TRUE, file = output)
  cat(sprintf(' <command>%s</command>\n', deparseTreeExp(e)), append = TRUE, file = output)
  cat(" <property/>\n", append = TRUE, file = output)
  cat(' <option type="com.ef_prime.rflow.node.base.LoopNodeModel">\n', append = TRUE, file = output)
  cat('  <entry key="loop">repeat</entry>\n', append = TRUE, file = output)

  body_exprs <- extract_body_list(e[[2]])
  process_subflow(w, body_exprs, output)

  cat(" </option>\n", append = TRUE, file = output)
  cat("</node>\n", append = TRUE, file = output)
}

SubflowNodeModel <- function(e, w) {

  xpos <- 440L
  ypos <- w$get("ypos") * 80L
  output <- w$get("output")
  cat(sprintf('<node id="%d" date="%s" uid="%s" x="%d" y="%d">\n', w$get("nodeID"), crdate(), uid(), xpos, ypos), append = TRUE, file = output)
  cat(sprintf(' <command>%s</command>\n', deparseTreeExp(e)), append = TRUE, file = output)
  cat(" <property/>\n", append = TRUE, file = output)
  cat(' <option type="com.ef_prime.rflow.node.base.SubflowNodeModel">\n', append = TRUE, file = output)

  body_exprs <- as.list(e)
  process_subflow(w, body_exprs, output)

  cat(" </option>\n", append = TRUE, file = output)
  cat("</node>\n", append = TRUE, file = output)
}

FreeNodeModel <- function(e, w) {

  xpos <- 440L
  ypos <- w$get("ypos") * 80L
  output <- w$get("output")

  cat(sprintf('<node id="%d" date="%s" uid="%s" x="%d" y="%d">\n', w$get("nodeID"), crdate(), uid(), xpos, ypos), append = TRUE, file = output)
  cat(sprintf(' <command>%s</command>\n', deparseTreeExp(e)), append = TRUE, file = output)
  cat(" <property/>\n", append = TRUE, file = output)
  cat(' <option type="com.ef_prime.rflow.node.base.FreeNodeModel"/>\n', append = TRUE, file = output)
  cat('</node>\n', append = TRUE, file = output)
}

TunnelNodeModel <- function(type = "in", w) {

  xpos <- 440L
  ypos <- w$get("ypos") * 80L
  output <- w$get("output")

  cat(sprintf('<node id="%d" date="%s" uid="%s" x="%d" y="%d">\n', w$get("nodeID"), crdate(), uid(), xpos, ypos), append = TRUE, file = output)
  cat(' <command></command>\n', append = TRUE, file = output)
  cat(' <property/>\n', append = TRUE, file = output)
  cat(' <option type="com.ef_prime.rflow.node.base.TunnelNodeModel">\n', append = TRUE, file = output)
  cat(sprintf('  <entry key="io">%s</entry>\n', type), append = TRUE, file = output)
  cat(' </option>\n', append = TRUE, file = output)
  cat('</node>\n', append = TRUE, file = output)
}

PrintEdges <- function(x, file) {
  if(length(x) > 1L) {
    for(i in seq_len(length(x) - 1L)) {
      cat(sprintf('<edge from="%d" to="%d"/>\n', x[i], x[i+1L]),
          file = file, append = TRUE)
    }
  }
}

Preamble <- function(output) {
  cat('<?xml version="1.0" encoding="UTF-8"?>\n', append = FALSE, file = output)
  cat('<!DOCTYPE rflow [\n', append = TRUE, file = output)
  cat('<!ENTITY lt "&#38;#60;">\n', append = TRUE, file = output)
  cat('<!ENTITY gt "&#62;">\n', append = TRUE, file = output)
  cat('<!ENTITY amp "&#38;#38;">\n', append = TRUE, file = output)
  cat('<!ENTITY apos "&#39;">\n', append = TRUE, file = output)
  cat('<!ENTITY quot "&#34;">\n', append = TRUE, file = output)
  cat(']>\n', append = TRUE, file = output)
}

Settings <- function(output) {
  cat('  <setting>\n', append = TRUE, file = output)
  cat('   <entry key="OUTPUT_DIRECTORY"></entry>\n', append = TRUE, file = output)
  cat('   <entry key="SAVE_CACHE">false</entry>\n', append = TRUE, file = output)
  cat('   <entry key="FONT_SCREEN">monospace</entry>\n', append = TRUE, file = output)
  cat('   <entry key="TEXT_ENCODING">UTF-8</entry>\n', append = TRUE, file = output)
  cat('   <entry key="LOAD_CACHE">false</entry>\n', append = TRUE, file = output)
  cat('   <entry key="IGNORE_ERROR">false</entry>\n', append = TRUE, file = output)
  cat('   <entry key="SAVE_WORKSPACE"></entry>\n', append = TRUE, file = output)
  cat('   <entry key="OUTPUT_REPORT">true</entry>\n', append = TRUE, file = output)
  cat('   <entry key="RUN_TYPE">2</entry>\n', append = TRUE, file = output)
  cat('   <entry key="OUTPUT_ITEMS">script</entry>\n', append = TRUE, file = output)
  cat('   <entry key="USE_GRID">true</entry>\n', append = TRUE, file = output)
  cat('   <entry key="REPORT_TYPES">pdf,html</entry>\n', append = TRUE, file = output)
  cat('   <entry key="FOLDER">empty</entry>\n', append = TRUE, file = output)
  cat('   <entry key="GRID_DISTANCE2">10</entry>\n', append = TRUE, file = output)
  cat('   <entry key="IMAGE_SIZE">480,480</entry>\n', append = TRUE, file = output)
  cat('   <entry key="FONT_OTHER">sans-serif</entry>\n', append = TRUE, file = output)
  cat('  </setting>\n', append = TRUE, file = output)
  cat('<properties>\n', append = TRUE, file = output)
  cat('  <v key="packages"/>\n', append = TRUE, file = output)
  cat('</properties>\n', append = TRUE, file = output)
}

Task <- function(output) {
  cat(' <task>\n', append = TRUE, file = output)
  cat('  <taskgroup>\n', append = TRUE, file = output)
  cat('   <taskproperty>\n', append = TRUE, file = output)
  cat('    <entry key="title">Task</entry>\n', append = TRUE, file = output)
  cat('   </taskproperty>\n', append = TRUE, file = output)
  cat('  </taskgroup>\n', append = TRUE, file = output)
  cat(' </task>\n', append = TRUE, file = output)
}

extract_body_list <- function(body_expr) {
  if ((length(body_expr) > 1L) && CurlyBracket(body_expr[[1]]))
  {
    return(as.list(body_expr))
  }
  list(body_expr)
}

extract_args <- function(s) {
  # Extract everything inside pairlist(...) or list(...)
  inner <- sub(".*pairlist\\((.*)\\).*", "\\1", s)
  inner <- sub(".*list\\((.*)\\).*", "\\1", inner)

  # Split into individual arguments
  parts <- trimws(strsplit(inside, ",")[[1]])

  # Extract names before '='
  names <- sub("\\s*=.*", "", parts)

  # Remove empty names (can appear if nothing before '=')
  names <- names[names != ""]

  # Return as comma-separated string
  paste(names, collapse = ", ")
}

extract_args2 <- function(s) {
  # Extract everything inside pairlist(...) or list(...)
  inner <- sub(".*pairlist\\((.*)\\).*", "\\1", s)
  inner <- sub(".*list\\((.*)\\).*", "\\1", inner)

  # Split into individual arguments
  parts <- trimws(strsplit(inner, ",")[[1]])

  # Process each argument
  out <- vapply(parts, function(p) {
    # Split on '=', but keep value part if present
    if (grepl("=", p, fixed = TRUE)) {
      lhs <- trimws(sub("=.*", "", p))
      rhs <- trimws(sub(".*=", "", p))

      if (rhs == "") {
        # No value: return only the name
        lhs
      } else {
        # Value present: keep "name = value"
        paste0(lhs, " = ", rhs)
      }

    } else {
      # No "=" at all: just return as is
      p
    }
  }, character(1))

  paste(out, collapse = ", ")
}

ReplaceChr <- function(text) {
  text <- gsub("&", "&amp;", text)
  text <- gsub("<", "&lt;", text)
  text <- gsub(">", "&gt;", text)
  text <- gsub("\"", "&quot;", text)
  text <- gsub("'", "&apos;", text)
  text
}

deparseTreeExp <- function(expr) {
  if(!is.expression(expr))
    expr <- deparse(expr, width.cutoff = 400L, backtick = TRUE)
  ReplaceChr(paste(expr, collapse = "\n"))
}

CurlyBracket <- function(x) {
  if(length(x) > 1L) return(FALSE)
  grepl("^`?\\{`?$", x)
}

uid <- function() {
  paste(sample(c(0:9, letters[1:6]), size = 17, replace = TRUE), collapse = "")
}

crdate <- function() {
  as.character(floor(1000 * as.numeric(Sys.time())))
}
