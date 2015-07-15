#' @import codetools
NULL

#' @title Convert R code to rflow file
#' 
#' @description Takes a file or text argument and prints out rflow xml
#' @author Lukasz Daniel
#' @param file A file containing R code.
#' @param text A text containing R code.
#' @param max.level Non-negative integer or NA value. Maximum level to which diagram may be expanded. NA means no limit.
#' @param output An output rflow file. If a 'file' argument is specified, 'output' argument is
#'               assumed to have the same file name as in 'file' but with rflow extension.
#' @return xml formated R code
#' @export
R2rflow <- function (text = NULL, file = NULL, output = NULL, max.level = NA) {
  
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
  
  functionSet <- function (v, e) assign(v, e, envir = env)
  functionGet <- function (v, e) get(v, envir = env)
  
  w <- makeCodeWalker(call = functionCall, leaf = functionLeaf, set = functionSet, get = functionGet)
  
  
  edge <- NULL
  w$set("ypos", length(edge))
  w$set("nodeID", 0L)
  w$set("output", output)
  w$set("level", 0L)
  w$set("max.level", max.level)
  
  Preambule(output)
  cat('<rflow>\n', append = TRUE, file = output)
  cat(' <graph version="0.3" width="894" height="742" locationtype="a" offsetx="0" offsety="0">\n', append = TRUE, file = output)
  Settings(output)
  
  if(is.na(max.level) || (max.level > 0L)) {
    w$set("level", w$get("level") + 1L)
    for(i in seq_len(length(codes))) {
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
  
  PrintEdges(edge, file = output)
  cat(' </graph>\n', append = TRUE, file = output)
  Task(output)
  cat('</rflow>\n', append = TRUE, file = output)
}

deparseTreeExp <- function (e) {
  
  if(!is.expression(e))
    e <- deparse(e, width.cutoff = 400L, backtick = TRUE)
  
  return(ReplaceChr(paste(e, sep = "", collapse = "\n")))
}

functionCall <- function (e, w) {
  
  w$set("nodeID", w$get("nodeID") + 1L)
  if(is.na(w$get("max.level")) || (w$get("level") < w$get("max.level"))) {
    if ((e[[1]] == "<-") && (length(e[[3]]) == 4L) && (e[[3]][[1]] == "function")) {
      FunctionNodeModel(e,w)
    } else if (e[[1]] == "if") {
      IfNodeModel(e,w)
    } else if (e[[1]] == "for") {
      ForNodeModel(e,w)
    } else if (e[[1]] == "while") {
      WhileNodeModel(e,w)
    } else if (e[[1]] == "repeat") {
      RepeatNodeModel(e,w)
    } else if (CurlyBracket(e[[1]])) {
      SubflowNodeModel(e,w)
    } else {
      functionLeaf(e,w)
    }
  } else
    functionLeaf(e,w)
}

functionLeaf <- function (e, w) {
  #w$set("nodeID", w$get("nodeID") + 1L)
  FreeNodeModel(e,w)
  NULL
} 

FunctionNodeModel <- function (e, w) {
  
  xpos <- 440L
  ypos <- w$get("ypos") * 80L
  x <- 688L
  y <- 700L
  output <- w$get("output")
  
  cat(sprintf('<node id="%d" x="%d" y="%d">\n', w$get("nodeID"), xpos, ypos), append = TRUE, file = output)
  cat(sprintf(' <command>%s</command>\n', deparseTreeExp(e)), append = TRUE, file = output)
  cat(" <property/>\n", append = TRUE, file = output)
  cat(' <option type="com.ef_prime.rflow.node.base.FunctionNodeModel">\n', append = TRUE, file = output)
  cat(sprintf('  <entry key="function">%s</entry>\n', deparseTreeExp(e[[2]])), append = TRUE, file = output)
  cat(sprintf('  <entry key="args">(%s)</entry>\n', gsub("= (,|$)", "\\1", gsub("^(pair)?list\\(|\\)$|^NULL$", "", deparseTreeExp(e[[3]][[2]])))), append = TRUE, file = output)
  cat("  <subflow>\n", append = TRUE, file = output)
  cat(sprintf('   <graph version="0.3" width="%d" height="%d" locationtype="a" offsetx="0" offsety="0">\n', x, y), append = TRUE, file = output)
  
  
  w$set("nodeID", w$get("nodeID") + 1L)
  In <- w$get("nodeID")
  edge <- In
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "in", w = w)
  
  w$set("level", w$get("level") + 1L)
  if ((length(e[[3]][[3]]) > 1L) && CurlyBracket(e[[3]][[3]][[1]])) {
    for (ee in as.list(e[[3]][[3]])) if (!missing(ee) && !CurlyBracket(deparseTreeExp(ee)))  {
      edge <- c(edge, w$get("nodeID") + 1L)
      w$set("ypos", length(edge))
      walkCode(ee, w)
    }
  } else {
    edge <- c(edge, w$get("nodeID") + 1L)
    w$set("ypos", length(edge))
    walkCode(e[[3]][[3]], w)
  }
  w$set("level", w$get("level") - 1L)
  
  w$set("nodeID", w$get("nodeID") + 1L)
  Out <- w$get("nodeID")
  edge <- c(edge, Out)
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "out", w = w)
  
  PrintEdges(edge, file = output)
  
  cat("   </graph>\n", append = TRUE, file = output)
  cat("  </subflow>\n", append = TRUE, file = output)
  cat(" </option>\n", append = TRUE, file = output)
  cat("</node>\n", append = TRUE, file = output)
}

IfNodeModel <- function (e, w) {
  
  xpos <- 440L
  ypos <- w$get("ypos") * 80L
  x <- 688L
  y <- 700L
  output <- w$get("output")
  
  cat(sprintf('<node id="%d" x="%d" y="%d">\n', w$get("nodeID"), xpos, ypos), append = TRUE, file = output)
  cat(sprintf(' <command>%s</command>\n', deparseTreeExp(e)), append = TRUE, file = output)
  cat(' <property/>\n', append = TRUE, file = output)
  cat(' <option type="com.ef_prime.rflow.node.base.IfNodeModel">\n', append = TRUE, file = output)
  cat(sprintf('  <entry key="condition">%s</entry>\n', deparseTreeExp(e[[2]])), append = TRUE, file = output)
  
  cat('<subflow>\n', append = TRUE, file = output)
  cat(sprintf(' <graph version="0.3" width="%d" height="%d" locationtype="a" offsetx="0" offsety="0">\n', x, y), append = TRUE, file = output)
  
  w$set("nodeID", w$get("nodeID") + 1L)
  In <- w$get("nodeID")
  edge <- In
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "in", w = w)
  
  w$set("level", w$get("level") + 1L)
  if ((length(e[[3]]) > 1L) && CurlyBracket(e[[3]][[1]])) {
    for (ee in as.list(e[[3]])) if (!missing(ee) && !CurlyBracket(deparseTreeExp(ee))) {
      edge <- c(edge, w$get("nodeID") + 1L)
      w$set("ypos", length(edge))
      walkCode(ee, w)
    }
  } else {
    edge <- c(edge, w$get("nodeID") + 1L)
    w$set("ypos", length(edge))
    walkCode(e[[3]], w)
  }
  w$set("level", w$get("level") - 1L)
  
  w$set("nodeID", w$get("nodeID") + 1L)
  Out <- w$get("nodeID")
  edge <- c(edge, Out)
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "out", w = w)
  
  PrintEdges(edge, file = output)
  cat(' </graph>\n', append = TRUE, file = output)
  cat('</subflow>\n', append = TRUE, file = output)
  
  cat('<subflow>\n', append = TRUE, file = output)
  cat(sprintf(' <graph version="0.3" width="%d" height="%d" locationtype="a" offsetx="0" offsety="0">\n', x, y), append = TRUE, file = output)
  
  w$set("nodeID", w$get("nodeID") + 1L)
  In <- w$get("nodeID")
  edge <- In
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "in", w = w)
  
  w$set("level", w$get("level") + 1L)
  if (length(e) == 4L) {
    if ((length(e[[4]]) > 1L) && CurlyBracket(e[[4]][[1]])) {
      for (ee in as.list(e[[4]])) if (!missing(ee) && !CurlyBracket(deparseTreeExp(ee)))  {
        edge <- c(edge, w$get("nodeID") + 1L)
        w$set("ypos", length(edge))
        walkCode(ee, w)
      }
    } else {
      edge <- c(edge, w$get("nodeID") + 1L)
      w$set("ypos", length(edge))
      walkCode(e[[4]], w)
    }
  } 
  w$set("level", w$get("level") - 1L)
  
  w$set("nodeID", w$get("nodeID") + 1L)
  Out <- w$get("nodeID")
  edge <- c(edge, Out)
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "out", w = w)
  
  if(length(edge) > 2L) PrintEdges(edge, file = output)
  cat(' </graph>\n', append = TRUE, file = output)
  cat('</subflow>\n', append = TRUE, file = output)
  cat(' </option>\n', append = TRUE, file = output)
  cat('</node>\n', append = TRUE, file = output)
  
}

ForNodeModel <- function (e, w) {
  
  xpos <- 440L
  ypos <- w$get("ypos") * 80L
  x <- 688L
  y <- 700L
  output <- w$get("output")
  cat(sprintf('<node id="%d" x="%d" y="%d">\n', w$get("nodeID"), xpos, ypos), append = TRUE, file = output)
  cat(sprintf(' <command>%s</command>\n', deparseTreeExp(e)), append = TRUE, file = output)
  cat(" <property/>\n", append = TRUE, file = output)
  cat(' <option type="com.ef_prime.rflow.node.base.LoopNodeModel">\n', append = TRUE, file = output)
  cat('  <entry key="loop">for</entry>\n', append = TRUE, file = output)
  cat(sprintf('  <entry key="condition">%s</entry>\n', paste(deparseTreeExp(e[[2]]), " in ", deparseTreeExp(e[[3]]), sep = "", collase = "")), append = TRUE, file = output)
  cat("  <subflow>\n", append = TRUE, file = output)
  cat(sprintf('   <graph version="0.3" width="%d" height="%d" locationtype="a" offsetx="0" offsety="0">\n', x, y), append = TRUE, file = output)
  
  
  w$set("nodeID", w$get("nodeID") + 1L)
  In <- w$get("nodeID")
  edge <- In
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "in", w = w)
  
  w$set("level", w$get("level") + 1L)
  if ((length(e[[4]]) > 1L) && CurlyBracket(e[[4]][[1]])) {
    for (ee in as.list(e[[4]])) if (!missing(ee) && !CurlyBracket(deparseTreeExp(ee))) {
      edge <- c(edge, w$get("nodeID") + 1L)
      w$set("ypos", length(edge))
      walkCode(ee, w)
    }
  } else {
    edge <- c(edge, w$get("nodeID") + 1L)
    w$set("ypos", length(edge))
    walkCode(e[[4]], w)
  }
  w$set("level", w$get("level") - 1L)
  
  w$set("nodeID", w$get("nodeID") + 1L)
  Out <- w$get("nodeID")
  edge <- c(edge, Out)
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "out", w = w)
  
  PrintEdges(edge, file = output)
  cat("   </graph>\n", append = TRUE, file = output)
  cat("  </subflow>\n", append = TRUE, file = output)
  cat(" </option>\n", append = TRUE, file = output)
  cat("</node>\n", append = TRUE, file = output)
}

WhileNodeModel <- function (e, w) {
  
  xpos <- 440L
  ypos <- w$get("ypos") * 80L
  x <- 688L
  y <- 700L
  output <- w$get("output")
  cat(sprintf('<node id="%d" x="%d" y="%d">\n', w$get("nodeID"), xpos, ypos), append = TRUE, file = output)
  cat(sprintf(' <command>%s</command>\n', deparseTreeExp(e)), append = TRUE, file = output)
  cat(" <property/>\n", append = TRUE, file = output)
  cat(' <option type="com.ef_prime.rflow.node.base.LoopNodeModel">\n', append = TRUE, file = output)
  cat('  <entry key="loop">while</entry>\n', append = TRUE, file = output)
  cat(sprintf('  <entry key="condition">%s</entry>\n', deparseTreeExp(e[[2]])), append = TRUE, file = output)
  cat("  <subflow>\n", append = TRUE, file = output)
  cat(sprintf('   <graph version="0.3" width="%d" height="%d" locationtype="a" offsetx="0" offsety="0">\n', x, y), append = TRUE, file = output)
  
  
  w$set("nodeID", w$get("nodeID") + 1L)
  In <- w$get("nodeID")
  edge <- In
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "in", w = w)
  
  w$set("level", w$get("level") + 1L)
  if ((length(e[[3]]) > 1L) && CurlyBracket(e[[3]][[1]])) {
    for (ee in as.list(e[[3]])) if (!missing(ee) && !CurlyBracket(deparseTreeExp(ee))) {
      edge <- c(edge, w$get("nodeID") + 1L)
      w$set("ypos", length(edge))
      walkCode(ee, w)
    }
  } else {
    edge <- c(edge, w$get("nodeID") + 1L)
    w$set("ypos", length(edge))
    walkCode(e[[3]], w)
  }
  w$set("level", w$get("level") - 1L)
  
  w$set("nodeID", w$get("nodeID") + 1L)
  Out <- w$get("nodeID")
  edge <- c(edge, Out)
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "out", w = w)
  
  PrintEdges(edge, file = output)
  cat("   </graph>\n", append = TRUE, file = output)
  cat("  </subflow>\n", append = TRUE, file = output)
  cat(" </option>\n", append = TRUE, file = output)
  cat("</node>\n", append = TRUE, file = output)
}

RepeatNodeModel <- function (e, w) {
  
  xpos <- 440L
  ypos <- w$get("ypos") * 80L
  x <- 688L
  y <- 700L
  output <- w$get("output")
  cat(sprintf('<node id="%d" x="%d" y="%d">\n', w$get("nodeID"), xpos, ypos), append = TRUE, file = output)
  cat(sprintf(' <command>%s</command>\n', deparseTreeExp(e)), append = TRUE, file = output)
  cat(" <property/>\n", append = TRUE, file = output)
  cat(' <option type="com.ef_prime.rflow.node.base.LoopNodeModel">\n', append = TRUE, file = output)
  cat('  <entry key="loop">repeat</entry>\n', append = TRUE, file = output)
  cat("  <subflow>\n", append = TRUE, file = output)
  cat(sprintf('   <graph version="0.3" width="%d" height="%d" locationtype="a" offsetx="0" offsety="0">\n', x, y), append = TRUE, file = output)
  
  w$set("nodeID", w$get("nodeID") + 1L)
  In <- w$get("nodeID")
  edge <- In
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "in", w = w)
  
  w$set("level", w$get("level") + 1L)
  if ((length(e[[2]]) > 1L) && CurlyBracket(e[[2]][[1]])) {
    for (ee in as.list(e[[2]])) if (!missing(ee) && !CurlyBracket(deparseTreeExp(ee))) {
      edge <- c(edge, w$get("nodeID") + 1L)
      w$set("ypos", length(edge))
      walkCode(ee, w)
    }
  } else {
    edge <- c(edge, w$get("nodeID") + 1L)
    w$set("ypos", length(edge))
    walkCode(e[[2]], w)
  }
  w$set("level", w$get("level") - 1L)
  
  w$set("nodeID", w$get("nodeID") + 1L)
  Out <- w$get("nodeID")
  edge <- c(edge, Out)
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "out", w = w)
  
  PrintEdges(edge, file = output)
  cat("   </graph>\n", append = TRUE, file = output)
  cat("  </subflow>\n", append = TRUE, file = output)
  cat(" </option>\n", append = TRUE, file = output)
  cat("</node>\n", append = TRUE, file = output)
}

SubflowNodeModel <- function (e, w) {
  
  xpos <- 440L
  ypos <- w$get("ypos") * 80L
  x <- 688L
  y <- 700L
  output <- w$get("output")
  cat(sprintf('<node id="%d" x="%d" y="%d">\n', w$get("nodeID"), xpos, ypos), append = TRUE, file = output)
  cat(sprintf(' <command>%s</command>\n', deparseTreeExp(e)), append = TRUE, file = output)
  cat(" <property/>\n", append = TRUE, file = output)
  cat(' <option type="com.ef_prime.rflow.node.base.SubflowNodeModel">\n', append = TRUE, file = output)
  cat("  <subflow>\n", append = TRUE, file = output)
  cat(sprintf('   <graph version="0.3" width="%d" height="%d" locationtype="a" offsetx="100" offsety="50">\n', x, y), append = TRUE, file = output)
  
  w$set("nodeID", w$get("nodeID") + 1L)
  In <- w$get("nodeID")
  edge <- In
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "in", w = w)
  
  w$set("level", w$get("level") + 1L)
  for (ee in as.list(e)) if (!missing(ee) && !CurlyBracket(deparseTreeExp(ee))) {
    edge <- c(edge, w$get("nodeID") + 1L)
    w$set("ypos", length(edge))
    walkCode(ee, w)
  }
  w$set("level", w$get("level") - 1L)
  
  w$set("nodeID", w$get("nodeID") + 1L)
  Out <- w$get("nodeID")
  edge <- c(edge, Out)
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "out", w = w)
  
  PrintEdges(edge, file = output)
  cat("   </graph>\n", append = TRUE, file = output)
  cat("  </subflow>\n", append = TRUE, file = output)
  cat(" </option>\n", append = TRUE, file = output)
  cat("</node>\n", append = TRUE, file = output)
}

FreeNodeModel <- function (e, w) {
  
  xpos <- 440L
  ypos <- w$get("ypos") * 80L
  output <- w$get("output")
  
  cat(sprintf('<node id="%d" x="%d" y="%d">\n', w$get("nodeID"), xpos, ypos), append = TRUE, file = output)
  cat(sprintf(' <command>%s</command>\n', deparseTreeExp(e)), append = TRUE, file = output)
  cat(" <property/>\n", append = TRUE, file = output)
  cat(' <option type="com.ef_prime.rflow.node.base.FreeNodeModel"/>\n', append = TRUE, file = output)
  cat('</node>\n', append = TRUE, file = output)
}

PrintEdges <- function(x, file) {
  if(length(x) > 1L) for(i in seq_len(length(x)-1L)) {
    cat(sprintf('<edge from="%d" to="%d"/>\n', x[i], x[i + 1L]), append = TRUE, file = file)
  }
}

TunnelNodeModel <- function(type = "in", w) {
  
  xpos <- 440L
  ypos <- w$get("ypos") * 80L
  output <- w$get("output")
  
  cat(sprintf('<node id="%d" x="%d" y="%d">\n', w$get("nodeID"), xpos, ypos), append = TRUE, file = output)
  cat(' <command></command>\n', append = TRUE, file = output)
  cat(' <property/>\n', append = TRUE, file = output)
  cat(' <option type="com.ef_prime.rflow.node.base.TunnelNodeModel">\n', append = TRUE, file = output)
  cat(sprintf('  <entry key="io">%s</entry>\n', type), append = TRUE, file = output)
  cat(' </option>\n', append = TRUE, file = output)
  cat('</node>\n', append = TRUE, file = output)
}

Preambule <- function(output) {
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

ReplaceChr <- function(text) {
  text <- gsub("&", "&amp;", text)
  text <- gsub("<", "&lt;", text)
  text <- gsub(">", "&gt;", text)
  text <- gsub("\"", "&quot;", text)
  return(text)
}

CurlyBracket <- function(x) {
  if(length(x) > 1L) return(FALSE)
  grepl(pattern = "^`?\\{`?$", x = x)
}