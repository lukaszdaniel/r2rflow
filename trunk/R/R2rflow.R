#' @import codetools
NULL

#' Convert R code to rflow file
#' 
#' Takes a file or text argument and prints out rflow xml
#' @author Lukasz Daniel
#' @param file A file containing R code.
#' @param text A text containing R code.
#' @param output An output rflow file. If a 'file' argument is specified, 'output' argument is
#'               assumed to have the same file name as in 'file' but with rflow extension.
#' @return xml formated R code
#' @export
R2rflow <- function (text = NULL, file = NULL, output = NULL) {
  
  if (missing(text) && (missing(file) || nchar(file) == 0)) {
    stop("No R code to convert")
  }
  if(!missing(text) && !missing(file)) {
    stop("Cannot specify both 'text' and 'file' arguments")
  }
  if(!missing(text) && missing(output)) {
    stop("'output' argument must be specified")
  }
  if(!missing(file) && missing(output)) {
    output <- sub("\\.[RrSq]$", "\\.rflow", file)
  }
  
  codes <- parse(text = text, file = file)
  env <- new.env()
  
  functionSet <- function (v, e) assign(v, e, envir = env)
  functionGet <- function (v, e) get(v, envir = env)
  
  w <- makeCodeWalker(call = functionCall, leaf = functionLeaf, set = functionSet, get = functionGet)
  
  
  edge <- NULL
  w$set("ypos", length(edge))
  w$set("nodeID", 0)
  w$set("output", output)
  
  Preambule(output)
  cat('<rflow>\n', append = TRUE, file = output)
  cat(' <graph version="0.3" width="894" height="742" locationtype="a" offsetx="0" offsety="0">\n', append = TRUE, file = output)
  Settings(output)
  
  for(i in seq_len(length(codes))) {
    
    edge <- c(edge, get("nodeID", envir = env)+1)
    w$set("ypos", length(edge))
    walkCode(codes[[i]], w)
  }
  
  PrintEdges(edge, file = output)
  cat(' </graph>\n', append = TRUE, file = output)
  Task(output)
  cat('</rflow>\n', append = TRUE, file = output)
}

deparseTreeExp <- function (e) {
  s <- NULL
  d <- deparse(e, width.cutoff = 400, backtick = TRUE)
  for (x in d) {
    s <- paste(s, x, "\n", sep = "")
  }
  s <- substring(s, 1, nchar(s, type = "chars") - 1)
  s <- ReplaceChr(s)
  return(s)
}

functionCall <- function (e, w) {

  w$set("nodeID", w$get("nodeID") + 1)

  if (e[[1]] == "<-" && (length(e[[3]]) == 4) && e[[3]][[1]] == "function") {
    FunctionNodeModel(e,w)
  } else if (e[[1]] == "if") {
    IfNodeModel(e,w)
  } else if (e[[1]] == "for") {
    ForNodeModel(e,w)
  } else if (e[[1]] == "while") {
    WhileNodeModel(e,w)
  } else if (e[[1]] == "repeat") {
    RepeatNodeModel(e,w)
  } else {
    FreeNodeModel(e,w)
  }
}

functionLeaf <- function (e, w) {
  w$set("nodeID", w$get("nodeID") + 1)
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
  cat(sprintf('  <entry key="args">(%s)</entry>\n', gsub("= (,|$)", "\\1", gsub("^list\\(|\\)$|^NULL$", "", deparseTreeExp(e[[3]][[2]])))), append = TRUE, file = output)
  cat("  <subflow>\n", append = TRUE, file = output)
  cat(sprintf('   <graph version="0.3" width="%d" height="%d" locationtype="a" offsetx="0" offsety="0">\n', x, y), append = TRUE, file = output)
  
  
  w$set("nodeID", w$get("nodeID") + 1)
  In <- w$get("nodeID")
  edge <- In
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "in", w = w)
  
  if ((length(e[[3]][[3]]) > 1) && e[[3]][[3]][[1]] == "{") {
    for (ee in as.list(e[[3]][[3]])) if (!missing(ee) && deparseTreeExp(ee) != "`{`")  {
      edge <- c(edge, w$get("nodeID") + 1)
      w$set("ypos", length(edge))
      walkCode(ee, w)
    }
  } else {
    edge <- c(edge, w$get("nodeID") + 1)

    w$set("ypos", length(edge))
    walkCode(e[[3]][[3]], w)
  }
  w$set("nodeID", w$get("nodeID") + 1)
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
  
  w$set("nodeID", w$get("nodeID") + 1)
  In <- w$get("nodeID")
  edge <- In
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "in", w = w)
  
  if ((length(e[[3]]) > 1L) && e[[3]][[1]] == "{") {
    for (ee in as.list(e[[3]])) if (!missing(ee) && deparseTreeExp(ee) != "`{`") {
      edge <- c(edge, w$get("nodeID") + 1)
      w$set("ypos", length(edge))
      walkCode(ee, w)
    }
  } else {
    edge <- c(edge, w$get("nodeID") + 1)
    w$set("ypos", length(edge))
    walkCode(e[[3]], w)
  }
  
  w$set("nodeID", w$get("nodeID") + 1)
  Out <- w$get("nodeID")
  edge <- c(edge, Out)
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "out", w = w)
  
  PrintEdges(edge, file = output)
  cat(' </graph>\n', append = TRUE, file = output)
  cat('</subflow>\n', append = TRUE, file = output)
  ######################
  
  cat('<subflow>\n', append = TRUE, file = output)
  cat(sprintf(' <graph version="0.3" width="%d" height="%d" locationtype="a" offsetx="0" offsety="0">\n', x, y), append = TRUE, file = output)
  
  w$set("nodeID", w$get("nodeID") + 1)
  In <- w$get("nodeID")
  edge <- In
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "in", w = w)
  
  if (length(e) == 4) {
    if ((length(e[[4]]) > 1L) && e[[4]][[1]] == "{") {
      for (ee in as.list(e[[4]])) if (!missing(ee) && deparseTreeExp(ee) != "`{`")  {
        edge <- c(edge, w$get("nodeID") + 1)
        w$set("ypos", length(edge))
        walkCode(ee, w)
      }
    } else {
      edge <- c(edge, w$get("nodeID") + 1)
      w$set("ypos", length(edge))
      walkCode(e[[4]], w)
    }
  }
  
  w$set("nodeID", w$get("nodeID") + 1)
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
  
  
  w$set("nodeID", w$get("nodeID") + 1)
  In <- w$get("nodeID")
  edge <- In
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "in", w = w)
  
  if ((length(e[[4]]) > 1L) && e[[4]][[1]] == "{") {
    for (ee in as.list(e[[4]])) if (!missing(ee) && deparseTreeExp(ee) != "`{`") {
      edge <- c(edge, w$get("nodeID") + 1)
      w$set("ypos", length(edge))
      walkCode(ee, w)
    }
  } else {
    edge <- c(edge, w$get("nodeID") + 1)
    w$set("ypos", length(edge))
    walkCode(e[[4]], w)
  }
  
  w$set("nodeID", w$get("nodeID") + 1)
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
  
  
  w$set("nodeID", w$get("nodeID") + 1)
  In <- w$get("nodeID")
  edge <- In
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "in", w = w)
  
  if ((length(e[[3]]) > 1L) && e[[3]][[1]] == "{") {
    for (ee in as.list(e[[3]])) if (!missing(ee) && deparseTreeExp(ee) != "`{`") {
      edge <- c(edge, w$get("nodeID") + 1)
      w$set("ypos", length(edge))
      walkCode(ee, w)
    }
  } else {
    edge <- c(edge, w$get("nodeID") + 1)
    w$set("ypos", length(edge))
    walkCode(e[[3]], w)
  }
  
  w$set("nodeID", w$get("nodeID") + 1)
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
  
  w$set("nodeID", w$get("nodeID") + 1)
  In <- w$get("nodeID")
  edge <- In
  w$set("ypos", length(edge))
  TunnelNodeModel(type = "in", w = w)
  
  if ((length(e[[2]]) > 1L) && e[[2]][[1]] == "{") {
    for (ee in as.list(e[[2]])) if (!missing(ee) && deparseTreeExp(ee) != "`{`") {
      edge <- c(edge, w$get("nodeID") + 1)
      w$set("ypos", length(edge))
      walkCode(ee, w)
    }
  } else {
    edge <- c(edge, w$get("nodeID") + 1)
    w$set("ypos", length(edge))
    walkCode(e[[2]], w)
  }
  
  w$set("nodeID", w$get("nodeID") + 1)
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
  if(length(x) > 1L)  for(i in seq_len(length(x)-1L)) {
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
