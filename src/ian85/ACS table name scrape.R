library(stringr)
library(methods)
library(plyr)
library(XML)


#arguments(endyear, span = 5, dataset = "acs", keyword, table.name, table.number, case.sensitive = T) 


endyear = 2015
span = 5
dataset = "acs"
table.number = "b01001"
table.name = table.number
case.sensitive = T
keyword = "sex"

library("XML")
url <- "http://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/"
doc <- htmlParse(url)
library(httr)
url <- "http://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/"
doc <- htmlParse(rawToChar(GET(url)$content))


doc.url = paste("http://api.census.gov/data/", endyear, "/acs", span, "/variables.xml", sep = "")

  # Try getting the xml locally
    doc.local = "./git/comm_fairfax/data/comm_fairfax/original/variables.xml"
  if (url.exists(doc.url)) {
    doc = xmlInternalTreeParse(doc.local)
  }

    
  tmp = doc
  getNodeSet(doc, "//d:var[@xml:id]", c(d = "http://thedataweb.rm.census.gov/api/discovery/"))
  tables = xpathSApply(doc, "//d:var[@xml:id]", fun = xmlGetAttr, namespaces = c(d = "http://thedataweb.rm.census.gov/api/discovery/"), name = "xml:id")
  
# Check to see if we can short-circuit acs.fetch
  
  
  
  
  endyear = 2015
  span = 5
  dataset = "acs"
  table.number = "b01001"
  geography = geo.make(state = "CA", county = "*", tract = "*")
  keyword = "sex"
  col.names = "auto"
  
  # function (endyear, span = 5, geography, table.name, table.number, 
  #           variable, keyword, dataset = "acs", key, col.names = "auto", 
  #           ...) 
  # {
    var.max = 40

      arglist = as.list(environment())
      missing.args = unlist(lapply(arglist, is.symbol))
      arglist = arglist[!missing.args]
      arglist = arglist[names(arglist) != "variable"]
      arglist = arglist[names(arglist) != "geography"]
      arglist = arglist[names(arglist) != "key"]
      arglist = arglist[names(arglist) != "col.names"]
      arglist = arglist[names(arglist) != "var.max"]
      variable = do.call(acs.lookup, arglist)

      
    if (is.acs.lookup(variable)) {
      variables.xml = results(variable)
      variables = variables.xml$variable.code
      if (dataset == "acs") {
        variables = paste(rep(variables, each = 2), c("E", 
                                                      "M"), sep = "")
      }
    }
    if (!is.acs.lookup(variable)) {
      if (variable[1] == "recur") {
        variables = variable[2:length(variable)]
      }
      else {
        if (dataset == "acs") {
          variables = paste(rep(variable, each = 2), c("E", 
                                                       "M"), sep = "")
        }
        if (dataset == "sf1" | dataset == "sf3") {
          variables = variable
        }
      }
      if (variable[1] == "") {
        variables.xml = acs.lookup(keyword = keyword, table.name = table.name, 
                                   endyear = endyear, dataset = dataset, ...)
        if (!identical(NA, variables.xml)) {
          variables.xml = results(variables.xml)
          variables = variables.xml$variable.code
        }
        else {
          warning("No results found;\n  perhaps try acs.lookup()...?")
          return(NA)
        }
      }
    }
    if (length(variables) == 1 && is.na(variable)) 
      return(NA)
    if (identical(col.names, "pretty")) {
      if (!is.acs.lookup(variable)) {
        warning("\"pretty\" col.names not available when variable codes provided.\n  Using standard variable code names for columns.")
        col.names = "auto"
      }
      else {
        col.names = paste(variables.xml$table.name, variables.xml$variable.name, 
                          sep = ": ")
      }
    }
    if (identical(col.names, "auto") & dataset == "acs") 
      col.names = rep("auto", (length(variables)/2))
    if (identical(col.names, "auto") & (dataset == "sf1" | dataset == 
                                        "sf3")) 
      col.names = rep("auto", length(variables))
    if (length(variables) > var.max & dataset == "acs") {
      acs.obj = cbind(acs.fetch(endyear = endyear, span = span, 
                                geography = geography, variable = c("recur", variables[1:(var.max - 
                                                                                            2)]), key = key, dataset = dataset, col.names = col.names[1:((var.max - 
                                                                                                                                                            2)/2)]), acs.fetch(endyear = endyear, span = span, 
                                                                                                                                                                               geography = geography, variable = c("recur", variables[(var.max - 
                                                                                                                                                                                                                                         1):length(variables)]), col.names = col.names[(1 + 
                                                                                                                                                                                                                                                                                          ((var.max - 2)/2)):length(col.names)], key = key, 
                                                                                                                                                                               dataset = dataset))
      return(acs.obj)
    }
    if (length(variables) > var.max & (dataset == "sf1" | dataset == 
                                       "sf3")) {
      acs.obj = cbind(acs.fetch(endyear = endyear, span = span, 
                                geography = geography, variable = c("recur", variables[1:(var.max - 
                                                                                            2)]), key = key, dataset = dataset, col.names = col.names[1:(var.max - 
                                                                                                                                                           2)]), acs.fetch(endyear = endyear, span = span, 
                                                                                                                                                                           geography = geography, variable = c("recur", variables[(var.max - 
                                                                                                                                                                                                                                     1):length(variables)]), col.names = col.names[(var.max - 
                                                                                                                                                                                                                                                                                      1):length(col.names)], key = key, dataset = dataset))
      return(acs.obj)
    }
    if (is.geo.set(geography) && length(geography) > 1) {
      acs.obj = rbind(acs.fetch(endyear = endyear, span = span, 
                                geography = geography[1], variable = c("recur", variables), 
                                key = key, dataset = dataset, col.names = col.names), 
                      acs.fetch(endyear = endyear, span = span, geography = geography[2:length(geography)], 
                                variable = c("recur", variables), dataset = dataset, 
                                key = key, col.names = col.names))
      if (combine(geography)) {
        acs.obj = apply(acs.obj, FUN = sum, MARGIN = 1, agg.term = combine.term(geography), 
                        ...)
        acs.obj@acs.units = .acs.identify.units(acs.colnames(acs.obj))
      }
      return(acs.obj)
    }
    if (is.geo.set(geography) && length(geography) == 1) {
      acs.obj = acs.fetch(endyear = endyear, span = span, geography = geography[[1]], 
                          variable = c("recur", variables), key = key, dataset = dataset, 
                          col.names = col.names)
      if (combine(geography)) {
        acs.obj = apply(acs.obj, FUN = sum, MARGIN = 1, agg.term = combine.term(geography), 
                        ...)
        acs.obj@acs.units = .acs.identify.units(acs.colnames(acs.obj))
      }
      return(acs.obj)
    }
    api.url = api.url.maker(endyear = endyear, span = span, key = key, 
                            variables = variables, dataset = dataset, geo.call = geography)
    geo.length = length(api.in(geography)) + 2
    url.test = url.exists(api.url, .header = T)
    if (url.test["statusMessage"] != "OK") {
      warning(call. = F, paste("No data found at:\n  ", api.url, 
                               sep = ""))
    }
    in.data = suppressWarnings(read.csv(api.url, na.strings = c("-", 
                                                                "**", "***", "(X)", "N", "null"), stringsAsFactors = F))
    in.data = in.data[, -length(in.data)]
    geocols = (length(in.data) - geo.length + 1):length(in.data)
    if (identical(col.names[1], "auto")) {
      if (dataset == "acs") {
        col.names = names(in.data)[seq(1, (length(in.data) - 
                                             geo.length), 2)]
      }
      else if (dataset == "sf1" | dataset == "sf3") {
        col.names = names(in.data)[1:(length(in.data) - geo.length)]
      }
      col.names[1] = gsub("X..", "", col.names[1])
      col.names = gsub(pattern = "E$", x = col.names, replacement = "")
    }
    datacols = 1:(length(in.data) - geo.length)
    in.data[in.data == "*****"] = 0
    in.data[[1]] = gsub("[", "", in.data[[1]], fixed = T)
    in.data[[length(in.data)]] = gsub("]", "", in.data[[length(in.data)]], 
                                      fixed = T)
    for (i in 1:length(datacols)) {
      in.data[[i]] = gsub(",", "", in.data[[i]])
      in.data[[i]] = as.numeric(in.data[[i]])
    }
    GEOGRAPHY = as.data.frame(in.data[, geocols])
    names(GEOGRAPHY) = gsub(".", "", names(GEOGRAPHY), fixed = T)
    if (dataset == "acs") {
      acs.obj = new(Class = "acs", endyear = endyear, span = span, 
                    geography = GEOGRAPHY, acs.colnames = col.names, 
                    acs.units = .acs.identify.units(col.names), currency.year = endyear, 
                    standard.error = as.matrix(in.data[, seq(2, (length(in.data) - 
                                                                   geo.length), 2)]), modified = F, estimate = as.matrix(in.data[, 
                                                                                                                                 seq(1, (length(in.data) - geo.length), 2)]))
    }
    if (dataset == "sf1" | dataset == "sf3") {
      acs.obj = new(Class = "acs", endyear = endyear, span = span, 
                    geography = GEOGRAPHY, acs.colnames = col.names, 
                    acs.units = .acs.identify.units(col.names), currency.year = endyear, 
                    standard.error = as.matrix(0 * (in.data[1:(length(in.data) - 
                                                                 geo.length)])), modified = F, estimate = as.matrix(in.data[1:(length(in.data) - 
                                                                                                                                 geo.length)]))
    }
    if (endyear(acs.obj) <= 2005) 
      acs.obj@standard.error = acs.obj@standard.error/1.65
    else acs.obj@standard.error = acs.obj@standard.error/1.645
    acs.obj = .acs.dimnames(acs.obj)
    acs.obj
  }