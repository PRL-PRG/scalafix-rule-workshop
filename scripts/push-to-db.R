library(tidyverse)
library(glue)
library(RMySQL)
library(magrittr)

args <- commandArgs(TRUE)

base_dir <- args[1]
project <- read_csv(file.path(base_dir, "project.csv"))
funs <- tryCatch(read_csv(file.path(base_dir, "funs.clean.csv")), error=function(e) -1)
params <- tryCatch(read_csv(file.path(base_dir, "params.clean.csv")), error=function(e) -1)
links <- tryCatch(read_csv(file.path(base_dir, "params-funs.clean.csv")), error=function(e) -1)
declared <- tryCatch(read_csv(file.path(base_dir, "declared-implicits.clean.csv")), error=function(e) -1)

db <- dbConnect(MySQL(), user='scala', password='scala', dbname='scala', host='127.0.0.1')

dbWithTransaction(db, {
    # this is just one project, right?
    dbWriteTable(db, name="projects", value=project, append=TRUE, row.names=FALSE)
    project_id <- dbGetQuery(db, "SELECT LAST_INSERT_ID();")

    if (funs != -1) {
      funs %<>% mutate(project=project_id$`LAST_INSERT_ID()`)
      dbWriteTable(db, name="funs", value=funs, append=TRUE, row.names=FALSE)
      # TODO: we should add unique indexes on sourcelink,project
      funs_ids <- dbGetQuery(db, sprintf("SELECT id, sourcelink FROM funs WHERE project=%s;", project_id))
    }
    
    if (params != -1) {
      params %<>% mutate(project=project_id$`LAST_INSERT_ID()`)
      dbWriteTable(db, name="params", value=params, append=TRUE, row.names=FALSE)
      # TODO: we should add unique indexes on fqn,project
      params_ids <- dbGetQuery(db, sprintf("SELECT id, fqn FROM params WHERE project=%s;", project_id))
    }
    
    if (declared != -1) {
      declared %<>% mutate(project=project_id$`LAST_INSERT_ID()`)
      dbWriteTable(db, name="declared_implicits", value=declared, append=TRUE, row.names=FALSE)
    }

    if (links != -1 && params != -1 && funs != -1) {
      links %<>%
          left_join(params_ids, by=c("from"="fqn")) %>%
          select(param=id, to) %>%
          left_join(funs_ids, by=c("to"="sourcelink")) %>%
          select(param, fun=id)
  
      dbWriteTable(db, name="params_funs", value=links, append=TRUE, row.names=FALSE)
    }
})
