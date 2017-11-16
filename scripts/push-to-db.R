library(tidyverse)
library(glue)
library(RMySQL)
library(magrittr)

args <- commandArgs(TRUE)

base_dir <- args[1]
project <- read_csv(file.path(base_dir, "project.csv"))
funs <- read_csv(file.path(base_dir, "funs.clean.csv"))
params <- read_csv(file.path(base_dir, "params.clean.csv"))
links <- read_csv(file.path(base_dir, "params-funs.clean.csv"))

db <- dbConnect(MySQL(), user='scala', password='scala', dbname='scala', host='127.0.0.1')

dbWithTransaction(db, {
    # this is just one project, right?
    dbWriteTable(db, name="projects", value=project, append=TRUE, row.names=FALSE)
    project_id <- dbGetQuery(db, "SELECT LAST_INSERT_ID();")

    params %<>% mutate(project=project_id$`LAST_INSERT_ID()`)
    funs %<>% mutate(project=project_id$`LAST_INSERT_ID()`)

    dbWriteTable(db, name="funs", value=funs, append=TRUE, row.names=FALSE)
    dbWriteTable(db, name="params", value=params, append=TRUE, row.names=FALSE)


    # TODO: we should add unique indexes on fqn,project
    params_ids <- dbGetQuery(db, sprintf("SELECT id, fqn FROM params WHERE project=%s;", project_id))

    # TODO: we should add unique indexes on sourcelink,project
    funs_ids <- dbGetQuery(db, sprintf("SELECT id, sourcelink FROM funs WHERE project=%s;", project_id))

    links %<>%
        left_join(params_ids, by=c("from"="fqn")) %>%
        select(param=id, to) %>%
        left_join(funs_ids, by=c("to"="sourcelink")) %>%
        select(param, fun=id)

    dbWriteTable(db, name="params_funs", value=links, append=TRUE, row.names=FALSE)

})
