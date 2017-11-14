library(tidyverse)
library(glue)
library(RMySQL)

args <- commandArgs(TRUE)
db <- dbConnect(MySQL(), user='scala', password='scala', dbname='scala', host='127.0.0.1')

project <- read_csv(paste(args[1], "project.csv", sep="/"))
funs <- read_csv(paste(args[1], "funs.clean.csv", sep="/"))
params <- read_csv(paste(args[1], "params.clean.csv", sep="/"))
links <- read_csv(paste(args[1], "params-funs.clean.csv", sep="/"))

dbWriteTable(db, name="projects", value=project, append=TRUE, row.names=FALSE)
project_id <- fetch(dbSendQuery(db, "SELECT LAST_INSERT_ID();"))

params$project <- rep(toString(project_id), times=length(params$fqn))
funs$project <- rep(toString(project_id), times=length(funs$symbol))

dbWriteTable(db, name="funs", value=funs, append=TRUE, row.names=FALSE)
dbWriteTable(db, name="params", value=params, append=TRUE, row.names=FALSE)

params_info <- fetch(dbSendQuery(db, glue_sql("SELECT id, fqn, project FROM params WHERE project={project_id};", .con = db)))
# I believe this has some errors, I don't know why but it seems to not retrieve all of the fields.
# To reproduce, run this on the terminal after the query has executed:
#   apply(links, 1, function(lnk) {subset(funs_info, sourcelink==lnk[2])})
# Some of the rows are empty, which shouldn't happen. I have checked, and the data is in the database.
funs_info <- fetch(dbSendQuery(db, glue_sql("SELECT id, sourcelink, project FROM funs WHERE project={project_id};", .con = db)))

get_link_ids = function(links, params_info, funs_info) {
  # Here goes the code that, given three lists of tuples:
  #   links: (fqn, sourcelink) params_info: (paramid, fqn, project) funs_info: (funid, sourcelink, project)
  # Must return a list of tuples res: (paramid, funid) 
  # such that
  #  (pid[i], fid[j]) is in res iff 
  #    there is a tuple (fqn[q], sourcelink[r]) in links such that
  #      there is a tuple (pid[i], fqn[q], project) in params_info and
  #      there is a tuple (fid[j], sourcelink[r]) in funs_info
}

link_ids <- get_link_ids(params_info, funs_info)
