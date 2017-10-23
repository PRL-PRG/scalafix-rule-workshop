create table projects(
  id INTEGER PRIMARY KEY,
  name text not null,
  version text not null,
  path text not null
);
create table params(
  id INTEGER PRIMARY KEY,
  project INTEGER not null,
  name text not null,
  fqn text not null,
  type text not null,
  fqtn text not null,
  kind INTEGER not null,
  FOREIGN KEY(project) REFERENCES projects(id)
);
create table funapps(
  id INTEGER PRIMARY KEY,
  project INTEGER not null,
  path text not null,
  line integer not null,
  col integer not null,
  name text not null,
  fqfn text not null,
  nargs integer not null,
  FOREIGN KEY(project) REFERENCES projects(id)
);
create table param_funs(
  param INTEGER not null,
  funapp INTEGER not null,
  FOREIGN KEY(param) REFERENCES params(id),
  FOREIGN KEY(funapp) REFERENCES funapps(id)
);