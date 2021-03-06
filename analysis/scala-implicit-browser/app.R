library(shiny)
library(magrittr)
library(tidyverse)
library(visNetwork)
library(igraph)
library(stringr)
library(janitor)
library(RMySQL)
library(dbplyr)

NODE_PARAM <- 1L
NODE_FUN <- 2L

load_data <- function() {
  con <- DBI::dbConnect(RMySQL::MySQL(), dbname="scala", username="scala", password="scala", host="mysql")
  projects <- tbl(con, "projects") %>% collect(n=Inf)
  funs <- tbl(con, "funs")
  params <- tbl(con, "params")
  params_funs <- tbl(con, "params_funs")

  list(
    projects=projects,
    funs=funs,
    params=params,
    params_funs=params_funs,
    con=con
  )
}

create_model <- function(proj, funs, params, params_funs) {
  if (nrow(params_funs) == 0) {
    return(NULL)
  }

  # convert the ids to simple indexes
  edges <- data_frame(
    from=match(params_funs$param, params$id),
    to=match(params_funs$fun, funs$id) + nrow(params),
    id=1:nrow(params_funs)
  )

  m_param_nodes <- params %>%
    mutate(
      id=1:nrow(params),
      node_type=NODE_PARAM
    ) %>%
    select(
      id, node_type, everything()
    )

  v_param_nodes <- m_param_nodes %>% transmute(
    id=1:nrow(params),
    node_type=NODE_PARAM,

    group="parameters",
    shape="dot",
    title=str_c("<pre><code>implicit ", kind, " ", fqn, ".", name, ": ", fqtn, "</code></pre>", sep=""),
    color="red"
  )

  funs_module <- str_replace(funs$path, "(.*)/src/.*", "\\1")
  funs_srcset <- str_replace(funs$path, ".*/src/([^/]+)/.*", "\\1")

  if (any(grepl(".scala$", funs_module))) {
    funs_module <- "all"
  }

  if (any(grepl(".scala$", funs_srcset))) {
    # TODO: try to match test files
    funs_srcset <- "all"
  }

  m_fun_nodes <- funs %>%
    mutate(
      id=nrow(params)+(1:nrow(funs)),
      node_type=NODE_FUN,

      # FIXME: this is only good for github
      url=str_c(str_c(proj$url, "blob", proj$last_commit, path, sep="/"), "#L", line),
      module=funs_module,
      srcset=funs_srcset
    ) %>% select(
      id, node_type, url, module, srcset, everything()
    )

  v_fun_nodes <- m_fun_nodes %>% transmute(
    id=nrow(params)+(1:nrow(funs)),
    node_type=NODE_FUN,

    group="functions",
    shape="dot",
    title=str_c('<pre><a target="_blank" href="', url ,'">', path, "</a><br/><br/><code>", symbol, "</code></pre>"),
    color="lightblue"
  )

  m_nodes <- bind_rows(m_param_nodes, m_fun_nodes)
  v_nodes <- bind_rows(v_param_nodes, v_fun_nodes)

  list(
    nodes=m_nodes,
    edges=edges,
    graph=igraph::graph_from_data_frame(edges, vertices=v_nodes, directed=T)
  )
}

ui <- fluidPage(
   titlePanel("Implicit Browser"),

   sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId="project",
          label=h4("Project"),
          choices=NULL
        ),
        h4("Filter"),
        checkboxGroupInput(
          inputId="filter_kind",
          label=h5("by kind")
        ),
        checkboxGroupInput(
          inputId="filter_srcset",
          label=h5("by source group")
        ),
        checkboxGroupInput(
          inputId="filter_module",
          label=h5("by module")
        ),
        checkboxGroupInput(
            inputId="filter_param_type",
          label=h5("by param type")
        ),
        hr()
      ),

      mainPanel(
        h3("Graph"),
        visNetworkOutput("graph", height = "600px"),
        h3("Selected Node"),
        dataTableOutput("node_details"),
        h3("Neighbors"),
        dataTableOutput("node_neighbors")
      )
   )
)

server <- function(input, output, session) {
  data <- reactive(load_data())

  observe({
    data <- data()
    df <- data$projects %>%
      mutate(name=str_c(name, version, sep=":")) %>%
      select(name, id) %>%
      arrange(name)

    projects <- df$id
    names(projects) <- df$name

    updateSelectInput(
      session,
      "project",
      choices=projects,
      selected=projects[1]
    )
  })

  model <- reactive({
    data <- data()
    p_id <- as.integer(input$project)

    if (is.na(p_id)) {
      NULL
    } else {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message="Creating model", value = 0)
      progress$inc(0, detail="loading data from DB")

      project <- data$project %>% filter(id == p_id)

      funs <-
        data$funs %>%
        filter(project == p_id) %>%
        collect(n=Inf)

      params <-
        data$params %>%
        filter(project == p_id) %>%
        collect(n=Inf) %>%
        distinct(id, .keep_all=TRUE)

      # there is a bug in DPLYR which generates wring query
      params_funs <-
        tbl(
          data$con,
          sql(str_c("select pf.* from params p inner join params_funs pf on p.id=pf.param where p.project=", p_id))
        ) %>%
        collect(n=Inf)

      progress$inc(1/2, detail="creating graph")
      create_model(project, funs, params, params_funs)
    }
  })

  observe({
    model <- model()
    kind <- if (is.null(model)) {
      character(0L)
    } else {
      model$nodes %>%
        filter(node_type == NODE_PARAM) %>%
        select(kind) %>%
        distinct() %>%
        .$kind
    }

    updateCheckboxGroupInput(
      session,
      "filter_kind",
      choices=kind,
      selected=kind
    )
  })

  observe({
    model <- model()
    modules <- if (is.null(model)) {
      character(0L)
    } else {
      model$nodes %>%
        filter(node_type == NODE_FUN) %>%
        select(module) %>%
        distinct() %>%
        .$module
    }

    updateCheckboxGroupInput(
      session,
      "filter_module",
      choices=modules,
      selected=modules
    )
  })

  observe({
    model <- model()
    srcset <- if (is.null(model)) {
      character(0L)
    } else {
      model$nodes %>%
        filter(node_type == NODE_FUN) %>%
        select(srcset) %>%
        distinct() %>%
        .$srcset
    }

    updateCheckboxGroupInput(
      session,
      "filter_srcset",
      choices=srcset,
      selected=srcset
    )
  })

  observe({
    model <- model()
    types <- if (is.null(model)) {
      character(0L)
    } else {
      model$nodes %>%
        filter(node_type == NODE_PARAM) %>%
        select(fqtn) %>%
        distinct() %>%
        .$fqtn
    }

    updateCheckboxGroupInput(
      session,
      "filter_param_type",
      choices=types,
      selected=types
    )
  })

  graph <- reactive({
    model <- model()
    if (is.null(model)) {
      NULL
    } else {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message="Creating graph view", value = 0)
      progress$inc(0, detail="filtering nodes")

      # filter params
      param_nodes <-
        model$nodes %>%
        filter(
          node_type == NODE_PARAM,
          kind %in% input$filter_kind,
          fqtn %in% input$filter_param_type
        )

      # filter funs
      fun_nodes <-
        model$nodes %>%
        filter(
          module %in% input$filter_module,
          srcset %in% input$filter_srcset
        )

      eids <-
        model$edges %>%
        filter(from %in% param_nodes$id & to %in% fun_nodes$id) %>%
        .$id

      progress$inc(1/2, detail="creating sub graph")
      subgraph.edges(model$graph, eids)
    }
  })

  output$graph <- renderVisNetwork({
    graph <- graph()
    if (is.null(graph)) {
      NULL
    } else {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message="Rendering graph", value = 0)

      progress$inc(0, detail=str_c("V: ", vcount(graph), " E: ", ecount(graph)))
      visIgraph(graph, idToLabel=FALSE, physics=FALSE, smooth=FALSE) %>%
      visOptions(nodesIdSelection=TRUE, highlightNearest=TRUE)
    }
  })

  selected_node <- reactive({
    s_id = as.integer(input$graph_selected)
    if (!is.na(s_id) && is.integer(s_id) && length(s_id) == 1) {
      s_id
    } else {
      NULL
    }
  })

  output$node_details <- renderDataTable({
    s_id <- selected_node()
    if (is.null(s_id)) {
      data_frame()
    } else {
      node <- model()$nodes %>% filter(id == s_id)
      node %>% gather(na.rm=TRUE)
    }
  }, options=list("paging"=FALSE, "searching"=FALSE))

  output$node_neighbors <- renderDataTable({
    s_id <- selected_node()
    if (is.null(s_id)) {
      data_frame()
    } else {
      # need to convert between graph and subgraph
      sub_s_id <- which(V(graph())$name == s_id)
      vs <- neighbors(graph(), sub_s_id, mode="all")
      model()$nodes %>%
        filter(id %in% vs$name) %>%
        mutate(path=str_c('<pre><a target="_blank" href="', url ,'">', path, "</a></pre>")) %>%
        select(-url) %>%
        remove_empty_cols()
    }
  }, options=list("paging"=FALSE, "searching"=FALSE), escape=FALSE)
}

shinyApp(ui = ui, server = server)
