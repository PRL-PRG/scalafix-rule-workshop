library(shiny)
library(magrittr)
library(tidyverse)
library(visNetwork)
library(igraph)
library(stringr)
library(janitor)

NODE_PARAM <- 1L
NODE_FUN <- 2L

#path <- "~/scala-projects/top-120/ensime-server"
path <- "."
project_name <- "ensime"

load_data <- function() {
  params <- read_csv(file.path(path, "params.csv")) %>% distinct(id, .keep_all=TRUE)
  funs <- read_csv(file.path(path, "funs.csv"))
  params_funs <- read_csv(file.path(path, "params-funs.csv"))
  
  # convert the ids to simple indexes
  edges <- data_frame(
    from=match(params_funs$from, params$id),
    to=match(params_funs$to, funs$id) + nrow(params),
    id=1:nrow(params_funs)
  )

  # extract fqn and name
  locations <- params$id %>% 
    str_replace(fixed("_root_."), "") %>%
    # it can be val / var / object in which canse there will be `.` at the end
    str_replace("\\.$", "") %>%
    # it can be a def in which case there will be `;.` at the end
    str_replace("\\;$", "") %>%
    # the `#` separates class members from their type
    # the `.` separates object memebers from their type
    # here we unify it
    str_replace("#", ".") %>%
    str_split("\\.") %>%
    lapply(function(x) c(x[length(x)], str_c(x[-length(x)], collapse=".")))
  
  m_param_nodes <- params %>% transmute(
    id=1:nrow(params),
    node_type=NODE_PARAM,
    
    name=sapply(locations, `[[`, 1),
    fqn=sapply(locations, `[[`, 2),
    type=type,
    fqt=str_replace(clazz, "_root_.(.*)#", "\\1"),
    kind=kind
  )

  v_param_nodes <- m_param_nodes %>% transmute(
    id=1:nrow(params),
    node_type=NODE_PARAM,
    
    group="parameters",
    shape="dot",
    title=str_c("<pre><code>implicit ", kind, " ", fqn, ".", name, ": ", fqt, "</code></pre>", sep=""),
    color="red"
  )
    
  locations <- str_match(funs$id, "(.*)/(.*):(\\d+):(\\d+)")
  m_fun_nodes <- funs %>% transmute(
    id=nrow(params)+(1:nrow(funs)),
    node_type=NODE_FUN,
    
    location=locations[, 1],
    dir=locations[, 2],
    file=locations[, 3],
    line=locations[, 4],
    col=locations[, 5],
    project=str_replace(location, "(.*)/src/.*", "\\1"),
    src=str_replace(location, ".*/src/([^/]+)/.*", "\\1"),
    code=str_replace_all(code, "\\\\n", "\n"),
    # TODO: process symbol name
    name=str_replace(symbol, fixed("_root_."), "")
  )
  
  v_fun_nodes <- m_fun_nodes %>% transmute(
    id=nrow(params)+(1:nrow(funs)),
    node_type=NODE_FUN,

    group="functions",
    shape="dot",
    title=str_c("<pre><code>", location, "\n", name, "</code></pre>"),
    color="lightblue"
  )
  
  m_nodes <- bind_rows(m_param_nodes, m_fun_nodes)
  v_nodes <- bind_rows(v_param_nodes, v_fun_nodes)
  
  list(model=list(nodes=m_nodes, edges=edges), graph=igraph::graph_from_data_frame(edges, vertices=v_nodes, directed=T))
}

ui <- fluidPage(
   titlePanel("Implicit Browser"),
   
   sidebarLayout(
      sidebarPanel(
        h4("Filter"),
        checkboxGroupInput(
          inputId="filter_kind",
          label=h5("by kind")
        ),
        checkboxGroupInput(
          inputId="filter_src",
          label=h5("by src")
        ),
        checkboxGroupInput(
          inputId="filter_project",
          label=h5("by project")
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
  model <- reactive(data()$model)
  # an igraph model of full graph
  g <- reactive(data()$graph)
  
  graph <- reactive({
    # filter params
    param_nodes <- 
      model()$nodes %>% 
      filter(node_type == NODE_PARAM & kind %in% input$filter_kind)
    
    # filter funs
    fun_nodes <- 
      model()$nodes %>% 
      filter(
        project %in% input$filter_project,
        src %in% input$filter_src
      )
    
    eids <- 
      model()$edges %>% 
      filter(from %in% param_nodes$id & to %in% fun_nodes$id) %>% 
      .$id
    
    subgraph.edges(g(), eids)
  })
  
  output$graph <- renderVisNetwork({
    visIgraph(graph(), idToLabel=FALSE, physics=FALSE, smooth=FALSE, randomSeed=1) %>%
      visOptions(nodesIdSelection=TRUE, highlightNearest=TRUE)
  })

  observe({
    kind <- model()$nodes %>% 
      filter(node_type == NODE_PARAM) %>% 
      select(kind) %>%
      distinct() %>%
      .$kind
    
    updateCheckboxGroupInput(
      session, 
      "filter_kind",
      choices=kind,
      selected=kind
    )

    df <- model()$nodes %>% 
      filter(node_type == NODE_FUN) %>% 
      select(project, src)
    
    projects <- df %>% select(project) %>% distinct() %>% .$project
    src <- df %>% select(src) %>% distinct() %>% .$src
    
    updateCheckboxGroupInput(
      session, 
      "filter_project",
      choices=projects,
      selected=projects
    )
    
    updateCheckboxGroupInput(
      session, 
      "filter_src",
      choices=src,
      selected=src
    )
  })
  
  selected_node <- reactive({
    s_id = as.integer(input$graph_selected)
    if (!is.na(s_id) && is.integer(s_id) && length(s_id) == 1) {
      s_id
    } else {
      NULL
    }
  })
    
  # TODO: include the links
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
        remove_empty_cols()
    }
  }, options=list("paging"=FALSE, "searching"=FALSE))
}

shinyApp(ui = ui, server = server)