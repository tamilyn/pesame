#diagrams

library(DiagrammeR)

DiagrammeR("graph LR;
           A-->B;
           B-->C;
           B-->D")

# available shapes
# https://www.graphviz.org/doc/info/shapes.html#html

# https://www.graphviz.org/doc/info/attrs.html#d:label
# https://www.graphviz.org/doc/info/attrs.html#a:fillcolor
# https://www.graphviz.org/doc/info/attrs.html#k:color
# box of color names
# https://www.graphviz.org/doc/info/colors.html

DiagrammeR::grViz("digraph {

graph [layout = dot, rankdir = LR]

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fillcolor = Linen]

data1 [label = 'Dataset 1', shape = folder, fillcolor = Beige]
data2 [label = 'Dataset 2', shape = folder, fillcolor = Beige]
importdata [label =  'Import \n Data', fillcolor = yellow]
selectfactors [label =  'Select \n Factors', fillcolor = yellow]
analyze [label =  'Analyze', fillcolor = yellow]

# edge definitions with the node IDs
{data1 data2}  -> importdata -> selectfactors -> analyze
}")

dia1 <- function() {
  DiagrammeR::grViz("digraph {

graph [layout = dot, rankdir = LR]

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fillcolor = Linen]

data1 [label = 'Dataset 1', shape = folder, fillcolor = Beige]
data2 [label = 'Dataset 2', shape = folder, fillcolor = Beige]
importdata [label =  'Import \n Data', fillcolor = yellow]
selectfactors [label =  'Select \n Factors', fillcolor = yellow]
analyze [label =  'Analyze', fillcolor = yellow]

# edge definitions with the node IDs
{data1 data2}  -> importdata -> selectfactors -> analyze
}")
  
}


dia2 <- function() {
  DiagrammeR::grViz("digraph {

graph [layout = dot, rankdir = LR, compound = true]

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fillcolor = Linen, font = 'Courier']

userdataformats [label = 'User data formats|csv', shape = record, fillcolor = pink]
readin [label = 'Read in', shape = folder, fillcolor = pink]
pesame [label = 'PESAME' fillcolor = pink]
phyloseq [label = 'phyloseq|otu_table()|sample_data()' shape=record fillcolor=pink]
create_factors [label = 'create factors|2 level factors' shape=record fillcolor=pink]
a_table [label = 'table' fillcolor=Beige]
a_plot [label= 'plot' fillcolor=Beige]

# edge definitions with the node IDs
userdataformats -> readin
phyloseq -> create_factors

subgraph cluster_inputs {
  penwidth = 3.0
  
  subgraph cluster_import_data {
    label = 'Import Data' color = 'blue' penwidth=1.0
    userdataformats; readin
  }
  
  subgraph cluster_process_data {
    label = 'Process Data' color = 'blue' penwidth = 1.0
    node[shape = record]; phyloseq; create_factors;
  }
  
}
subgraph cluster_3 {
  label = 'Outputs' color = 'blue'
  a_table; a_plot
  }

}
}")
  
}



dia3 <- function() {
  DiagrammeR::grViz("digraph {

graph [layout = dot, rankdir = LR, compound = true]

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fillcolor = Linen, font = 'Courier']

userdataformats [label = 'User data formats|csv|phyloseq', shape = record, fillcolor = pink]
readin [label = 'Read in', shape = rect, fillcolor = pink]
pesame [label = 'PESAME' fillcolor = lightblue]
phyloseq [label = 'phyloseq|otu_table()|sample_data()' shape=record fillcolor=gold]
create_factors [label = 'create factors|2 level factors' shape=record fillcolor=gainsboro]
a_table [label = 'table' fillcolor=Beige]
a_plot [label= 'plot' fillcolor=Beige]

# edge definitions with the node IDs
userdataformats -> readin -> phyloseq -> pesame
pesame -> a_table
pesame -> a_plot
pesame -> create_factors

}
}")
  
}


subgraph cluster_import_data {
  label = 'Import Data' color = 'blue' penwidth=1.0
  userdataformats; readin
}
subgraph cluster_process_data {
  label = 'Process Data' color = 'blue' penwidth = 1.0
  node[shape = record]; phyloseq; create_factors;
}

subgraph cluster_3 {
  label = 'Outputs' color = 'blue' rankdir = LR
  a_table; a_plot
}

#[1]: 'Hello world'
pesame -> a_plot [ltail = output_cluster_1 lhead=pesame]
readin -> pesame [ltail = cluster_import_data lhead=cluster_2 constraint=false]
readin -> pesame [ltail = cluster_import_data lhead=cluster_process_data constraint=false]
