library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
setwd("/home/shikhani/Documents/Sevan_plotting_draft/Final_plots_codes/Fig2_flowchart/")

grViz('
digraph boxes_and_circles {

  # a "graph" statement
  graph [overlap = true, fontsize = 30, rankdir = LR]

  # several "node" statements
    node [shape = circle, fixedsize = true, width = 3, fontname = Helvetica, style = filled, fillcolor = "lightblue",fontsize = 30]
  "CORDEX\\ndata" [group= "input"]
  "ERA5\\ndata" [group= "input"]

  node [shape = square, color = "blue", fillcolor = "white", fontcolor = "black", fontname = Helvetica, width = 3,fontsize = 30]
  "re-project\\nCORDEX\\ndata"
  "bilinear\\ninterpolation"
  "bias\\ncorrection"
  "generate\\n100\\ncombinations"
  "force\\nLakeEnsemblR"
  "500\\nlake\\ntemperature\\nprojections"
  "calibrate\\nlake\\nmodels"

  # several "edge" statements
  "CORDEX\\ndata" -> "re-project\\nCORDEX\\ndata" [label = "CDO",fontsize = 30,arrowsize=2]
  "re-project\\nCORDEX\\ndata" -> "bilinear\\ninterpolation" [label = "Climate4R",fontsize = 30,arrowsize=2]
  "bilinear\\ninterpolation" -> "bias\\ncorrection" [label = "Climate4R",fontsize = 30,arrowsize=2]
  "bias\\ncorrection" -> "generate\\n100\\ncombinations" [label = "Climate4R",fontsize = 30,arrowsize=2]
  "generate\\n100\\ncombinations" -> "force\\nLakeEnsemblR" [label = "LakeEnsemblR",fontsize = 30,arrowsize=2]
  "force\\nLakeEnsemblR" -> "500\\nlake\\ntemperature\\nprojections" [label = "LakeEnsemblR",fontsize = 30,arrowsize=2]
  "calibrate\\nlake\\nmodels" -> "force\\nLakeEnsemblR" [label = "LakeEnsemblR",fontsize = 30,arrowsize=2]
  "ERA5\\ndata" -> "bias\\ncorrection" [label = "Climate4R",fontsize = 30,arrowsize=2]
  "ERA5\\ndata" -> "calibrate\\nlake\\nmodels" [label = "LakeEnsemblR",fontsize = 30,arrowsize=2]
}
') %>%
  export_svg %>% charToRaw %>% rsvg_png("Fig2_t3.png")
