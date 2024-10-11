library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
#setwd("/home/shikhani/Documents/Sevan_plotting_draft/Final_plots_codes/Fig2_flowchart/")

grViz('
digraph boxes_and_circles {

  # a "graph" statement
  graph [overlap = true, fontsize = 150, rankdir = LR, margin = 2]

  # several "node" statements
    node [shape = circle, fixedsize = true, width = 13, fontname = Helvetica, style = filled, fillcolor = "lightblue",fontsize = 150]
  "CORDEX\\ndata" [group= "input"]
  "ERA5\\ndata" [group= "input"]

  node [shape = rectangle, color = "blue", fillcolor = "white", fontcolor = "black", fontname = Helvetica, width = 16, height = 12, fontsize = 150, penwidth = 6]
  "re-project\\nCORDEX\\ndata"
  "bilinear\\ninterpolation"
  "bias\\ncorrection"
  "generate\\n100\\ncombinations"
  "force\\nLakeEnsemblR"
  "500\\nlake\\ntemperature\\nprojections"[shape = octagon, fillcolor = "palevioletred2", fontcolor = "black", fontname = Helvetica, width = 15,fontsize = 150, height=15]
  "calibrate\\nlake\\nmodels"
  "anomaly\\nanalysis"[shape =ellipse , fillcolor = "palegreen4", fontcolor = "black", fontname = Helvetica, width = 14,fontsize = 150, height=11]
  "ice\\nand\\nstratification\\nindices"[shape =ellipse , fillcolor = "palegreen4", fontcolor = "black", fontname = Helvetica, width = 14,fontsize = 150, height=11]
  "variance\\ndecomposition"[shape = ellipse , fillcolor = "palegreen4", fontcolor = "black", fontname = Helvetica, width = 15,fontsize = 150, height=11]


  # several "edge" statements
  "CORDEX\\ndata" -> "re-project\\nCORDEX\\ndata" [label = " CDO ",fontsize = 150,arrowsize=7, width = 13, penwidth = 6]
  "re-project\\nCORDEX\\ndata" -> "bilinear\\ninterpolation" [label = " Climate4R ",fontsize = 150,arrowsize=7, width = 13, penwidth = 6]
  "bilinear\\ninterpolation" -> "bias\\ncorrection" [label = " Climate4R ",fontsize = 150,arrowsize=7, width = 13, penwidth = 6]
  "bias\\ncorrection" -> "generate\\n100\\ncombinations" [label = " Climate4R" ,fontsize = 150,arrowsize=7, width = 13, penwidth = 6]
  "generate\\n100\\ncombinations" -> "force\\nLakeEnsemblR" [label = " LakeEnsemblR ",fontsize = 150,arrowsize=7, width = 13, penwidth = 6]
  "force\\nLakeEnsemblR" -> "500\\nlake\\ntemperature\\nprojections" [label = " LakeEnsemblR ",fontsize = 150,arrowsize=7, width = 13, penwidth = 6]
  "calibrate\\nlake\\nmodels" -> "force\\nLakeEnsemblR" [label = " LakeEnsemblR ",fontsize = 150,arrowsize=7, width = 13, penwidth = 6]
  "ERA5\\ndata" -> "bias\\ncorrection" [label = " Climate4R ",fontsize = 150,arrowsize=7, width = 13, penwidth = 6]
  "ERA5\\ndata" -> "calibrate\\nlake\\nmodels" [label = " LakeEnsemblR ",fontsize = 150,arrowsize=7, width = 13, penwidth = 6]
  "500\\nlake\\ntemperature\\nprojections" -> "anomaly\\nanalysis" [label = " ", arrowsize=7, width = 13, penwidth = 6]
  "500\\nlake\\ntemperature\\nprojections" -> "ice\\nand\\nstratification\\nindices" [label = " ", arrowsize=7, width = 13, penwidth = 6]
  "500\\nlake\\ntemperature\\nprojections" -> "variance\\ndecomposition" [label = " ", arrowsize=7, width = 13, penwidth = 6]

}
') %>%
  export_svg() %>% charToRaw() %>% rsvg_pdf("../Main_Manuscript_figures/Fig2_t3_updatedbigger1.pdf")
