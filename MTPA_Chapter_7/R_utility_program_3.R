# Split-Plotting Utilities with grid Graphics (R)

library(grid)  # grid graphics foundation of split-plotting utilities

# functions used with ggplot2 graphics to split the plotting region
# to set margins and to plot more than one ggplot object on one page/screen
vplayout <- function(x, y) 
viewport(layout.pos.row=x, layout.pos.col=y) 

# grid graphics utility plots one plot with margins
ggplot.print.with.margins <- function(ggplot.object.name,left.margin.pct=10,
  right.margin.pct=10,top.margin.pct=10,bottom.margin.pct=10)
{ # begin function for printing ggplot objects with margins
  # margins expressed as percentages of total... use integers
 grid.newpage() 
pushViewport(viewport(layout=grid.layout(100,100)))
print(ggplot.object.name, 
  vp=vplayout((0 + top.margin.pct):(100 - bottom.margin.pct),
  (0 + left.margin.pct):(100 - right.margin.pct))) 
} # end function for printing ggplot objects with margins

# grid graphics utility plots two ggplot plotting objects in one column
special.top.bottom.ggplot.print.with.margins <- 
  function(ggplot.object.name,ggplot.text.tagging.object.name,
  left.margin.pct=5,right.margin.pct=5,top.margin.pct=5,
  bottom.margin.pct=5,plot.pct=80,text.tagging.pct=10) { 
# begin function for printing ggplot objects with margins 
# and text tagging at bottom of plot
# margins expressed as percentages of total... use integers
  if((top.margin.pct + bottom.margin.pct + plot.pct + text.tagging.pct) != 100) 
    stop(paste("function special.top.bottom.ggplot.print.with.margins()",
    "execution terminated:\n   top.margin.pct + bottom.margin.pct + ",
    "plot.pct + text.tagging.pct not equal to 100 percent",sep=""))  
  grid.newpage() 
  pushViewport(viewport(layout=grid.layout(100,100)))
  print(ggplot.object.name, 
  vp=vplayout((0 + top.margin.pct):
    (100 - (bottom.margin.pct + text.tagging.pct)),
  (0 + left.margin.pct):(100 - right.margin.pct))) 

  print(ggplot.text.tagging.object.name, 
    vp=vplayout((0 + (top.margin.pct + plot.pct)):(100 - bottom.margin.pct),
    (0 + left.margin.pct):(100 - right.margin.pct))) 
} # end function for printing ggplot objects with margins and text tagging

# grid graphics utility plots three ggplot plotting objects in one column
three.part.ggplot.print.with.margins <- function(ggfirstplot.object.name,
ggsecondplot.object.name,
ggthirdplot.object.name,
left.margin.pct=5,right.margin.pct=5,
top.margin.pct=10,bottom.margin.pct=10,
first.plot.pct=25,second.plot.pct=25,
third.plot.pct=30) { 
# function for printing ggplot objects with margins and top and bottom plots
# margins expressed as percentages of total... use integers
if((top.margin.pct + bottom.margin.pct + first.plot.pct + 
  second.plot.pct  + third.plot.pct) != 100) 
    stop(paste("function special.top.bottom.ggplot.print.with.margins()",
         "execution terminated:\n   top.margin.pct + bottom.margin.pct",
         "+ first.plot.pct + second.plot.pct  + third.plot.pct not equal",
         "to 100 percent",sep=""))  
grid.newpage() 
pushViewport(viewport(layout=grid.layout(100,100)))

print(ggfirstplot.object.name, vp=vplayout((0 + top.margin.pct):
  (100 - (second.plot.pct  + third.plot.pct + bottom.margin.pct)),
  (0 + left.margin.pct):(100 - right.margin.pct))) 

print(ggsecondplot.object.name, 
  vp=vplayout((0 + top.margin.pct + first.plot.pct):
  (100 - (third.plot.pct + bottom.margin.pct)),
  (0 + left.margin.pct):(100 - right.margin.pct))) 

print(ggthirdplot.object.name, 
  vp=vplayout((0 + top.margin.pct + first.plot.pct + second.plot.pct):
  (100 - (bottom.margin.pct)),(0 + left.margin.pct):
  (100 - right.margin.pct))) 
} 

# grid graphics utility plots two ggplot plotting objects in one row
# primary plot graph at left... legend at right
special.left.right.ggplot.print.with.margins <- 
  function(ggplot.object.name, ggplot.text.legend.object.name,
  left.margin.pct=5, right.margin.pct=5, top.margin.pct=5,
  bottom.margin.pct=5, plot.pct=85, text.legend.pct=5) { 
# begin function for printing ggplot objects with margins 
# and text legend at bottom of plot
# margins expressed as percentages of total... use integers
  if((left.margin.pct + right.margin.pct + plot.pct + text.legend.pct) != 100) 
    stop(paste("function special.left.right.ggplot.print.with.margins()",
    "execution terminated:\n   left.margin.pct + right.margin.pct + ",
    "plot.pct + text.legend.pct not equal to 100 percent",sep=""))  
  grid.newpage() 
  pushViewport(viewport(layout=grid.layout(100,100)))
  print(ggplot.object.name, 
  vp=vplayout((0 + top.margin.pct):(100 - (bottom.margin.pct)),
  (0 + left.margin.pct + text.legend.pct):(100 - right.margin.pct))) 

  print(ggplot.text.legend.object.name, 
    vp=vplayout((0 + (top.margin.pct)):(100 - bottom.margin.pct),
    (0 + left.margin.pct + plot.pct):(100 - right.margin.pct))) 
} # end function for printing ggplot objects with margins and text legend
 
# save split-plotting utilities for future work
save(vplayout,
  ggplot.print.with.margins,
  special.top.bottom.ggplot.print.with.margins,
  three.part.ggplot.print.with.margins,
  special.left.right.ggplot.print.with.margins,
  file="mtpa_split_plotting_utilities.Rdata")

