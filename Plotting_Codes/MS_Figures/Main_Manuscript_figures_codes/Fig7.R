
#initial clean up
rm(list = ls())
graphics.off()
cat("\f")

setwd("/home/shikhani/Documents/Sevan_plotting_draft/")

library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(grid)
library(RColorBrewer)

# function for variance partitioning
frac_fun <- function(data, smoothe = TRUE){
  
  out_part <- sapply(unique(data$date), function(y){
    tst <- subset(data, date == y)
    tst <- na.omit(tst)
    an <- anova(lm(value ~ model * variable, data = tst))
    totsst <- sum(an$`Sum Sq`)
    sep <- an$`Sum Sq`
    sep <- sep/totsst
    return(sep)
  })
  
  out_indfract <- data.frame(year = unique(data$date),
                             LakeModel = out_part[1, ],
                             Climate= out_part[2, ],
                             Interactions = apply(out_part[3:4, ], 2, sum))
  if(smoothe) {
    out_indfract$LakeModel <- loess(LakeModel~ year,
                                    data = out_indfract, span = 0.2)$fitted
    out_indfract$Climate<- loess(Climate ~ year,
                                 data = out_indfract, span = 0.2)$fitted
    out_indfract$Interactions <- loess(Interactions ~ year,
                                       data = out_indfract, span = 0.2)$fitted
  }
  out_indfract <- tidyr::pivot_longer(out_indfract, 2:4)
  out_indfract$name <- factor(out_indfract$name,
                              levels = c("Interactions", "Climate",
                                         "LakeModel"))
  
  
  return(out_indfract)
}


# load in data
output_annual_biascorr <-  get(load("cordex_LER_annual_named.rda"))[1:2]


# reshape surf temp of corrected to long format
surf_temp <- melt(output_annual_biascorr$Surface_Temperature, id.vars = "date")
colnames(surf_temp) <- c("date", "variable", "value", "scen", "model" )
surf_temp$scen <- as.factor(surf_temp$scen)
surf_temp$model <- as.factor(surf_temp$model)

bott_temp <- melt(output_annual_biascorr$Bottom_Temperature, id.vars = "date")
colnames(bott_temp) <- c("date", "variable", "value", "scen", "model" )
bott_temp$scen <- as.factor(bott_temp$scen)
bott_temp$model <- as.factor(bott_temp$model)




frac_surf <- setNames(lapply(c("historical", "rcp26", "rcp85", "rcp45"), function(s){
  surf_temp %>% filter(scen == s) %>% frac_fun()
}), c("historical", "rcp26", "rcp85", "rcp45"))


frac_bott <- setNames(lapply(c("historical", "rcp26", "rcp85", "rcp45"), function(s){
  bott_temp %>% filter(scen == s) %>% frac_fun()
}), c("historical", "rcp26", "rcp85", "rcp45"))



## plot


thm <- theme_pubr(base_size = 20) + grids() +
  theme(plot.title = element_text(size = 17, hjust = 0.5),
        legend.text = element_text(size = 15))  # Adjust the legend text size


get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#arranging the legend and plots in a grid:
p2 <- ggplot(frac_surf$rcp45) + geom_area(aes(x = year, y = value, fill = name),
                                          stat="identity", col = 1, lwd = 0.1) + 
  thm + xlab("") + ylab("") + 
  scale_fill_manual(name = "", values = rev(brewer.pal(5, "Dark2"))) +
  ggtitle("Surface temperature RCP 4.5")
p2_legend <- get_legend(p2)



p11 <-  ggplot(frac_surf$historical) + geom_area(aes(x = year, y = value, fill = name),
                                                 stat="identity", col = 1, lwd = 0.1) + 
  thm + xlab("") + ylab("Fraction of variance") + 
  scale_fill_manual(name = "", values = rev(brewer.pal(5, "Dark2"))) +
  ggtitle("Surface temperature hist")+ theme(legend.position = "none")

p1 <- ggplot(frac_surf$rcp26) + geom_area(aes(x = year, y = value, fill = name),
                                          stat="identity", col = 1, lwd = 0.1) + 
  thm + xlab("") + ylab("") + 
  scale_fill_manual(name = "", values = rev(brewer.pal(5, "Dark2"))) +
  ggtitle("Surface temperature RCP 2.6")+ theme(legend.position = "none")

p2 <- ggplot(frac_surf$rcp45) + geom_area(aes(x = year, y = value, fill = name),
                                          stat="identity", col = 1, lwd = 0.1) + 
  thm + xlab("") + ylab("") + 
  scale_fill_manual(name = "", values = rev(brewer.pal(5, "Dark2"))) +
  ggtitle("Surface temperature RCP 4.5")+ theme(legend.position = "none")

p3 <- ggplot(frac_surf$rcp85) + geom_area(aes(x = year, y = value, fill = name),
                                          stat="identity", col = 1, lwd = 0.1) + 
  thm + xlab("") + ylab("") + 
  scale_fill_manual(name = "", values = rev(brewer.pal(5, "Dark2"))) +
  ggtitle("Surface temperature RCP 8.5")+ theme(legend.position = "none")



p22 <- ggplot(frac_bott$historical) + geom_area(aes(x = year, y = value, fill = name),
                                          stat="identity", col = 1, lwd = 0.1) + 
  thm + xlab("") + ylab("Fraction of variance") + 
  scale_fill_manual(name = "", values = rev(brewer.pal(5, "Dark2"))) +
  ggtitle("Bottom temperature hist")+ theme(legend.position = "none")


p4 <- ggplot(frac_bott$rcp26) + geom_area(aes(x = year, y = value, fill = name),
                                          stat="identity", col = 1, lwd = 0.1) + 
  thm + xlab("") + ylab("") + 
  scale_fill_manual(name = "", values = rev(brewer.pal(5, "Dark2"))) +
  ggtitle("Bottom temperature RCP 2.6")+ theme(legend.position = "none")

p5 <- ggplot(frac_bott$rcp45) + geom_area(aes(x = year, y = value, fill = name),
                                          stat="identity", col = 1, lwd = 0.1) + 
  thm + xlab("") + ylab("") + 
  scale_fill_manual(name = "", values = rev(brewer.pal(5, "Dark2"))) +
  ggtitle("Bottom temperature RCP 4.5")+ theme(legend.position = "none")

p6 <- ggplot(frac_bott$rcp85) + geom_area(aes(x = year, y = value, fill = name),
                                          stat="identity", col = 1, lwd = 0.1) + 
  thm + xlab("") + ylab("") + 
  scale_fill_manual(name = "", values = rev(brewer.pal(5, "Dark2"))) +
  ggtitle("Bottom temperature RCP 8.5")+ theme(legend.position = "none")



pp <- ggarrange(p11, p1, p2, p3,p22, p4, p5, p6, ncol = 4, nrow = 2, common.legend = F) 
pp


p_all <- grid.arrange( 
             p2_legend, pp,
             nrow=2,heights=c(1, 10))

ggsave('variance_fracs_all.png', p_all,  dpi = 300,width = 600,height =400, units = 'mm')

