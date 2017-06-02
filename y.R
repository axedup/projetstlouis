library(TraMineR)
data(mvad)
seqstatl(mvad[, 17:86])


test= data.frame(
   
  x = c(6,20,40,60,80,100 ),
  y = c(33,25,27,36,23,25),
  n=c(284,re$n.risk[which.min((re$time-20)<0)-1],re$n.risk[which.min((re$time-40)<0)-1],
      re$n.risk[which.min((re$time-60)<0)-1],re$n.risk[which.min((re$time-80)<0)-1],
      re$n.risk[which.min((re$time-100)<0)-1]),
  ypos=c(0.05,0.05,0.05,0.05,0.05,0.05)
  
)


for (ii in 1:nrow(test))
{
  #display numbers at each visit
  km_os=km_os+ annotation_custom(grob = textGrob(test$n[ii]),  
                           xmin = test$x[ii], 
                           xmax = test$x[ii], 
                           ymin = test$ypos[ii], 
                           ymax = test$ypos[ii])
    
}


km_os=km_os+annotation_custom(grob = textGrob("N at risk"),  
                  xmin = 5, 
                  xmax = 5, 
                  ymin = 0.1, 
                  ymax = 0.1)



gt <- ggplot_gtable(ggplot_build(km_os))
gt$layout$clip[gt$layout$name=="panel"] <- "off"

grid.draw(gt)





# Base plot
df = data.frame(x=seq(1:10), y = seq(1:10))
p = ggplot(data = df, aes(x = x, y = y)) + geom_point() + ylim(0,10) +
  theme(plot.margin = unit(c(1,1,3,1), "cm"))
p

# Create the textGrobs
Text1 = textGrob(paste("Largest x-value is", round(max(df$x), 2), sep = " "))
Text2 = textGrob(paste("Mean = ", mean(df$x), sep = ""))

p1 = p + annotation_custom(grob = Text1,  xmin = 4, xmax = 4, ymin = -3, ymax = -3) +
  annotation_custom(grob = Text2,  xmin = 8, xmax = 8, ymin = -3, ymax = -3)
p1

# Code to override clipping
gt <- ggplotGrob(p1)
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)
