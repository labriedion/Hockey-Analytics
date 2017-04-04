#Graph the results from back-to-back calculations - Étienne Labrie-Dion

require(ggplot2)
require(reshape2)

#Change the original matrix into a data frame formatted for plotting
datadf2 <- melt(t(B2Btotalteam))                    
datadf2 <- arrange(datadf2, desc(Var2))
datadf2 <- within(datadf2, Var2 <- relevel(Var2, ref = 3))

#Set the graph
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="none", legend.title=element_blank(), 
                     panel.grid.major.x=element_blank())

boxplot = ggplot(datadf2, aes(x = Var2, y = value)) + 
          ggtitle("Goalie SV% in back-to-back situations") +
          theme(plot.title = element_text(face="bold",color="grey20", hjust = 0.5))
boxplot = boxplot + 
          geom_boxplot(outlier.colour = NULL, aes_string(colour="Var2",fill="Var2")) +
          stat_summary(geom = "crossbar", width=0.85, fatten=.9, color="white", fun.data = function(x){ 
            return(c(y=median(x), ymin=median(x), ymax=median(x))) 
            })

theme = theme_update(axis.title.x = element_blank(),
                     axis.text.x  = element_text(face= "bold", angle=0, vjust=0.5, size=12))
theme = theme_update(axis.text.y=element_blank(), axis.ticks.y = element_blank(), 
                     axis.line.y = element_blank(), axis.title.y=element_blank())
theme = theme_update(axis.line.y = element_blank(), axis.title.y=element_blank(), 
                     axis.text.y = element_text(colour="grey"), axis.ticks.y= element_line(colour="grey"))

boxplot

#ANOVA comparing the various conditions
datalm = lm(value ~ Var2, data = datadf2)
summary(datalm)
