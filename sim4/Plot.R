require(ggplot2)
ggplot.temp=ggplot(data=subest) +
  facet_wrap(n~k)+theme(legend.position = "none") +
  xlab("")+ylab("RRMSE")
plotsave=ggplot.temp +
  geom_text(aes(y=value,x=variable,label=variable,
                color=factor(variable)),alpha=1,size=3,angle=15,
            position = position_dodge(width=0.2))

#facet_wrap(n~k,scales = "free_y" )