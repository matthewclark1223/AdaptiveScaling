
modd<-rstan::extract(mod)
modd<-as.data.frame(modd)

modd<-modd%>%dplyr::select(params.1,params.2,params.3)
modd<-modd%>%pivot_longer(cols=1:3,names_to="parameter",values_to = "estimate")

modd<-modd%>%group_by(parameter)%>%summarise(med=median(estimate),lower=quantile(estimate,0.05),upper=quantile(estimate,0.95))
#true<-data.frame(parameter=modd$parameter,med=c(0.3,0.15,0.02))

p2<-ggplot(modd)+
  geom_pointrange(aes(ymin=lower,y=med,ymax=upper,x=parameter),size=1)+
  coord_flip()+geom_hline(yintercept=0,color="grey",linetype=2)+
  #geom_point(data=true,aes(x=parameter,y=med),color="blue",alpha=0.99,size=6,shape=2)+
  scale_x_discrete(labels=c("Social    \ntransmission (\U03B2)","Dropout (\U03B3)","Independent\nadoption (\u03b1)"), )+
  theme_classic()+  xlab("Parameter")+ylab("Estimate")+
  theme(axis.title = element_text(colour = "black",size=16),
        axis.text=element_text(color="black",size=14))


#cowplot::plot_grid(p2, p1, labels = c('A: Inference', 'B: Prediction'), label_size = 12)

p2
