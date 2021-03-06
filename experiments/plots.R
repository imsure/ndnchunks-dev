library(ggplot2)
library(gridExtra)
library(reshape2)
library(extrafont)

brewer.palette <- "Set1"
ggplot <- function(...) ggplot2::ggplot(...) + 
  scale_color_brewer(palette=paste(brewer.palette))

folder <- "./200mb_localhost"
setwd("~/ubuntu1404_node1/ndn-tools-dev/tools/chunks/experiments/200mb_localhost")
# cctype <- 'aimd'
# cctype <- 'bic'
# cctype <- 'cubic'

cctype.list <- c('aimd', 'bic', 'cubic')
for (cctype in cctype.list) {
  cwnd <- read.table(paste("cwnd", "_", cctype, ".txt", sep=""), header=T)
  rtt <- read.table(paste("rtt", "_", cctype, ".txt", sep=""), header=T)
  rate <- read.table(paste("rate", "_", cctype, ".txt", sep=""), header=T)
  
  summary(cwnd)
  summary(rtt)
  summary(rate)
  
  rtt.melt <- melt(rtt, id=c('time', 'segment'))
  
  summary(rtt.melt)
  
  g.cwnd <- ggplot(cwnd, aes (x=time, y=cwndsize)) +
    geom_line(size=0.8) + 
    ylab("Cwnd [Pkts]") +
    labs(type='custom text')
  g.cwnd
    
  g.rate <- ggplot(rate, aes (x=time, y=kbps/1000)) +
    geom_line(size=0.8) + 
    ylab("Rate [Mbit/s]") +
    ylim(0,350) +
    labs(type='custom text') 
  g.rate
  
  g.rtt <- ggplot(rtt.melt, aes (x=time, y=value, color=variable)) +
    geom_line(size=0.8) + 
    ylab("RTT") +
    xlab("") +
    labs(type='custom text')  + theme(legend.position="top")
  g.rtt
  
  pdfwidth <- 7
  
  pdf(paste("./graphs/", cctype, "1.pdf", sep=""), useDingbats=F, height=4, width=pdfwidth)
  grid.arrange(g.rate, g.cwnd, top = paste("rate & cwnd: ", cctype))
  dev.off()
  
  pdf(paste("./graphs/", cctype, "2.pdf", sep=""), useDingbats=F, height=4, width=pdfwidth)
  #g.rtt <- g.rtt + labs(title = paste("rtt: ", cctype))
  grid.arrange(g.rtt)
  dev.off()
}

