install.packages("data.table")
install.packages("ggplot2")

library(ggplot2)
library(data.table)

read.data.source<-function(dir="data/exdata-data-NEI_data"){
        nei<-readRDS(paste0(c(dir, "summarySCC_PM25.rds"), collapse="/"))
        scc<-readRDS(paste0(c(dir, "Source_Classification_Code.rds"), collapse = "/"))
        nei_source<-merge(nei, scc, by.x = "SCC", by.y = "SCC")
        
        nei.dt<-data.table(nei_source)
        return(nei.dt)
}

create.ggplot2<-function(data, title){
        
        t=data
        p<-qplot(year, total, data = t)+labs(x="Year", y=expression(paste("PM"[2.5], " Emission (Total)")), title=title)
        p<-p+theme(plot.title=element_text(face="bold", vjust = 2, size=20), 
                   axis.title.x=element_text(face="bold", size=15), 
                   axis.title.y=element_text(face="bold", size=15),
                   axis.text.x=element_text(size=10, vjust=0.5, angle=50))
        p<-p+geom_point(color = "firebrick")+stat_smooth(method = "lm")
        return(p)
}

print.png<-function(p, dir, png.file){
        png(paste0(c(dir, png.file), collapse="/"), height = 480, width = 480)
        print(p)
        dev.off()
}

print.plot6<-function(data, dir = "Project2 (Exploratory Analysis)"){
        nei.dt<-data[data$fips %in% c("24510","06037"), ]
        t<-nei.dt[grep("On-Road", nei.dt$EI.Sector), ]
        t<-t[, .("total"=sum(Emissions)), by=.(year, fips)]
        
        p<-create.ggplot2(t, "Motor Vehicle Emission\nLA County vs. Baltimore City")+
                facet_wrap(~fips, ncol=1, scales="free")
        print.png(p, dir, "plot6.png")
}

nei.dt<-read.data.source()

# 6. Generating Plot 6
print.plot6(nei.dt)
