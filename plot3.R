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

print.plot3<-function(data, dir = "Project2 (Exploratory Analysis)"){
        nei.dt<-data[data$fips=="24510", ]
        
        t<-nei.dt[, .("total"=sum(Emissions)), by=.(year, type)]
        p<-create.ggplot2(t, "Total Emissions\n(by Type)")+facet_wrap(~type, ncol=2, scales="free")
        print.png(p, dir, "plot3.png")
}

nei.dt<-read.data.source()

# 3. Generating Plot 3
print.plot3(nei.dt)

