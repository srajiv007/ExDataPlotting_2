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

print.plot1<-function(data, dir = "Project2 (Exploratory Analysis)"){
        nei.dt<-data
        t<-nei.dt[, .("total"=sum(Emissions)), by = year]
        par(mar=c(1, 1, 1, 1))
        x<-t$year
        y<-t$total
        png(paste0(c(dir, "plot1.png"), collapse="/"), height = 480, width = 480)
        plot(x,y, main="Total Emission", xlab="Year", ylab="Emission (Total)", pch=20, col="firebrick")
        model<-lm(y ~ x)
        abline(model, lwd=2, col="blue")
        dev.off()
}

print.plot2<-function(data, dir = "Project2 (Exploratory Analysis)"){
        nei.dt<-data
        par(mar=c(1, 1, 1, 1))
        baltimore.nei<-nei.dt[nei.dt$fips=="24510", ]
        maryland.emissions<-baltimore.nei[, .("total.emission"=sum(Emissions)), by = .(year)]
        x<-maryland.emissions$year
        y<-maryland.emissions$total.emission
        png(paste0(c(dir, "plot2.png"), collapse="/"), height = 480, width = 480)
        plot(x,y, main="Baltimore City Emissions\n (fips=24510)", xlab="Year", ylab="Emission (Total)", pch=20, col="blue")
        model<-lm(y ~ x)
        abline(model, lwd=2, col="red")
        dev.off()
        
}

print.plot3<-function(data, dir = "Project2 (Exploratory Analysis)"){
        nei.dt<-data[data$fips=="24510", ]
        
        t<-nei.dt[, .("total"=sum(Emissions)), by=.(year, type)]
        p<-create.ggplot2(t, "Total Emissions\n(by Type)")+facet_wrap(~type, ncol=2, scales="free")
        print.png(p, dir, "plot3.png")
}

print.plot4<-function(data, dir = "Project2 (Exploratory Analysis)"){
        nei.dt<-data
        coal.dt<-nei.dt[grep("coal", nei.dt$Short.Name), ]
        
        t<-coal.dt[, .("total"=sum(Emissions)), by =year]
        p<-create.ggplot2(t, "Total Coal Emissions\n(USA)")
        print.png(p, dir, "plot4.png")
}

print.plot5<-function(data, dir = "Project2 (Exploratory Analysis)"){
        # filter for Baltimore City and Motor vehicle emissions
        nei.dt<-data[data$fips == "24510", ]
        t<-nei.dt[grep("On-Road", nei.dt$EI.Sector), ]
        
        t<-t[, .("total"=sum(Emissions)), by=year]
        p<-create.ggplot2(t, "Motor Vehicle Emission\nBaltimore City")
        print.png(p, dir, "plot5.png")
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

# 1. Generating Plot 1
print.plot1(nei.dt)

#2. Generating Plot 2
print.plot2(nei.dt)

# 3. Generating Plot 3
print.plot3(nei.dt)

# 4. Generating Plot 4
print.plot4(nei.dt)

# 5. Generating Plot 5
print.plot5(nei.dt)

# 6. Generating Plot 6
print.plot6(nei.dt)
