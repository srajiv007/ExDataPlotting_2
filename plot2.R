install.packages("data.table")
library(data.table)

read.data.source<-function(dir="data/exdata-data-NEI_data"){
        nei<-readRDS(paste0(c(dir, "summarySCC_PM25.rds"), collapse="/"))
        scc<-readRDS(paste0(c(dir, "Source_Classification_Code.rds"), collapse = "/"))
        nei_source<-merge(nei, scc, by.x = "SCC", by.y = "SCC")
        
        nei.dt<-data.table(nei_source)
        return(nei.dt)
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

nei.dt<-read.data.source()

#2. Generating Plot 2
print.plot2(nei.dt)
