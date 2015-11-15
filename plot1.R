install.packages("data.table")
library(data.table)

read.data.source<-function(dir="data/exdata-data-NEI_data"){
        nei<-readRDS(paste0(c(dir, "summarySCC_PM25.rds"), collapse="/"))
        scc<-readRDS(paste0(c(dir, "Source_Classification_Code.rds"), collapse = "/"))
        nei_source<-merge(nei, scc, by.x = "SCC", by.y = "SCC")
        
        nei.dt<-data.table(nei_source)
        return(nei.dt)
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

nei.dt<-read.data.source()

# 1. Generating Plot 1
print.plot1(nei.dt)

