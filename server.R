library(shiny)
library(UsingR)
library(ggplot2)
#data(galton)
dta <- mtcars

dta$gear <- factor(dta$gear,levels=c(3,4,5),
                      labels=c("3gears","4gears","5gears")) 
dta$am <- factor(dta$am,levels=c(0,1),
                    labels=c("Automatic","Manual")) 
dta$cyl <- factor(dta$cyl,levels=c(4,6,8),
                     labels=c("4cyl","6cyl","8cyl")) 

dt <- dta
dt3gear <- dt$gear=="3gears"
dt4gear <- dt$gear=="4gears"
dt5gear <- dt$gear=="5gears"

dt4cylinder <- dt$cyl=="4cyl"
dt6cylinder <- dt$cyl=="6cyl"
dt8cylinder <- dt$cyl=="8cyl"


shinyServer(
        function(input, output) {
                output$newHist <- renderPlot({
                        #hist(galton$child, xlab='child height', col='lightblue',main='Histogram')
                        formulaText <- reactive(function() {
                                grs<- paste("", input$chkGear,collapse=", ")
                                cyl<- paste("", input$chkCylinders,collapse=", ")
                                paste("gear(s) ",grs, "number of cylinders ",cyl)
                        })
                        output$caption <- reactiveText(function() {
                                formulaText()
                        })
                        
                        hasGear3 <- sum(input$chkGear==3)
                        hasGear4 <- sum(input$chkGear==4)
                        hasGear5 <- sum(input$chkGear==5)
                        
                        has4Cylinders <- sum(input$chkCylinders==4)
                        has5Cylinders <- sum(input$chkCylinders==6)
                        has6Cylinders <- sum(input$chkCylinders==8)
                        
                        if(has4Cylinders & !has5Cylinders & !has6Cylinders) {
                                dt3gear <- dt3gear & dt4cylinder
                                dt4gear <- dt4gear & dt4cylinder
                                dt5gear <- dt5gear & dt4cylinder
                        }
                        if(hasGear3 & has5Cylinders & !has6Cylinders){ 
                                #dt <- dt[dt3gear|dt4gear,]
                                dt3gear <- dt3gear & (dt4cylinder|dt6cylinder)
                                dt4gear <- dt4gear & (dt4cylinder|dt6cylinder)
                                dt5gear <- dt5gear & (dt4cylinder|dt6cylinder)
                        }
                        if (has4Cylinders &!has5Cylinders & has6Cylinders) {
                                #dt <- dt[dt3gear|dt5gear,]
                                dt3gear <- dt3gear & (dt4cylinder|dt8cylinder)
                                dt4gear <- dt4gear & (dt4cylinder|dt8cylinder)
                                dt5gear <- dt5gear & (dt4cylinder|dt8cylinder)
                        }
                        if (!has4Cylinders & has5Cylinders & !has6Cylinders) {
                                #dt <- dt[dt4gear,]
                                dt3gear <- dt3gear & (dt6cylinder)
                                dt4gear <- dt4gear & (dt6cylinder)
                                dt5gear <- dt5gear & (dt6cylinder)
                        }
                        if (!has4Cylinders &has5Cylinders & has6Cylinders) {
                                #dt <- dt[dt4gear|dt5gear,]
                                dt3gear <- dt3gear & (dt6cylinder|dt8cylinder)
                                dt4gear <- dt4gear & (dt6cylinder|dt8cylinder)
                                dt5gear <- dt5gear & (dt6cylinder|dt8cylinder)
                        }
                        if (!has4Cylinders & !has5Cylinders & has6Cylinders) {
                                #dt <- dt[dt5gear]
                                dt3gear <- dt3gear & dt8cylinder
                                dt4gear <- dt4gear & dt8cylinder
                                dt5gear <- dt5gear & dt8cylinder
                        }
                        
                        dt <- dta
                        if(hasGear3 & !hasGear4 & !hasGear5) dt <- dt[dt3gear,]
                        if(hasGear3 & hasGear4 & !hasGear5) dt <- dt[dt3gear|dt4gear,]
                        if (hasGear3 &!hasGear4 & hasGear5) dt <- dt[dt3gear|dt5gear,]
                        if (!hasGear3 & hasGear4 & !hasGear5) dt <- dt[dt4gear,]
                        if (!hasGear3 &hasGear4 & hasGear5) dt <- dt[dt4gear|dt5gear,]
                        if (!hasGear3 & !hasGear4 & hasGear5) dt <- dt[dt5gear]
                        
                        
                        print(qplot(mpg, data=dt, geom="density", fill=gear, alpha=I(.5), 
                              main="Distribution of Gas Milage", xlab="Miles Per Gallon", 
                              ylab="Density"))
                        #p <- qplot(hp, mpg, data=mtcars, shape=am, color=am, 
                        #           facets=gear~cyl, main="Scatterplots of MPG vs. Horsepower",
                        #           xlab="Horsepower", ylab="Miles per Gallon")
                        #p + theme_bw()
                        #p + theme(axis.title=element_text(face="bold.italic", 
                        #                                  size="12", color="brown"), legend.position="top")
                        mu <- input$mu
                        ##lines(c(mu, mu), c(0, 200),col="red",lwd=5)
                        mse <- mean((galton$child - mu)^2)
                        
                        ##text(63, 150, paste("mu = ", mu))
                        ##text(63, 140, paste("MSE = ", round(mse, 2)))
                })
                
        }
)
