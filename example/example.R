setwd("C:/Users/bai.li/Documents/StockSMARTVisualizer")
devtools::load_all()

setwd("C:/Users/bai.li/Documents/StockSMARTVisualizer/example/")
library(rgdal)
library(maps)
library(mapdata)
lme_spdf <- readOGR(
  dsn = file.path(getwd(), "data", "LME", "LMEs66.shp"),
  verbose = FALSE
) # The GIS files are downloaded from USGS: https://www.sciencebase.gov/catalog/item/55c77722e4b08400b1fd8244

lme_names <- lme_spdf$LME_NAME %in% lme_spdf$LME_NAME[c(60, 66, 4, 10, 25, 21, 12, 24, 28, 19, 20)]
us_lme <- lme_spdf[lme_names, ]

# png(file=file.path(getwd(), "LargeMarineEcosystemsMap.png"),
#     width=170, height=120, units="mm", res=1200)
par(oma=c(0, 0, 0, 10))
plot(us_lme, col=rainbow(length(us_lme$LME_NAME)),
     xlim=c(-175, -60), ylim=c(-5, 73))
map("world", add=T, fill=T, col="gray90")
points(coordinates(us_lme), pch=21, cex=3,
       bg="white")
text(coordinates(us_lme), as.character(1:length(us_lme$LME_NAME)))

degAxis(1)
degAxis(2, las=2)
box()
legend(par('usr')[2], par('usr')[4], xpd=NA,
       title="Large Marine Ecosystems",
       paste0(1:length(us_lme$LME_NAME), ":", us_lme$LME_NAME, sep=""),
       cex=0.8, bty="n")
# dev.off()

raw_data <- read_stocksmart_data(
  filedir = system.file("extdata", package="StockSMARTVisualizer"),
  filename = "stocksmart.csv"
)

model_names <- find_model_names(data=raw_data,
                                model=c("AMAK", "ASAP",
                                        "BAM", "SS"))

# model_names$amak
id <- c(9, 37, 38)
data <- standardize_model_names(data=raw_data,
                                model="AMAK",
                                names=model_names$amak[id])

# model_names$asap
id <- c(12, 14, 33, 34, 35, 37, 38)
data <- standardize_model_names(data=data,
                                model="ASAP",
                                names=model_names$asap[id])

# model_names$bam
id <- c(4, 9, 10, 12)
data <- standardize_model_names(data=data,
                                model="BAM",
                                names=model_names$bam[id])

# model_names$ss
id <- c(15, 16, 17, 18, 21, 26, 30, 31, 32, 33, 34, 35, 36, 40, 43, 44, 45, 46, 48, 49, 51, 52, 55, 56, 58, 59, 60, 61, 62)
data <- standardize_model_names(data=data,
                                model="SS",
                                names=model_names$ss[id])

summary(as.factor(data$Assessment.Model.Standardize))

# png(file=file.path(getwd(), "AssessmentModel.png"),
#     width=170, height=120, units="mm", res=1200)

key_model <- c("AMAK", "ASAP", "BAM", "SS")
subdata <- data[(data$Assessment.Model.Standardize %in% key_model),]


barplot(table(as.factor(subdata$Life.History.Data),
      as.factor(subdata$Assessment.Year)), col=rainbow(5))

par(mfrow=c(1,3))
a <- table(as.factor(subdata$Life.History.Data),
           as.factor(subdata$Regional.Ecosystem))

b <- apply(a, 2, function(x) {x*100/sum(x, na.rm=T)})
barplot(b, col=rainbow(5))

a <- table(as.factor(subdata$Abundance.Data),
           as.factor(subdata$Regional.Ecosystem))

b <- apply(a, 2, function(x) {x*100/sum(x, na.rm=T)})
barplot(b, col=rainbow(5))

a <- table(as.factor(subdata$Catch.Data),
           as.factor(subdata$Regional.Ecosystem))

b <- apply(a, 2, function(x) {x*100/sum(x, na.rm=T)})
barplot(b, col=rainbow(5))

par(mfrow=c(1,3))
a <- table(as.factor(subdata$Life.History.Data),
           as.factor(subdata$Science.Center))

b <- apply(a, 2, function(x) {x*100/sum(x, na.rm=T)})
barplot(b, col=rainbow(5))

a <- table(as.factor(subdata$Abundance.Data),
           as.factor(subdata$Science.Center))

b <- apply(a, 2, function(x) {x*100/sum(x, na.rm=T)})
barplot(b, col=rainbow(5))

a <- table(as.factor(subdata$Catch.Data),
           as.factor(subdata$Science.Center))

b <- apply(a, 2, function(x) {x*100/sum(x, na.rm=T)})
barplot(b, col=rainbow(5))


a <- table(as.factor(subdata$Regional.Ecosystem),
           as.factor(subdata$Catch.Data))

b <- apply(a, 2, function(x) {x*100/sum(x, na.rm=T)})
barplot(b, col=rainbow(11))

a <- table(as.factor(subdata$Regional.Ecosystem),
           as.factor(subdata$Abundance.Data))

b <- apply(a, 2, function(x) {x*100/sum(x, na.rm=T)})
barplot(b, col=rainbow(11))

a <- table(as.factor(subdata$Regional.Ecosystem),
           as.factor(subdata$Life.History.Data))

b <- apply(a, 2, function(x) {x*100/sum(x, na.rm=T)})
barplot(b, col=rainbow(11))


library(mgcv)



subdata$Regional.Ecosystem <- as.factor(subdata$Regional.Ecosystem)
model1 <- gam(B.Blimit~s(F.Flimit, by=Regional.Ecosystem), data=subdata)
summary(model1)

library(tidymv)
model_p <- predict_gam(model1)

library(dplyr)
model_p %>%
  ggplot(aes(F.Flimit, fit)) +
  geom_smooth_ci(Regional.Ecosystem)
