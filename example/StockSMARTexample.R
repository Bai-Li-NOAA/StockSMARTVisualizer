
# Set working directory -----------------------------------------------------------------------

setwd("C:/Users/bai.li/Documents/StockSMARTVisualizer")
devtools::load_all()

setwd("C:/Users/bai.li/Documents/StockSMARTVisualizer/example/")


# Load large marine ecosystem data ------------------------------------------------------------

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


# Fisheries Science Centers -------------------------------------------------------------------

science_centers <- read.csv(file.path(getwd(), "data", "ScienceCentersLocation.csv"))

# png(file=file.path(getwd(), "ScienceCenters.png"),
#     width=170, height=120, units="mm", res=1200)
par(oma=c(0, 0, 0, 0))
plot(us_lme, col="lightblue",
     xlim=c(-170, -60), ylim=c(-5, 73))
map("world", add=T, fill=T, col="gray90")
points(science_centers$Longitude, science_centers$Latitude,
       pch=16, cex=2, col="red")

line_distance <- 20
for(i in 1:nrow(science_centers)){

  if (science_centers$Science.Center[i] %in% c("PIFSC", "SWFSC")) {
    segments(x0=science_centers$Longitude[i],
             y0=science_centers$Latitude[i],
             x1=science_centers$Longitude[i]-line_distance,
             y1=science_centers$Latitude[i],
             col="red", lty=2)
    text(science_centers$Longitude[i]-line_distance*1.2,
         science_centers$Latitude[i]*1.05,
         science_centers$Science.Center[i], col="red")
  }

  if (science_centers$Science.Center[i] %in% c("NEFSC", "SEFSC")) {
    segments(x0=science_centers$Longitude[i],
             y0=science_centers$Latitude[i],
             x1=science_centers$Longitude[i]+line_distance,
             y1=science_centers$Latitude[i],
             col="red", lty=2)
    text(science_centers$Longitude[i]+line_distance*1.2,
         science_centers$Latitude[i]*1.05,
         science_centers$Science.Center[i], col="red")
  }

  if (science_centers$Science.Center[i] %in% c("NWFSC")) {
    segments(x0=science_centers$Longitude[i],
             y0=science_centers$Latitude[i],
             x1=science_centers$Longitude[i]-line_distance,
             y1=science_centers$Latitude[i]-line_distance/3,
             col="red", lty=2)
    text((science_centers$Longitude[i]-line_distance)*1.05,
         (science_centers$Latitude[i]-line_distance/3)*1.05,
         science_centers$Science.Center[i], col="red")
  }

  if (science_centers$Science.Center[i] %in% c("AFSC")) {
    segments(x0=science_centers$Longitude[i],
             y0=science_centers$Latitude[i],
             x1=science_centers$Longitude[i]-line_distance,
             y1=science_centers$Latitude[i]+line_distance/3,
             col="red", lty=2)
    text((science_centers$Longitude[i]-line_distance)*1.05,
         (science_centers$Latitude[i]+line_distance/3),
         science_centers$Science.Center[i], col="red")
  }

}

degAxis(1)
degAxis(2, las=2)
box()


# Number of assessments per model -------------------------------------------------------------

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

key_model <- c("AMAK", "ASAP", "BAM", "SS")
subdata <- data[(data$Assessment.Model.Standardize %in% key_model),]
subdata <- subdata[(subdata$Assessment.Year > 2012),]

barplot_data <- as.matrix(table(subdata$Science.Center, subdata$Assessment.Model.Standardize))
barplot_data

science_centers <- read.csv(file.path(getwd(), "data", "ScienceCentersLocation.csv"))

# png(file=file.path(getwd(), "ScienceCenters2010.png"),
#     width=170, height=120, units="mm", res=1200)
par(oma=c(0, 0, 0, 0))
plot(us_lme, col="lightblue",
     xlim=c(-170, -60), ylim=c(-5, 73))
map("world", add=T, fill=T, col="gray90")
points(science_centers$Longitude, science_centers$Latitude,
       pch=16, cex=2, col="red")

line_distance <- 20
col <- c("orange", "green", "red", "deepskyblue3")
barwidth=3
maxheight=5
legend_title <- paste(min(subdata$Assessment.Year), " - ", max(subdata$Assessment.Year))
source(file.path(getwd(), "map_bar_plot.R"))
