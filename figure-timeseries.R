library(ggplot2)
library(animint2)
library(dplyr)
library(data.table)
library(RColorBrewer)
library(RJSONIO)
library(grid)

load("velos.RData")
load("bike.paths.RData")
load("counter.locations.RData")
load("accidents.RData")

montreal.bikes <- list(
  counter.counts=data.frame(velos),
  counter.locations=data.frame(counter.locations),
  path.locations=data.frame(bike.paths),
  accidents=data.frame(accidents))
save(montreal.bikes, file="montreal.bikes.RData", compress="xz")

one.day <- 60 * 60 * 24

accidents.dt <- data.table(accidents)
accidents.dt[, date.POSIXct := as.POSIXct(strptime(date.str, "%Y-%m-%d"))]
accidents.dt[, month.str := strftime(date.POSIXct, "%Y-%m")]

velos.dt <- data.table(velos)[!(count==0 | is.na(count)),]
velos.dt[, month.str := strftime(date, "%Y-%m")]
velos.dt[, day.of.the.month := as.integer(strftime(date, "%d"))]

uniq.month.vec <- unique(c(
  accidents.dt$month.str,
  velos.dt[, month.str]))
months <- data.table(month.str=uniq.month.vec)
months[, month01.str := paste0(month.str, "-01")]
months[, month01.POSIXct := as.POSIXct(strptime(month01.str, "%Y-%m-%d"))]
months[, next.POSIXct := month01.POSIXct + one.day * 31]
months[, next01.str := paste0(strftime(next.POSIXct, "%Y-%m"), "-01")]
months[, next01.POSIXct := as.POSIXct(strptime(next01.str, "%Y-%m-%d"))]
months[, month.str := strftime(month01.POSIXct, "%B %Y")]

month.levs <- months[order(month01.POSIXct), month.str]
months[, month := factor(month.str, month.levs)]

accidents.dt[, month.text := strftime(date.POSIXct, "%B %Y")]
accidents.dt[, month := factor(month.text, month.levs)]
accidents.dt[, month.POSIXct := as.POSIXct(
  strptime(paste0(month.str, "-15"), "%Y-%m-%d"))]
stopifnot(!is.na(accidents.dt$month.POSIXct))
accidents.per.month <- accidents.dt[, list(
  total.accidents=.N,
  total.people=sum(deaths+people.severely.injured+people.slightly.injured),
  deaths=sum(deaths),
  people.severely.injured=sum(people.severely.injured),
  people.slightly.injured=sum(people.slightly.injured)
), by=.(month, month.str, month.text, month.POSIXct)]
accidents.per.month[, next.POSIXct := month.POSIXct + one.day * 30]
accidents.per.month[, month01.str := paste0(strftime(month.POSIXct, "%Y-%m"), "-01")]
accidents.per.month[, month01.POSIXct := as.POSIXct(strptime(month01.str, "%Y-%m-%d"))]
accidents.per.month[, next01.str := paste0(strftime(next.POSIXct, "%Y-%m"), "-01")]
accidents.per.month[, next01.POSIXct := as.POSIXct(strptime(next01.str, "%Y-%m-%d"))]

severity.vec <- c(
  "people slightly injured",
  "people severely injured",
  "deaths")
accidents.by.severity <- list()
for(severity in severity.vec){
  severity.col <- gsub(" ", ".", severity)
  people <- accidents.dt[[severity.col]]
  accidents.by.severity[[severity]] <-
    data.table(accidents.dt,
               severity=factor(severity, severity.vec),
               people)
}
accidents.tall <- do.call(rbind, accidents.by.severity)[0 < people,]
accidents.per.month.tall <- accidents.tall[, list(
  people=sum(people)
), by=.(month, month.str, month.text, month.POSIXct, severity)]
accidents.per.day.tall <- accidents.tall[, list(
  people=sum(people)
), by=.(date.POSIXct, severity, month.POSIXct, month, month.text, month.str)]

## dots for each accident.
accidents.dt[, severity.str := ifelse(
  0 < deaths, "deaths", ifelse(
    0 < people.severely.injured,
    "people severely injured",
    "people slightly injured"))]
accidents.dt[, severity := factor(severity.str, severity.vec)]
setkey(accidents.dt, date.POSIXct, severity)
accidents.cumsum <-
  accidents.dt[, accident.i := seq_along(severity), by=.(date.POSIXct, month)]

velos.dt[, month.text := strftime(date, "%B %Y")]
velos.dt[, month := factor(month.text, month.levs)]
velos.dt[, month.POSIXct := as.POSIXct(
  strptime(paste0(month.str, "-15"), "%Y-%m-%d"))]
counts.per.month <- velos.dt[, list(
  days=.N,
  mean.per.day=mean(count),
  count=sum(count)
), by=.(location, month, month.str, month.POSIXct)][0 < count,]
counts.per.month[, month01.str := paste0(month.str, "-01")]
counts.per.month[, month01.POSIXct := as.POSIXct(strptime(month01.str, "%Y-%m-%d"))]
counts.per.month[, next.POSIXct := month01.POSIXct + one.day * 31]
counts.per.month[, next01.str := paste0(strftime(next.POSIXct, "%Y-%m"), "-01")]
counts.per.month[, next01.POSIXct := as.POSIXct(strptime(next01.str, "%Y-%m-%d"))]
counts.per.month[, days.in.month := round(next01.POSIXct - month01.POSIXct)]
counts.per.month[days < days.in.month, ]
complete.months <- counts.per.month[days == days.in.month, ]
month.labels <- counts.per.month[, {
  .SD[which.max(count), ]
}, by=location]
day.labels <- velos.dt[, {
  .SD[which.max(count), ]
}, by=.(location, month)]
city.wide.cyclists <- counts.per.month[0 < count, list(
  locations=.N,
  count=sum(count)
), by=.(month, month.str, month.POSIXct)]
city.wide.cyclists[, month01.str := paste0(month.str, "-01")]
city.wide.cyclists[, month01.POSIXct := as.POSIXct(strptime(month01.str, "%Y-%m-%d"))]
city.wide.cyclists[, next.POSIXct := month01.POSIXct + one.day * 31]
city.wide.cyclists[, next01.str := paste0(strftime(next.POSIXct, "%Y-%m"), "-01")]
city.wide.cyclists[, next01.POSIXct := as.POSIXct(strptime(next01.str, "%Y-%m-%d"))]

month.str.vec <- strftime(seq(
  strptime("2012-01-15", "%Y-%m-%d"),
  strptime("2013-01-15", "%Y-%m-%d"),
  by="month"), "%Y-%m")

city.wide.complete <- complete.months[0 < count, list(
  locations=.N,
  count=sum(count)
), by=.(month, month.str, month.POSIXct)]
city.wide.complete[, month01.str := paste0(month.str, "-01")]
setkey(city.wide.complete, month.str)
scatter.cyclists <- city.wide.complete[month.str.vec,]
setkey(accidents.per.month, month.str)
setkey(scatter.cyclists, month.str)
scatter.accidents <- accidents.per.month[scatter.cyclists,]
scatter.not.na <- scatter.accidents[!is.na(locations),]
scatter.max <- scatter.not.na[locations==max(locations),]
fit <- lm(total.accidents ~ count - 1, scatter.max)
scatter.max[, mean(total.accidents/count)]
scatter.max[, pred.accidents := predict(fit)]

##dput(RColorBrewer::brewer.pal(10, "Reds"))
severity.colors <- 
  c("#FFF5F0",#lite red
    "people slightly injured"="#FEE0D2",
    "#FCBBA1",
    "#FC9272",
    "people severely injured"="#FB6A4A",
    "#EF3B2C", 
    "#CB181D",
    deaths="#A50F15",
    "#67000D")#dark red

accidents.cumsum[, day.of.the.month := as.integer(strftime(date.POSIXct, "%d"))]

accidents.per.day.cumsum <- accidents.per.day.tall[, {
  cs <- cumsum(people)
  data.table(max=cs, min=cs-people, severity)
}, by=.(date.POSIXct, month)]

counter.locations[, lon := coord_X]
counter.locations[, lat := coord_Y]

loc.name.code <- c(
  "Berri1"="Berri",
  "Brebeuf"="Brébeuf",
  CSC="Côte-Sainte-Catherine",
  "Maisonneuve_1"="Maisonneuve 1",
  "Maisonneuve_2"="Maisonneuve 2",
  "Parc"="du Parc",
  PierDup="Pierre-Dupuy",
  "Rachel/Papineau"="Rachel",
  "Saint-Urbain"="Saint-Urbain",
  "Totem_Laurier"="Totem_Laurier")

counter.locations[, location := loc.name.code[nom_comptage]]
velo.counts <- table(velos$location)
setkey(counter.locations, location)
show.locations <- counter.locations[names(velo.counts)]
map.lim <- show.locations[, list(
  range.lat=range(lat),
  range.lon=range(lon)
)]
diff.vec <- sapply(map.lim, diff)
diff.mat <- c(-1, 1) * matrix(diff.vec, 2, 2, byrow=TRUE)
scale.mat <- as.matrix(map.lim) + diff.mat

location.colors <-
  c("#32d6ba", "#7649c8", "#a799ff", "#FB8072", "#0591f5", "#fc9d31", 
    "#93df10", "#f58ac1", "#a77b7b", "#d21bd5", "#a5a26b", "#226243")
names(location.colors) <- show.locations$location

setkey(show.locations, location)
setkey(counts.per.month, location)
counts.per.month.loc <- counts.per.month[show.locations,]

some.paths <- bike.paths[
  scale.mat[1, "range.lat"] < lat &
    scale.mat[1, "range.lon"] < lon &
    lat < scale.mat[2, "range.lat"] &
    lon < scale.mat[2, "range.lon"]]
print("x limit is : ")
print(map.lim$range.lon)

mtl.map <- ggplot() +
    # theme_bw() +
    theme_animint(height = 550 , width = 800) +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(), 
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank()
      # panel.border = element_blank()
    ) +
    coord_equal(xlim = c(map.lim$range.lon[1] - 0.01 , map.lim$range.lon[2] + 0.02), ylim = c(map.lim$range.lat[1]-0.03, map.lim$range.lat[2]+0.02) ) +
    scale_color_manual(values = location.colors) +
    scale_x_continuous(limits = scale.mat[, "range.lon"]) +
    scale_y_continuous(limits = scale.mat[, "range.lat"]) +
    # Bike paths
    geom_path(
      aes(lon, lat, group = paste(feature.i, path.i)),
      data = some.paths,
      color = "grey"
    ) +
    # Location points
    geom_point(
      aes(lon, lat, color = location, size = count),
      data = counts.per.month.loc,
      showSelected = "month",
      clickSelects = "location"
    ) +
    scale_size_animint() +
    # Location labels
    geom_text(
      aes(lon, lat, label = location),
      data = show.locations,
      clickSelects = "location"
    )+guides(color = "none") 

dates <- velos %>%
  group_by(date) %>%
  summarise(locations=sum(!is.na(count))) %>%
  filter(locations > 0) %>%
  mutate(min.date=date-one.day/2,
         max.date=date+one.day/2)

location.labels <- velos %>%
  group_by(location) %>%
  filter(seq_along(count)==which.max(count))

location.ranges <- counts.per.month[0 < count, list(
  min=min(month.POSIXct),
  max=max(month.POSIXct)
), by=location]



accidents.range <- accidents.dt[, data.table(
  location="accidents",
  min=min(date.POSIXct),
  max=max(date.POSIXct))]

counter.title <- "mean cyclists per day"
accidents.title <- "city-wide accidents"

MonthSummary <- ggplot() +
    theme_bw() +
    theme_animint(width = 400, height = 500) +
    xlab("range of dates in data") +
    ylab("data type") +
    scale_color_manual(values = location.colors) +
    guides(color = "none") +
    # Month selection rectangles - fixed make_tallrect usage
    geom_tallrect(
      aes(xmin = month01.POSIXct, xmax = next01.POSIXct),
      data = months,
      alpha = 0.5,
      clickSelects = "month"
    ) +
    # Location ranges
    geom_segment(
      aes(min, location, xend = max, yend = location, color = location),
      data = location.ranges,
      alpha = 0.75,
      size = 10,
      clickSelects = "location"
    ) +
    # Accident range
    geom_segment(
      aes(min, location, xend = max, yend = location),
      data = accidents.range,
      color = severity.colors[["deaths"]],
      size = 10
    )

accidents.per.month.sum <- accidents.tall[, 
  .(total_people = sum(people)), 
  by = .(month.POSIXct, severity, month, month.str, month.text)
]

MonthFacet <-
  ggplot() +
  ggtitle("All data, select month") +
  guides(color = "none", fill = "none") +
  theme_bw() +
  facet_grid(facet ~ ., scales = "free") +
  theme_animint(height = 500, width = 600) +
  
  # Background rectangles for month selection
  geom_tallrect(aes(xmin = month01.POSIXct, xmax = next01.POSIXct),
                clickSelects = "month",
                data = data.table(city.wide.cyclists, facet = counter.title),
                alpha = 1/2) +
  
  # Lines for bike counts
  geom_line(aes(month.POSIXct, mean.per.day, group = location, color = location),
            showSelected = "location",
            clickSelects = "location",
            data = data.table(counts.per.month, facet = counter.title)) +
  
  # Points for bike counts
  geom_point(aes(month.POSIXct, mean.per.day, color = location,
               tooltip = paste(
                 count, "cyclists counted at",
                 location, "in",
                 days, "days of", month,
                 sprintf("(mean %d cyclists/day)", as.integer(mean.per.day)))),
            showSelected = "location",
            clickSelects = "location",
            size = 5,
            data = data.table(counts.per.month, facet = counter.title)) +
  
  # Location labels
  geom_text(aes(month.POSIXct, mean.per.day + 300, color = location, label = location),
            showSelected = "location",
            clickSelects = "location",
            data = data.table(month.labels, facet = counter.title)) +
  
  # Color scale for locations (used in lines, points, and text)
  scale_color_manual(values = location.colors) +
  
  # Accident bars - using fill for severity
  geom_bar(aes(month.POSIXct, total_people, fill = severity),
           showSelected = "severity",
           stat = "identity",
           position = "stack",
           color = NA,
           data = data.table(accidents.per.month.sum, facet = accidents.title)) +
  
  # Fill scale for severity only
  scale_fill_manual(values = severity.colors, breaks = rev(severity.vec)) +
  
  # Month selection rectangles for accidents
  geom_tallrect(aes(xmin = month01.POSIXct, xmax = next01.POSIXct,
                    tooltip = paste(
                      ifelse(deaths == 0, "",
                             ifelse(deaths == 1,
                                    "1 death,",
                                    paste(deaths, "deaths,"))),
                      ifelse(people.severely.injured == 0, "",
                             ifelse(people.severely.injured == 1,
                                    "1 person severely injured,",
                                    paste(people.severely.injured,
                                          "people severely injured,"))),
                      people.slightly.injured,
                      "people slightly injured in",
                      month)),
                clickSelects = "month",
                alpha = 0.5,
                data = data.table(accidents.per.month, facet = accidents.title)) +
  
  xlab("month") +
  ylab("")

days.dt <- data.table(day.POSIXct=with(months, seq(
  min(month01.POSIXct),
  max(next01.POSIXct),
  by="day")))
days.dt[, day.of.the.week := strftime(day.POSIXct, "%a")]
weekend.dt <- days.dt[day.of.the.week %in% c("Sat", "Sun"),]
weekend.dt[, month.text := strftime(day.POSIXct, "%B %Y")]
weekend.dt[, month := factor(month.text, month.levs)]
weekend.dt[, day.of.the.month := as.integer(strftime(day.POSIXct, "%d"))]
counter.title <- "cyclists per day"
DaysFacet <- 
ggplot()+
  theme_bw()+
  theme_animint(height = 500,width=600)+
  ggtitle("Selected month (weekends in grey)")+
   geom_tallrect(
    aes(
      xmin = day.of.the.month - 0.5, 
      xmax = day.of.the.month + 0.5,
      key = day.POSIXct 
    ),
    showSelected = "month",
    alpha = 0.85,
    color = NA,
    fill = "grey",
    data = unique(weekend.dt, by = "day.POSIXct")
  ) +
  # guides(color="none")+
  
  facet_grid(facet ~ ., scales="free")+
  geom_line(
  aes(day.of.the.month, count, group = location, color = location, key = location),
  showSelected = "month",
  clickSelects = "location",
  data = data.table(velos.dt, facet = counter.title)
) +
  
  ylab("")+
  geom_point(
  aes(day.of.the.month, count,key=paste(day.of.the.month, location), color = location, tooltip=paste(
                   count, "cyclists counted at",
                   location, "on",
                   date)
                 ),
  showSelected = c("location", "month"),
  clickSelects = "location",
  size = 5,
  data = data.table(velos.dt, facet = counter.title)
)+
  scale_color_manual(values=location.colors)+
  scale_fill_manual(values=severity.colors, breaks=rev(severity.vec))+
  geom_text(aes(15, 24,               
                label=month),
                showSelected="month", size=15,
            data=data.table(months, facet=accidents.title))+
  scale_x_continuous("day of the month", breaks=c(1, 10, 20, 30))+
  geom_text(aes(day.of.the.month, count+500, color=location, label=location,       
                key=location               
                ),                
                clickSelects="location",
                showSelected=c("location", "month"),
            data=data.table(day.labels, facet=counter.title))+
  geom_point(aes(day.of.the.month, accident.i,
                 key=paste(date.str, accident.i),
                 
                 tooltip=paste(
                      ifelse(deaths==0, "",
                             ifelse(deaths==1,
                                    "1 death,",
                                    paste(deaths, "deaths,"))),
                      ifelse(people.severely.injured==0, "",
                             ifelse(people.severely.injured==1,
                                    "1 person severely injured,",
                                    paste(people.severely.injured,
                                          "people severely injured,"))),
                      people.slightly.injured,
                      "people slightly injured at",
                      ifelse(is.na(street.number), "", street.number),
                      street, "/", cross.street,
                      date.str, time.str),
                 fill=severity), showSelected="month",
             size=4,
             chunk_vars=c("month"),
             data=data.table(accidents.cumsum, facet=accidents.title))

viz <-
  list(
    source = "https://github.com/suhaani-agarwal/montreal-velos/blob/master/figure-timeseries.R",
    MonthFacet=MonthFacet,
    DaysFacet=DaysFacet,
    summary=MonthSummary,
    map=mtl.map,
    selector.types=list(location="multiple", severity="multiple"),
    duration=list(month=2000),
    first=list(
      location=c("Berri", "Rachel"),
      month="September 2012"),
    time=list(variable="month", ms=5000),
    title="Montreal cyclists and accidents, 2009-2014")

animint2pages(viz, "montreal-bikes-Jan2016")

