# Descriptive plots
library(lubridate)
library(dplyr)
library(readxl)
library(ggplot2)
library(MetBrewer)
library(googledrive)
library(gt)


# Function to load data ---------------------------------------------------

loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}


# Read weighted data ------------------------------------------------------

base_dir_wei <- 'data/weighted'
listWeightedData <- list.files(base_dir_wei, pattern = "*.Rda", full.names = TRUE)
readWeightedData <- lapply(listWeightedData, loadRData)
names(readWeightedData) <- gsub("Data.Rda", "", basename(listWeightedData))
readWeightedData[[1]]

names(readWeightedData)[1] <- "Ballinagee"
names(readWeightedData)[17] <- "Hortland"
names(readWeightedData)[19] <- "Killurney"


WeightedData <-  reshape2::melt(readWeightedData, id = c("day", "month", "year", "min_temp", "max_temp", "temp"))
WeightedData <- WeightedData |> transform(year = as.factor(year),
                                            month = as.factor(month))

# Read summary table

# downloadData(name = "summaryData",
#              link = "https://docs.google.com/spreadsheets/d/1Xekodvbzysn-5iy82gzK_OlbkrfyN3aooBk9YsWoXq0/edit#gid=0",
#              path = "Data/",
#              type = "xlsx", gdrive = TRUE)



summaryData <- read_excel("data/summaryData.xlsx")
summaryData <- summaryData %>% dplyr::select(c(1:16) ) %>%
  dplyr::rename("Site ID" = "Site",
                "Site" = "Trial site emergence",
                "Slope" = "Slope %",
                "Soil Type" = "Soil",
                "Soil Conditions" = "Conditions") %>%
  dplyr::filter(!(Site == "Ballybrittas" & `Weather Station` == "Durrow"))

summaryTable <- summaryData %>% gt() %>%
  tab_style(
    style = list(
      cell_fill(color = 'steelblue')
    ),
    locations = cells_body(
      #columns = vars(V1, V2), # not needed if coloring all columns
      rows = Distance <= 10)
  )

# Select weather stations that the distance is smaller than 10 km (use them alone)
coClimate <- summaryData %>% filter(Distance <= 10)
coClimate %>% gt()
coClimate$Site

# Box plot

WeightedData %>%
  ggplot(aes(y=L1, x=temp)) +
  labs(x = "Weighted mean temperature", y = "Site")+
  geom_boxplot(fill = 'steelblue' , color = "black") +
  xlim(-10, 30) +
  theme_bw(base_size = 14)
# theme_few() +
# scale_fill_few()

#ggsave("Plots/boxplotW.png")


# WeightedDataTidy <- WeightedData %>%
#   group_by(L1) %>%
#   mutate(dayYear = 1:length(temp))
#
# WeightedDataTidy %>%
#   ggplot(aes(x = dayYear, y = temp)) +
#   #geom_smooth()+
#   geom_line(color = "gray40", alpha = 0.8, size = 0.2) +
#   theme_bw(base_size = 14) +
#   theme(strip.background =element_rect(fill="white")) +
#   # theme_few() +
#   # scale_fill_few() +
#   scale_colour_brewer(palette="Set1") +
#   labs(x = "Day of the year", y = expression(paste("Temperature (",degree,"C)"))) +
#   geom_rect(data = subset(WeightedDataTidy, L1 %in% coClimate$Site), fill = NA,
#             colour = "red", xmin = -Inf, xmax = Inf,
#             ymin = -Inf, ymax = Inf, size = 1.2) +
#   facet_wrap(~L1)

# Plots for each covariate

# Histograms

AltitudeHist <- summaryData %>% ggplot(aes(Altitude)) +
  geom_histogram(bins = 6, color = "black", fill='steelblue') +
  theme_bw(base_size = 14) + labs(y = "Frequency")
# theme_few() +
# scale_fill_few()

DistanceHist <- summaryData %>% ggplot(aes(Distance)) +
  geom_histogram(bins = 6, color = "black", fill='steelblue') +
  theme_bw(base_size = 14) + labs(y = "Frequency") +
  xlab("Distance (km)")
# theme_few() +
# scale_fill_few()

gridExtra::grid.arrange(AltitudeHist, DistanceHist, nrow = 1)
#ggsave("Plots/histAltDist.pdf")


SlopeHist <- summaryData %>%  na.omit(Slope) %>%
  ggplot(aes(Slope)) +
  geom_histogram(bins = 6, color = "black", fill='steelblue') +
  theme_bw(base_size = 14) +
  # theme_few() +
  # scale_fill_few() +
  labs(y = "Frequency")

SlopeAngleHist <- summaryData %>%
  ggplot(aes(`Slope Angle`)) +
  geom_histogram(bins = 6, color = "black", fill='steelblue') +
  theme_bw(base_size = 14) +
  # theme_few() +
  # scale_fill_few() +
  labs(y = "Frequency", x = expression(paste("Slope (",degree, ")")))


# Covariate vs Site

SlopeAngleSite <- summaryData %>%  group_by(Site) %>%
  ggplot(aes(x = Slope , y = reorder(Site,  Slope))) +
  geom_point(colour = 'steelblue', fill = "black", size = 2) +
  geom_segment(aes(x = 0, xend=Slope, y=Site, yend=Site), color='steelblue') +
  labs(x = expression(paste("Slope (", degree, ")")), y = "Site") +
  theme_bw(base_size = 14)
# theme_few() +
# scale_fill_few()


# SlopePercSite <- summaryData %>%  group_by(Site) %>%
#   ggplot(aes(x = Slope, y = reorder(Site,  Slope))) +
#   geom_point(colour = 'steelblue', fill = "black", size = 4) +
#   geom_segment(aes(x = 0, xend=Slope, y=Site, yend=Site), color='steelblue') +
#   labs(x = "Slope (%)", y = "Site") +
#   theme_bw(base_size = 20)


AltitudePercSite <- summaryData %>%  group_by(Site) %>%
  ggplot(aes(x = Altitude, y = reorder(Site,  Altitude))) +
  geom_point(colour = 'steelblue', fill = "black", size = 4) +
  geom_segment(aes(x = 0, xend=Altitude, y=Site, yend=Site), color='steelblue') +
  labs(x = "Altitude", y = " ") +
  theme_bw(base_size = 14)

slAl <- gridExtra::grid.arrange(SlopeAngleSite, AltitudePercSite, nrow = 1)
#ggsave("Plots/SlopeAltSite.png")


## Aspect

AspectFreq <- summaryData %>% group_by(Aspect) %>%
  dplyr::summarise(Frequency = n()) %>% ggplot(aes(y=reorder(Aspect, - Frequency), x = Frequency)) +
  geom_bar(stat="identity", fill = 'steelblue', colour = "black") +
  theme_bw(base_size = 14)+
  ylab("Aspect")
#ggsave("Plots/AspectFreq.pdf")


# SlopeTypeSite <- summaryData %>%
#   group_by(Site) %>%
#   filter(`Slope Type`!="N/A") %>%
#   ggplot(aes(x = `Slope Type`, y = reorder(Site,  `Slope Type`))) +
#   geom_point(colour = 'steelblue', fill = "black", size = 3) +
#   #geom_segment(aes(x = 0, xend=`Slope Type`, y=Site, yend=Site), color='steelblue') +
#   labs(x = "Slope Type", y = "Site") +
#   theme_bw(base_size = 14)
#   # theme_few() +
#   # scale_fill_few()

# SpeciesSite <- summaryData %>%  group_by(Site) %>%
#   ggplot(aes(x = Species, y = reorder(Site,  Species))) +
#   geom_point(colour = 'steelblue', fill = "black", size = 3) +
#   #geom_segment(aes(x = 0, xend= Species, y=Site, yend=Site), color='steelblue') +
#   labs(x = "Species", y = "Site") +
#   theme_bw(base_size = 14)


# Plot all categorical variables

summaryDataSelect <- summaryData %>%
  select(Site, Species, `Soil Type`, `Soil Conditions`, Roughness)  %>%
  mutate_if(is.character,as.factor)
summaryDataSelect <-  summaryDataSelect %>% as.data.frame()
df <- reshape::melt(summaryDataSelect, id = "Site")

CategSite <- ggplot(df, aes(value, Site)) +
  geom_point(size = 4, col = 'steelblue') +
  facet_grid(~ variable, scale="free", space="free_x") +
  xlab(" ") +
  theme_bw(base_size = 20) +
  theme(strip.background =element_rect(fill="white"))
# theme_few() +
# scale_fill_few()

CategSite
#ggsave("Plots/CategSite.png")

# plot aspect

summaryDataSelect <- summaryData %>%
  select(Site, Aspect)  %>%
  mutate_if(is.character,as.factor)
summaryDataSelect <-  summaryDataSelect %>% as.data.frame()
df <- reshape::melt(summaryDataSelect, id = "Site")

aspec <- ggplot(df, aes(value, Site)) +
  geom_point(size = 4, col = 'steelblue') +
  #facet_grid(~ variable, scale="free", space="free_x") +
  xlab("Aspect") +
  theme_bw(base_size = 18) +
  theme(strip.background =element_rect(fill="white"))
# theme_few() +
# scale_fill_few()

aspec
#ggsave("Plots/aspect.png")


# plot slope and altitude

summaryDataSelect <- summaryData %>%
  select(Site, Slope, Altitude)  %>%
  mutate_if(is.character,as.factor)
summaryDataSelect <-  summaryDataSelect %>% as.data.frame()
df <- reshape::melt(summaryDataSelect, id = "Site")

slopep <- ggplot(df, aes(value, Site)) +
  geom_point(size = 4, col = 'steelblue') +
  #facet_grid(~ variable, scale="free", space="free_x") +
  xlab("Aspect") +
  theme_bw(base_size = 18) +
  theme(strip.background =element_rect(fill="white"))
# theme_few() +
# scale_fill_few()

ggplot(df, aes(x = value, y = Site,  value)) +
  geom_point(colour = 'steelblue', fill = "black", size = 2) +
  geom_segment(aes(x = 0, xend=value, y=Site, yend=Site), color='steelblue') +
  facet_grid(~variable, scale="free", space="free_x") +
  #labs(x = "Slope (%)", y = "Site") +
  theme_bw(base_size = 14) +
  theme(strip.background =element_rect(fill="white"))

aspec
#ggsave("Plots/aspect.png")





# Structure data ----------------------------------------------------------

# popStrucData <- read_excel("/Users/alessalemos/Downloads/popStruc.xlsx", range = cell_cols("A:M"))
# save(popStrucData, file = "Data/popStrucData.Rda")
# altTypeData <- read_excel("/Users/alessalemos/Downloads/altitudeallsites.xlsx", range = cell_cols("A:C"))
# save(altTypeData, file = "Data/altTypeData.Rda")


load("data/popStrucData.Rda")
load("data/altTypeData.Rda")
popStrucData %>% names()
popStrucData$site_name <- tools::toTitleCase(popStrucData$site_name)

# summaryData$Site %>% unique()
# popStrucData$site_name <- c("Ballinagee", "Oakwood", "Glendine", "Lackenrea 1", "Lackenrea 2",
#                             "Summerhill", "Deerpark", "Ballymacshaneboy", "Kilduff", "Rossnagad",
#                             "Ballyroan 1", "Ballyroan 2", "Ballybrittas", "Donadea" ,
#                             "Glendine trial", "Cloondara", "Knockaville" , "Killurney",
# )


totalW <- popStrucData %>% ggplot(aes(y = reorder(site_name, - total_weevils), x = total_weevils)) +
  geom_point(colour = 'steelblue', fill = "black", size = 2) +
  geom_segment(aes(x = 0, xend=total_weevils, y=site_name, yend=site_name), color='steelblue') +
  labs(x = "Total weevils", y = "Site") +
  theme_bw(base_size = 14)

totalW

dfStage <- data.frame(site = rep(popStrucData$site_name, 3),
                      stage = c(rep("Larvae", length(popStrucData$site_name)),
                                rep("Pupae", length(popStrucData$site_name)),
                                rep("Adults", length(popStrucData$site_name))),
                      values = c(popStrucData$larvae_3weeks_before_emergence,
                                 popStrucData$pupae_3weeks_before_emergence,
                                 popStrucData$adults_3weeks_before_emergence))
dfStage$stage <- factor(dfStage$stage, levels=unique(dfStage$stage))
levels(dfStage$stage) <- c("L", "P", "A")

dfStage |> filter(site == "Knockeen")
dfStage <- dfStage |> group_by(site) |> mutate(summ = sum(values),
                                               perc = (values/summ)*100) |>
  filter(!(site %in% c("Knockeen", "Corracloon", "Corrakyle")))

dfStage %>% ggplot(aes(y = perc, x = stage, group = site)) +
  geom_line(colour = "steelblue") +
  geom_point(colour = "steelblue") +
  facet_wrap(~site) +
  theme_bw(base_size = 16) +
  theme(strip.background =element_rect(fill="white")) +
  labs(y = "Population percentage (%)", x = "Stage") +
  ylim(c(0,100))


# including gone

# G means gone
dfStageG <- data.frame(site = rep(popStrucData$site_name, 4),
                       stage = c(rep("L", length(popStrucData$site_name)),
                                 rep("P", length(popStrucData$site_name)),
                                 rep("A", length(popStrucData$site_name)),
                                 rep("G", length(popStrucData$site_name))),
                       values = c(popStrucData$larvae_3weeks_before_emergence,
                                  popStrucData$pupae_3weeks_before_emergence,
                                  popStrucData$adults_3weeks_before_emergence,
                                  popStrucData$gone_emerged_3weeks_before_emergence))
dfStageG$stage <- factor(dfStageG$stage, levels=unique(dfStageG$stage))

dfStageG %>% ggplot(aes(y = values, x = stage, group = site)) +
  geom_line(colour = "steelblue") +
  geom_point(colour = "steelblue") +
  facet_wrap(~site) +
  theme_bw(base_size = 14) +
  theme(strip.background =element_rect(fill="white")) +
  labs(y = "Population number", x = "Stage")


# altitude vs site near station

# create new dataframe
newDF <- data.frame(
  Site = altTypeData[1:36, 1:2],
  weatherStation = altTypeData[37:72, 2]
)

newDF <- newDF %>% filter(!Site.site %in% c("Emo", "Killnaconnigan", "Knockeen", "The Rodneys",
                                            "Annalecka", "glendalough"))

# set the names of the columns
names(newDF) <- c("Site", "Site.Alt", "Weather.Station.Alt")

# create new column that assings a colour of the segment based on whether the
# site altitude is higher than the weather station (or the opposite)
newDF <- newDF %>%
  mutate(myColor = ifelse(Weather.Station.Alt > Site.Alt, "steelblue",  "firebrick"))

# create plot
ggplot(newDF, aes(x = reorder(Site, Site.Alt, max), y = Site.Alt)) +
  geom_segment(aes(xend = Site, y = Weather.Station.Alt, yend = Site.Alt), color = newDF$myColor) +
  geom_point(aes(color = "Site")) +
  geom_point(aes(x = Site, y = Weather.Station.Alt, color = "Weather.Station.Alt")) +
  scale_color_manual(values = c("Site" = 'firebrick','Weather.Station.Alt' = 'steelblue'),
                     labels = c("Site", "Weather Station")) +
  coord_flip() +
  xlab("") +
  ylab("Altitude") +
  labs(color = 'Altitudes') +
  theme_bw()




