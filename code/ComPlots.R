# ----------- Descriptive plots ---------------- #

library(pineR)
library(reshape2)
library(readxl)
library(geofacet)
library(ggplot2)
library(ggh4x)
library(dplyr)
library(tibble)

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
readWeightedData

names(readWeightedData)[1] <- "Ballinagee"
names(readWeightedData)[17] <- "Hortland"
names(readWeightedData)[19] <- "Killurney"


# Read gridded data -------------------------------------------------------

base_dir_cli <- 'data/climate'
listGridData <- list.files(base_dir_cli, pattern = "*.Rda", full.names = TRUE)
readGridData <- lapply(listGridData, loadRData)
names(readGridData) <- gsub("CD.Rda", "", basename(listGridData))


# organize Ballyroan 1
readGridData$Ballyroan1 <- readGridData$Ballyroan1 %>% select(-c(`eobsmax$temp`))
load("data/climate/Ballyroan1_1999.Rda")
readGridData$Ballyroan1 <- readGridData$Ballyroan1[, names(Ballyroan1_1999)]
readGridData$Ballyroan1[1:365, ] <- Ballyroan1_1999[1:365,]

readGridData <- readGridData[-4]

readGridData$Ballyroan1[366:370, ]
Ballyroan1_1999[1:365,]
readGridData$Ballyroan1[, names(Ballyroan1_1999)]

# Create one data frame ---------------------------------------------------

allData <- lapply(1:length(readGridData), function(i) {
  tibble::add_column(readGridData[[i]], tempW = readWeightedData[[i]]$temp)
})

names(allData) <- names(readGridData)

allData <- plyr::ldply(allData) %>%
  plyr::rename(c('.id'='Site')) %>%
  mutate(diff = temp - tempW)


# Plots -------------------------------------------------------------------


# Just weighted data

allData %>% filter(!(Site == "Corrakyle") & !(Site == "Corracloon")) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = tempW), color = "firebrick", size = 0.4) +
  theme_bw(base_size = 16) +
  labs(
    x = "Date",
    y = expression(paste("Temperature (", degree, "C)"))
  ) +
  facet_wrap(vars(Site), scales = "free_x") +
  theme(strip.background = element_blank(),
        panel.spacing.x = unit(6,"mm"),
        legend.position = c(0.75, 0.05))#,

# Just two sites (weighted data)

hort <- allData %>% filter(Site == "Hortland") |> select(Site, temp, tempW, date)
kild <- allData %>% filter(Site == "Kilduff") |> select(Site, temp, tempW, date)
hort_kild <- data.frame(Hortland = hort$tempW, Kilduff = kild$tempW, date = hort$date)

hort_kild |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = Hortland, color = "Hortland")) +
  geom_line(aes(y = Kilduff, color = "Kilduff"), size = 0.15) +
  theme_bw(base_size = 16) +
  scale_color_manual(
    name = "Site",
    values = c("steelblue", "firebrick"),
    labels = c("Hortland", "Kilduff")
  ) +
  labs(
    x = "Date",
    y = expression(paste("Temperature (", degree, "C)"))
  )



# Just the co-locations

co_locations <- c('Ballymacshaneboy', 'Summerhill', 'Kilduff', 'Cloondara',
                  'Clonoghil', 'Tigroney', 'Gurtnapisha', 'Cashelduff')
co_locations_df <- allData %>% filter(Site %in% co_locations) |> select(Site, temp, tempW, date)

co_locations_df |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = tempW), color = "firebrick", size = 0.4) +
  theme_bw(base_size = 16) +
  labs(
    x = "Date",
    y = expression(paste("Temperature (", degree, "C)"))
  ) +
  facet_wrap(vars(Site), scales = "free_x", ncol = 4) +
  theme(strip.background = element_blank(),
        panel.spacing.x = unit(5,"mm"))


# Just the co-locations against climate

co_locations <- c('Ballymacshaneboy', 'Summerhill', 'Kilduff', 'Cloondara',
                  'Clonoghil', 'Tigroney', 'Gurtnapisha', 'Cashelduff')
co_locations_df <- allData %>% filter(Site %in% co_locations) |> select(Site, temp, tempW, date)

co_locations_df |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = temp, color = "temp")) +
  geom_line(aes(y = tempW, color = "tempW"), size = 0.15) +
  theme_bw(base_size = 16) +
  scale_color_manual(
    name = "Data",
    values = c("steelblue", "firebrick"),
    labels = c("Gridded", "Weighted")
  ) +
  labs(
    x = "Date",
    y = expression(paste("Temperature (", degree, "C)"))
  ) +
  facet_wrap(vars(Site), scales = "free_x", ncol = 4) +
  theme(strip.background = element_blank(),
        panel.spacing.x = unit(5,"mm"))


# Just climate data

allData %>% filter(!(Site == "Corrakyle") & !(Site == "Corracloon")) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = temp), color = "steelblue", size = 0.4) +
  theme_bw(base_size = 16) +
  labs(
    x = "Date",
    y = expression(paste("Temperature (", degree, "C)"))
  ) +
  facet_wrap(vars(Site), scales = "free_x") +
  theme(strip.background = element_blank(),
        panel.spacing.x = unit(6,"mm"),
        legend.position = c(0.75, 0.05))#,



# Aspects
east <- c("Hortland", "Rickardstown", "Ballybrittas")
south <- c("Ballinagee", "Glendine1" , "Glendine2" , "Kilduff"   , "Ballyroan1", "Ballyroan2", "Kilurney"  , "Clonoghil" , "Tigroney"  , "Cashelduff", "Woodford")
west <- c("Oakwood" , "Summerhill"  , "Rossnagad"   , "Cloondara"   , "Knockaville" , "Doon"        , "Longfordpass")
north <- c("Lackenrea1","Lackenrea2","Deerpark"  ,   "BallymacshaneboDonadea",     "Gurtnapisha")


# By aspect

# east
allData %>% filter(Site %in% east) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = temp, color = "temp")) +
  geom_line(aes(y = tempW, color = "tempW"), size = 0.15) +
  theme_bw(base_size = 16) +
  scale_color_manual(
    name = "Data",
    values = c("steelblue", "firebrick"),
    labels = c("Gridded", "Weighted")
  ) +
  labs(
    x = "Date",
    y = expression(paste("Temperature (", degree, "C)"))
  ) +
  facet_wrap(vars(Site), scales = "free_x") +
  theme(strip.background = element_blank(),
        panel.spacing.x = unit(5,"mm"))
#ggsave("Figures/Aspect/east2.pdf")

# south
allData %>% filter(Site %in% south) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = temp, color = "temp")) +
  geom_line(aes(y = tempW, color = "tempW"), size = 0.15) +
  theme_bw(base_size = 16) +
  scale_color_manual(
    name = "Data",
    values = c("steelblue", "firebrick"),
    labels = c("Gridded", "Weighted")
  ) +
  labs(
    x = "Date",
    y = expression(paste("Temperature (", degree, "C)"))
  ) +
  facet_wrap(vars(Site), scales = "free_x") +
  theme(strip.background = element_blank(),
        panel.spacing.x = unit(5,"mm"))
#ggsave("Figures/Aspect/south2.pdf")

# west
allData %>% filter(Site %in% west) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = temp, color = "temp")) +
  geom_line(aes(y = tempW, color = "tempW"), size = 0.15) +
  theme_bw(base_size = 16) +
  scale_color_manual(
    name = "Data",
    values = c("steelblue", "firebrick"),
    labels = c("Gridded", "Weighted")
  ) +
  labs(
    x = "Date",
    y = expression(paste("Temperature (", degree, "C)"))
  ) +
  facet_wrap(vars(Site), scales = "free_x") +
  theme(strip.background = element_blank(),
        panel.spacing.x = unit(5,"mm"))
#ggsave("Figures/Aspect/west2.pdf")

# north
allData %>% filter(Site %in% north) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = temp, color = "temp")) +
  geom_line(aes(y = tempW, color = "tempW"), size = 0.15) +
  theme_bw(base_size = 16) +
  scale_color_manual(
    name = "Data",
    values = c("steelblue", "firebrick"),
    labels = c("Gridded", "Weighted")
  ) +
  labs(
    x = "Date",
    y = expression(paste("Temperature (", degree, "C)"))
  ) +
  facet_wrap(vars(Site), scales = "free_x") +
  theme(strip.background = element_blank(),
        panel.spacing.x = unit(5,"mm"))
#ggsave("Figures/Aspect/north2.pdf")

# all sites

allData %>% filter(!(Site == "Corrakyle") & !(Site == "Corracloon")) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = temp, color = "temp")) +
  geom_line(aes(y = tempW, color = "tempW"), size = 0.15) +
  theme_bw(base_size = 16) +
  scale_color_manual(
    name = "Data",
    values = c("steelblue", "firebrick"),
    labels = c("Gridded", "Weighted")
  ) +
  labs(
    x = "Date",
    y = expression(paste("Temperature (", degree, "C)"))
  ) +
  facet_wrap(vars(Site), scales = "free_x") +
  theme(strip.background = element_blank(),
        panel.spacing.x = unit(6,"mm"),
        legend.position = c(0.75, 0.05))#,
#axis.text = element_text(face="bold"))



## each site

listPlots <- lapply(unique(allData$Site), function (i){
  allData %>% filter(Site == i) %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = temp, color = "temp")) +
    geom_line(aes(y = tempW, color = "tempW"), size = 0.3) +
    theme_bw(base_size = 16) +
    scale_color_manual(
      name = "Data",
      values = c("steelblue", "firebrick"),
      labels = c("Gridded", "Weighted")
    )+
    labs(
      x = "Date",
      y = expression(paste("Temperature (", degree, "C)"))
    )
})

names(listPlots) <-  names(readGridData)

# lapply(1:29, function(i){
#   name <- paste0("figures/each site/", names(listPlots)[i], ".pdf")
#   ggsave(filename = name, plot = listPlots[[i]] )
# })


# listPlots <- lapply(unique(allData$Site), function (i){
#   allData %>% filter(Site == i) %>%
#     ggplot(aes(x = date)) +
#     geom_point(aes(y = temp, color = "temp")) +
#     geom_point(aes(y = tempW, color = "tempW"), size = 0.8) +
#     theme_bw(base_size = 16) +
#     scale_color_manual(
#       name = "Data",
#       values = c("steelblue", "firebrick"),
#       labels = c("Gridded", "Weighted")
#     )+
#     labs(
#       x = "Date",
#       y = expression(paste("Temperature (", degree, "C)"))
#     )
# })
#
# listPlots[[2]]


# PLot difference ---------------------------------------------------------


# east
allData %>% filter(Site %in% east) %>%
  ggplot(aes(x = date, y = diff)) +
  geom_line(colour = "steelblue") +
  #geom_line(aes(y = tempW, color = "tempW"), linetype = "dotted", size = 1) +
  theme_bw(base_size = 16) +
  labs(
    x = "Date",
    y = expression(paste("Temperature difference (", degree, "C)"))
  ) +
  facet_wrap(vars(Site), scales = "free_x") +
  theme(strip.background = element_blank(),
        panel.spacing.x = unit(6,"mm"))
#ggsave("Figures/Aspect/east.pdf")

# south
allData %>% filter(Site %in% south) %>%
  ggplot(aes(x = date, y = diff)) +
  geom_line(colour = "steelblue") +
  #geom_line(aes(y = tempW, color = "tempW"), linetype = "dotted", size = 1) +
  theme_bw(base_size = 16) +
  labs(
    x = "Date",
    y = expression(paste("Temperature difference (", degree, "C)"))
  ) +
  facet_wrap(vars(Site), scales = "free_x") +
  theme(strip.background = element_blank(),
        panel.spacing.x = unit(6,"mm"))
#ggsave("Figures/Aspect/south.pdf")

# west
allData %>% filter(Site %in% west) %>%
  ggplot(aes(x = date, y = diff)) +
  geom_line(colour = "steelblue") +
  #geom_line(aes(y = tempW, color = "tempW"), linetype = "dotted", size = 1) +
  theme_bw(base_size = 16) +
  labs(
    x = "Date",
    y = expression(paste("Temperature difference (", degree, "C)"))
  ) +
  facet_wrap(vars(Site), scales = "free_x") +
  theme(strip.background = element_blank(),
        panel.spacing.x = unit(6,"mm"))
#ggsave("Figures/Aspect/west.pdf")

# north
allData %>% filter(Site %in% north) %>%
  ggplot(aes(x = date, y = diff)) +
  geom_line(colour = "steelblue") +
  #geom_line(aes(y = tempW, color = "tempW"), linetype = "dotted", size = 1) +
  theme_bw(base_size = 16) +
  labs(
    x = "Date",
    y = expression(paste("Temperature difference (", degree, "C)"))
  ) +
  facet_wrap(vars(Site), scales = "free_x") +
  theme(strip.background = element_blank(),
        panel.spacing.x = unit(6,"mm"))
#ggsave("Figures/Aspect/north.pdf")


# all sites
allData %>%  filter(!(Site == "Corrakyle") & !(Site == "Corracloon")) %>%
  ggplot(aes(x = date, y = diff)) +
  geom_line(colour = "steelblue") +
  #geom_line(aes(y = tempW, color = "tempW"), linetype = "dotted", size = 1) +
  theme_bw(base_size = 14) +
  labs(
    x = "Date",
    y = expression(paste("Temperature difference (", degree, "C)"))
  ) +
  facet_wrap(vars(Site), scales = "free_x") +
  theme(strip.background = element_blank(),
        panel.spacing.x = unit(6,"mm"))


listPlotsDiff <- lapply(unique(allData$Site), function (i){
  allData %>% filter(Site == i) %>%
    ggplot(aes(x = date, y = diff)) +
    geom_line(colour = "steelblue") +
    #geom_line(aes(y = tempW, color = "tempW"), linetype = "dotted", size = 1) +
    theme_bw(base_size = 14) +
    labs(
      x = "Date",
      y = expression(paste("Temperature difference (", degree, "C)"))
    )
})

names(listPlotsDiff) <-  names(readGridData)

# lapply(1:29, function(i){
#   name <- paste0("figures/each site diff/", names(listPlotsDiff)[i], ".pdf")
#   ggsave(filename = name, plot = listPlotsDiff[[i]])
# })



# Test --------------------------------------------------------------------

allData %>% filter(Site == "Ballyroan1") %>%
  ggplot(aes(x = date, y = diff)) +
  geom_line(colour = "steelblue") +
  #geom_line(aes(y = tempW, color = "tempW"), linetype = "dotted", size = 1) +
  theme_bw(base_size = 14) +
  labs(
    x = "Date",
    y = expression(paste("Temperature difference (", degree, "C)"))
  )

allData %>% filter(Site == "Ballyroan1") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = temp, color = "temp")) +
  geom_line(aes(y = tempW, color = "tempW"), size = 0.3) +
  theme_bw(base_size = 16) +
  scale_color_manual(
    name = "Data",
    values = c("steelblue", "firebrick"),
    labels = c("Gridded", "Weighted")
  )+
  labs(
    x = "Date",
    y = expression(paste("Temperature (", degree, "C)"))
  )


## teste balyyroan 1
bb <- data.frame(readWeightedData$Ballyroan1[1:365,], date = Ballyroan1_1999$date)
readWeightedData$Ballyroan1[366:370,]
ggplot() +
  geom_line(data = Ballyroan1_1999, aes(x = date, y = temp, color = "steelblue")) +
  geom_line(data = bb, aes(x =date,  y = temp, color = "firebrick"), size = 0.3) +
  theme_bw(base_size = 16) +
  labs(
    x = "Date",
    y = expression(paste("Temperature (", degree, "C)"))
  )






