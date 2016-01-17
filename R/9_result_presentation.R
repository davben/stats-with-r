
# map of Germany ----------------------------------------------------------

library(rgdal)
library(maptools)
src <- "http://sg.geodatenzentrum.de/web_download/vg/vg1000-ew_3112/utm32s/shape/vg1000-ew_3112.utm32s.shape.ebenen.zip"
lcl <- "data/germany_shape.zip"

if (!file.exists(lcl)) {
  download.file(src, lcl)
}
unzip(lcl, exdir = "data/germany_shape/")

germany <- readOGR(dsn = "data/germany_shape/vg1000-ew_3112.utm32s.shape.ebenen/vg1000-ew_ebenen", 
                   layer = "VG1000_KRS")
germany <- spTransform(germany, CRS("+proj=longlat +ellps=GRS80 +datum=WGS84 +no_defs"))
# save(germany, file = "./data/germany.Rdata")


# load("./data/germany.Rdata")

library(broom)
germany_df <- tidy(germany, region="RS")

ggplot(germany_df) +
  geom_map(map = germany_df, aes(long, lat, map_id = id), fill="#f0f0f0", colour = "black")


ggplot(germany_df) +
  geom_map(map = germany_df, aes(long, lat, map_id = id), fill="#f0f0f0", colour = "black") +
  coord_map() # mercator projection (others available!)


## data to plot on map:
event_data <- read.csv("./data/event_data.csv", stringsAsFactors = FALSE)

ggplot(germany_df) +
  geom_map(map = germany_df, aes(long, lat, map_id = id), 
           fill="#f0f0f0", colour = "black") +
  geom_point(data=event_data, aes(lon, lat, colour=kategorie)) +
  coord_map()



# lacina world plot -------------------------------------------------------
world <- readOGR(dsn = "data/TM_WORLD_BORDERS-0", layer = "TM_WORLD_BORDERS-0.3")

# save(world, file = "./data/world.Rdata")
# load(file = "./data/world.Rdata")

world_df <- tidy(world, region="ISO3")

ggplot(world_df) +
  geom_map(map = world_df, aes(long, lat, map_id = id)) +
  coord_map()

## get lacina data:
lacina <- read.csv("./data/lacina/lacina.csv")
lacina$cname <- sub("RUM", "ROM", lacina$cname)
lacina$cname <- sub("PAk", "PAK", lacina$cname)

### summarise: sum of deaths per 1000 inhabitants of each country
deaths <- lacina %>%
  group_by(cname) %>%
  summarise(deaths = sum(battledeadbest, na.rm = TRUE),
            population = mean(pop)) %>%
  mutate(deaths_per_1000 = deaths/population * 1000,
         country = countrycode(cname, "cowc", "iso3c"))

world_df <- world_df %>%
  left_join(deaths, c("id" = "country"))


lacina_world <- ggplot(world_df) +
  geom_map(map = world_df, aes(long, lat, fill = deaths_per_1000, map_id = id)) +
  coord_map() +
  theme_map()
print(lacina_world)

# saving plots for use in documents!
ggsave(filename = "./fig/lacina_world.pdf", lacina_world)


# Now practice: Map GDP p.c. from exercise sheet!

# regression output -------------------------------------------------------

buhaug <- read.csv("./data/aufgabenblatt/buhaug_gates.csv", stringsAsFactors = FALSE)

model_1 <- lm(location ~ ln_abs_scope + ln_land_area + identity + incompatibility,
              data = buhaug)

model_2 <- lm(location ~ rel_scope + ln_land_area + identity + incompatibility,
              data = buhaug)

model_3 <- lm(ln_abs_scope ~ location + ln_land_area + duration + border + resource,
              data = buhaug)

model_4 <- lm(ln_abs_scope ~ location + ln_land_area + duration + border + resource +
                mountain + forest,
              data = buhaug)

model_5 <- lm(rel_scope ~ location + ln_land_area + duration + border + resource,
              data = buhaug)

model_6 <- lm(rel_scope ~ location + ln_land_area + duration + border + resource +
                mountain + forest,
              data = buhaug)


# tables
library(stargazer)
stargazer(list(model_1, model_2), style="apsr")

##  write to html-file (helpful for use with MS Word)
stargazer(list(model_3, model_4, model_5, model_6), type="html", out = "./table_4.html")



# plots
library(dotwhisker)

dwplot(list(model_1, model_2),  alpha=0.05) +
  geom_vline(xintercept = 0) +
  scale_color_discrete(name="") +
  theme_bw()


small_multiple(list(model_1, model_2))


### more advanced:
## Using submodels to compare results across different samples
# Generate a tidy data.frame of regression results from five models on
# the mtcars data subset by transmission type (am)
ordered_vars <- c("wt", "cyl", "disp", "hp", "gear")
mod <- "mpg ~ wt"
by_trans <- mtcars %>% group_by(am) %>%  # group data by transmission
  do(tidy(lm(mod, data = .))) %>%        # run model on each group
  rename(submodel = am) %>%              # make submodel variable
  mutate(model = "Model 1")              # make model variable

for (i in 2:5) {
  mod <- paste(mod, "+", ordered_vars[i])
  by_trans <- rbind(by_trans, mtcars %>% group_by(am) %>%
                      do(tidy(lm(mod, data = .))) %>%
                      rename(submodel = am) %>%
                      mutate(model = paste("Model", i)))
}

small_multiple(by_trans) +
  theme_bw() + ylab("Coefficient Estimate") +
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1),
        legend.position=c(0, 0), legend.justification=c(0, 0),
        legend.title = element_text(size=9),
        legend.background = element_rect(color="gray90"),
        legend.margin = unit(-3, "pt"),
        legend.key.size = unit(10, "pt")) +
  scale_colour_hue(name = "Transmission",
                   breaks = c(0, 1),
                   labels = c("Automatic", "Manual"))

