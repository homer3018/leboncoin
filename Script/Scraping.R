library(httr)
library(rvest)
library(tidyverse)
library(rebus)
library(knitr)
library(tictoc)
library(osmdata)
library(sf)

#   ____________________________________________________________________________
#   Locals                                                                  ####

options(scipen = 999)

agents <- c("Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:62.0) Gecko/20100101 Firefox/62.0",
            "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15")
acc <- c("text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
         "/")
lang <- c("en-GB,en;q=0.9",
          "fr,fr-FR;q=0.8,en-US;q=0.5,en;q=0.3")

fra <- readRDS("gadm36_FRA_2_sf.rds")

#   ____________________________________________________________________________
#   Functions                                                               ####

go_GET <- function(url,
                   ag = sample(agents, 1),
                   ac = sample(acc, 1),
                   acl = sample(lang, 1)) {
  result <- GET(
    url,
    add_headers(
      "User-Agent" = ag,
      "Accept" = ac,
      "Accept-Language" = acl
    )
  )
  print(httr::http_status(result)$message)
  return(result)
}

ads_by_page <- function(page) {
  Sys.sleep(runif(1, 0, 5))
  my_html <- read_html(go_GET(page))
  links <- my_html %>%
    html_elements("div + a") %>%
    html_attr("href") %>%
    na.omit()
  tib <- tibble(urls = str_c("https://www.leboncoin.fr", links))
  return(tib)
}

ads_in_bm <- function(html) {
  links <- html %>%
    html_elements(".egdKTe") %>% 
    html_attr("href") %>%
    na.omit()
  tib <- tibble(urls = str_c("https://www.leboncoin.fr", links))
  return(tib)
}

ad_info <- function(ad) {
  Sys.sleep(runif(1, 0, 30))
  html_ad <- read_html(go_GET(ad))
  title <- html_ad %>%
    html_elements(".-HQxY") %>%
    html_text()
  criteria <-
    tibble(
      name = html_ad %>%
        html_nodes("._2k43C") %>%
        html_text(),
      value = html_ad %>%
        html_nodes("._3eNLO") %>%
        html_text() %>%
        .[1:length(name)]
    )
  f <- function(x) {
    if (length(x) == 0) {
      x <- NA
    }
    return(x)
  }
  year <- filter(criteria, name == "Année-modèle")$value %>% f()
  circ_date <- filter(criteria, name == "Mise en circulation")$value %>% f()
  km <- filter(criteria, str_detect(name, "Kilométrage"))$value %>%
    str_extract(one_or_more(DIGIT)) %>%
    f()
  spareparts <- filter(criteria, name == "Disponibilité des pièces détachées")$value %>% f()
  
  price <- html_ad %>%
    html_nodes(".Roh2X") %>%
    html_text() %>%
    first() %>%
    str_replace_all("[^0-9]", "") %>%
    as.numeric()
  description <- html_ad %>%
    html_nodes("._2BMZF") %>%
    html_text() %>%
    str_replace_all("\n", " ")
  publish <- html_ad %>%
    html_nodes(".Snj6Y._PypL.Dqdzf._3j0OU.cJtdT") %>%
    html_text()
  publish_date <- str_split(string = publish, pattern = " ", simplify = TRUE)[, 1]
  publish_time <- str_split(string = publish, pattern = " ", simplify = TRUE)[, 3]
  publish_dttm <- as.POSIXct(strptime(paste(publish_date, publish_time), format = "%d/%m/%Y %H:%M", tz = "Europe/Paris"))
  location <- html_ad %>%
    html_nodes("._64Mha._2NG-q.Dqdzf._3j0OU.cJtdT._1GcfX") %>%
    last() %>%
    html_text()
  
  coord_table <- osmdata::getbb(location)
  latitude <- (coord_table[2, 1] + coord_table[2, 2]) / 2
  longitude <- (coord_table[1, 1] + coord_table[1, 2]) / 2
  
  home_coord_table <- osmdata::getbb("Orléans 45000")
  home_latitude <- (home_coord_table[2, 1] + home_coord_table[2, 2]) / 2
  home_longitude <- (home_coord_table[1, 1] + home_coord_table[1, 2]) / 2
  home_coord <<- c(home_longitude, home_latitude)
  
  dist <- geosphere::distGeo(c(longitude, latitude), home_coord) / 1000
  
  tib_ad <- bind_cols(
    URL = ad,
    Title = title,
    Price = price,
    Year = year,
    "Mise en circulation" = circ_date,
    "Dispo. Pièves détachées" = spareparts,
    Kms = km,
    Description = description,
    Publication = publish_dttm,
    location = location,
    latitude = latitude,
    longitude = longitude,
    Distance = dist
  )
  return(tib_ad)
}

get_loc_region <- function(point) {
  intersec <- fra[st_intersects(point %>% st_set_crs(4326),
                                fra %>% st_set_crs(4326),
                                sparse = FALSE), ]
  return(intersec$NAME_1)
}

get_loc_dep <- function(point) {
  intersec <- fra[st_intersects(point %>% st_set_crs(4326),
                                fra %>% st_set_crs(4326),
                                sparse = FALSE), ]
  return(intersec$NAME_2)
}

#   ____________________________________________________________________________
#   Read bookmarks                                                          ####

html_base_bm <- read_html("bookmarks.html")

nb_links_bm <- html_base_bm %>%
  html_elements("[type=button]") %>%
  .[2] %>%
  html_text() %>%
  str_extract(one_or_more(DIGIT)) %>%
  as.numeric()

nb_pages_bm <- ceiling(nb_links_bm / 35)

pages_bm <- c(
  url_bm,
  str_c(url_bm, "&page=", 2:nb_pages)
)

url_bm <- ads_in_bm(html_base_bm)

df_bm <- df %>% 
  filter(URL %in% url_bm$urls)

#   ____________________________________________________________________________
#   Scraping                                                                ####

# go_GET("https://www.leboncoin.fr/recherche?category=4&text=capucine&price=min-25000")

url_base <- "https://www.leboncoin.fr/recherche?category=4&text=capucine&price=min-25000"
url_base_raw <- go_GET(url_base)
html_base <- read_html(url_base_raw)

nb_links <- html_base %>%
  html_elements("[type=button]") %>%
  .[2] %>%
  html_text() %>%
  str_extract(one_or_more(DIGIT)) %>%
  as.numeric()

nb_pages <- ceiling(nb_links / 35)

pages <- c(
  url_base,
  str_c(url_base, "&page=", 2:nb_pages)
)

# pages[1:5]

# ads_by_page(pages[1])

tic()
tib_ads_urls <- map(pages[1:2], safely(ads_by_page)) %>%
  map("result") %>%
  bind_rows()
toc()

# ad_info(tib_ads_urls$urls[1]) %>% kable()

tic()
new <- tib_ads_urls %>%
  filter(!urls %in% df$URL)

if (nrow(new) > 0) {
  print(paste0(nrow(new), " new ads to fetch."))
  tib_ads <- map(
    tib_ads_urls$urls[1:50],
    safely(ad_info)
  ) %>%
    map("result") %>%
    bind_rows()
  
  df <- df %>% 
    bind_rows(tib_ads %>% 
                mutate(Kms = Kms %>% as.numeric(),
                       Year = Year %>% as.numeric())) %>% 
    distinct()
} else {
  print("No new ads to fetch.")
}

toc()

#   ____________________________________________________________________________
#   Enrich dataset                                                          ####

mod <- loess(Price ~ Kms + Year, data = df)
df$pred <- predict(mod)

df <- df %>%
  mutate(Kms = Kms %>% as.numeric(),
         Year = Year %>% as.numeric(),
         bunk = str_detect(Description %>%
                             str_to_lower(), or("superpo", "jumeau")),
         solar_panel = str_detect(Description %>%
                                    str_to_lower(), or("solaire", "photovoltaïque")),
         hitch = str_detect(Description %>%
                              str_to_lower(), "attelage"),
         camera = str_detect(Description %>%
                               str_to_lower(), "recul"),
         hp = str_extract(Description %>%
                            str_to_lower(), one_or_more(DIGIT) %R% "cv"),
         twin_wheels = str_detect(Description %>%
                                    str_to_lower(), "jumel"),
         roof = str_detect(Description %>%
                             str_to_lower(), "relevable"),
         U_shape = str_detect(Description %>%
                                str_to_lower(), "en U"))

France <- fra %>%
  rename(region = NAME_0,
         dept_name = NAME_2,
         dept_num = CC_2) %>% 
  mutate(dept_num = as.numeric(dept_num)) %>% 
  select(region, dept_name, dept_num)

df <- df %>% select(-c(contains("region"),
                       contains("dept_name"),
                       contains("geometry")))

df <- df %>%
  mutate(diff = Price - pred) %>% 
  mutate(dept_num = location %>%
           str_extract_all(one_or_more(DIGIT)) %>% 
           as.numeric(),
         dept_num = (dept_num / 1000) %>% 
           floor()) %>% 
  left_join(France, by = c("dept_num")) %>% 
  distinct()

df_bm <- df %>% 
  filter(URL %in% url_bm$urls) %>% 
  mutate(diff = Price - pred)

#   ____________________________________________________________________________
#   Export                                                                  ####

df %>% vroom::vroom_write("Camping car.csv")

#   ____________________________________________________________________________
#   Plots (general)                                                         ####

fra %>% 
  st_transform(crs = 4326) %>% 
  ggplot() +
  geom_sf(show.legend = FALSE,
          color = "white",
          size = 0.2) +
  geom_point(data = df,
             aes(x = longitude, y = latitude,
                 col = Distance),
             size = 3) +
  geom_point(data = home_coord %>%
               t() %>% 
               as_tibble(),
             aes(x = V1, y = V2),
             col = "red") +
  scale_color_viridis_c() +
  theme_minimal() +
  coord_sf(datum = NA, expand = FALSE) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank())

df %>%
  ggplot(aes(x = Price, y = Year)) +
  geom_point(aes(col = Kms)) +
  geom_smooth(aes(group = 1)) +
  geom_point(data = df %>% 
               filter(str_detect(URL, "2259866420")),
             shape = 21,
             size = 4,
             col = "red") +
  scale_color_viridis_c()

#   ____________________________________________________________________________
#   Plots (bookmarks)                                                       ####

fra %>% 
  st_transform(crs = 4326) %>% 
  ggplot() +
  geom_sf(show.legend = FALSE,
          color = "white",
          size = 0.2) +
  geom_sf(data = df_bm %>% 
            filter(diff <=2000),
          aes(geometry = geometry,
              fill = dept_name),
          alpha = 0.3,
          color = "white",
          size = 0.2) +
  geom_point(data = df,
             aes(x = longitude, y = latitude,
                 col = Distance),
             size = 3,
             alpha = 0.5) +
  geom_point(data = home_coord %>%
               t() %>%
               as_tibble(),
             aes(x = V1, y = V2),
             col = "red") +
  geom_point(data = df_bm %>% 
               filter(diff <=2000),
             aes(x = longitude, y = latitude),
             shape = 21,
             size = 5,
             col = "red") +
  geom_point(data = df %>% 
               filter(str_detect(URL, "2259866420")),
             aes(x = longitude, y = latitude),
             shape = 21,
             size = 7,
             col = "blue") +
  scale_color_viridis_c() +
  theme_minimal() +
  coord_sf(datum = NA, expand = FALSE) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank())

df %>%
  ggplot(aes(x = Year, y = Price, col = Kms)) +
  geom_point() +
  # geom_smooth(aes(group = 1),
  #             method = "loess") +
  geom_line(data = df %>% 
              group_by(Year) %>% 
              summarise(pred = median(pred)),
            aes(y = pred, group = 1),
            col = "blue") +
  geom_point(data = df_bm,
             shape = 21,
             size = 4,
             col = "red") +
  geom_point(data = df %>% 
               filter(str_detect(URL, "2259866420")),
             shape = 21,
             size = 6,
             col = "blue") +
  scale_color_viridis_c()

#   ____________________________________________________________________________
#   Browse bookmarks                                                        ####

visit <- df_bm %>%
  filter(Distance <=200) %>% 
  # filter(dept_name %in% c("Savoie", "Doubs", "Jura", "Côte-d'Or", "Vosges")) %>% 
  arrange(diff)

visit %>% 
  pull(URL) %>% 
  walk(browseURL)

df_bm %>% 
  pull(URL) %>% 
  walk(browseURL)

#   ____________________________________________________________________________
#   Explore others non bookmarked                                           ####

explore <- df %>%
  # filter(diff <= 2000) %>% 
  # filter(dept_name %in% c("Savoie", "Doubs", "Jura", "Côte-d'Or")) %>% 
  # filter(Distance >= 150) %>% 
  # filter(Distance <= 200) %>% 
  filter(bunk, solar_panel) %>% 
  filter(Year %in% 1999:2008) %>% 
  filter(Kms <= 200000) %>% 
  filter(Kms >= 50000) %>% 
  arrange(Distance)

explore %>% 
  pull(URL) %>% 
  walk(browseURL)

#   ____________________________________________________________________________
#   Explore around particular location                                      ####

other_coord_table <- osmdata::getbb("Libourne 33500")
other_latitude <- (other_coord_table[2, 1] + other_coord_table[2, 2]) / 2
other_longitude <- (other_coord_table[1, 1] + other_coord_table[1, 2]) / 2
other_coord <<- c(other_longitude, other_latitude)

explore <- df %>% 
  mutate(Distance_other = NA_real_)
1:nrow(df) %>% 
  walk(function(x) {
    dist = geosphere::distGeo(c(df$longitude[x], df$latitude[x]), other_coord) / 1000
    explore$Distance_other[x] <<- dist
  })

explore <- explore %>% 
  filter(Distance_other <= 100)

fra %>% 
  st_transform(crs = 4326) %>% 
  ggplot() +
  geom_sf(show.legend = FALSE,
          color = "white",
          size = 0.2) +
  geom_sf(data = explore,
          aes(geometry = geometry,
              fill = dept_name),
          alpha = 0.3,
          color = "white",
          size = 0.2) +
  geom_point(data = df,
             aes(x = longitude, y = latitude,
                 col = Distance),
             size = 3,
             alpha = 0.5) +
  geom_point(data = home_coord %>%
               t() %>%
               as_tibble(),
             aes(x = V1, y = V2),
             col = "red") +
  geom_point(data = other_coord %>%
               t() %>%
               as_tibble(),
             aes(x = V1, y = V2),
             col = "yellow") +
  geom_point(data = explore,
             aes(x = longitude, y = latitude),
             shape = 21,
             size = 5,
             col = "red") +
  geom_point(data = df %>% 
               filter(str_detect(URL, "2256361528")),
             aes(x = longitude, y = latitude),
             shape = 21,
             size = 7,
             col = "blue") +
  scale_color_viridis_c() +
  theme_minimal() +
  coord_sf(datum = NA, expand = FALSE) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank())

explore %>% 
  pull(URL) %>% 
  walk(browseURL)
