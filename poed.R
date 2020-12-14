library(rvest)
library(tidyverse)
library(httr)
library(jsonlite)





# Selver

adr_selv <- "https://www.selver.ee/kauplused/"



poed_selver <- read_html(adr_selv) %>%
  html_nodes(xpath = '//*[@class="col-xs-12 col-sm-6 col-md-6 col-lg-6"]') %>%
  html_nodes("div") %>%
  html_text() %>%
  .[seq(2, 165, 3)] %>%
  str_sub(2, -2) %>%
  str_subset(".") %>%
  .[1:22] %>%
  str_replace("Tallinn ", "Tallinn") %>%
  str_c(", Tallinn, Estonia") %>%
  str_replace("Tallinn, Tallinn", "Tallinn") %>%
  tibble(
    Address = .,

    Chain = "Selver"
  )



# Maxima

adr_maxima <- "https://minukataloogid.ee/tallinn/maxima"



poed_maixma <- read_html(adr_maxima) %>%
  html_nodes(xpath = '//*[@class="col-xs-12 col-sm-6"]') %>%
  html_nodes("span") %>%
  html_text() %>%
  .[seq(2, 58, 2)] %>%
  str_sub(12, -1) %>%
  str_subset(".") %>%
  str_c(" Tallinn Estonia") %>%
  tibble(
    Address = .,

    Chain = "Maxima"
  )





# Rimi

adr_rimi <- "https://minukataloogid.ee/tallinn/rimi"



poed_rimi <- read_html(adr_rimi) %>%
  html_nodes(xpath = '//*[@class="col-xs-12 col-sm-6"]') %>%
  html_nodes("span") %>%
  html_text() %>%
  .[seq(2, 58, 2)] %>%
  str_sub(12, -1) %>%
  str_subset(".") %>%
  str_c(" Tallinn Estonia") %>%
  tibble(
    Address = .,

    Chain = "Rimi"
  )



# Prisma

adr_prisma <- "https://minukataloogid.ee/tallin/prisma"



poed_prisma <- read_html(adr_prisma) %>%
  html_nodes(xpath = '//*[@class="col-xs-12 col-sm-6"]') %>%
  html_nodes("span") %>%
  html_text() %>%
  .[seq(2, 10, 2)] %>%
  str_sub(11, -1) %>%
  str_subset(".") %>%
  str_c(" Tallinn Estonia") %>%
  tibble(
    Address = .,

    Chain = "Prisma"
  )



# Kokku ------

POED <- reduce(list(poed_maixma, poed_prisma, poed_rimi, poed_selver), rbind) %>%
  mutate(
    Address = str_replace_all(Address, ",", ""),
    Address = str_replace_all(Address, "\\.", ""),
    Address = str_replace_all(Address, "[EJK] ", ""),
    Address = str_replace_all(Address, "ü", "u"),
    Address = str_replace_all(Address, "ä", "a"),
    Address = str_replace_all(Address, "ö", "o"),
    Address = str_replace_all(Address, "õ", "o"),
    Address = str_replace_all(Address, "Õ", "O"),
    Address = str_replace_all(Address, "Ahtri tn", "Ahtri"),
    Address = str_replace_all(Address, "Erika tn", "Erika")
  ) %>%
  arrange(Address) %>%
  mutate(Address = POED$Address %>% str_split(" Tallinn Estonia") %>% unlist() %>% str_subset("."))



url_api <- "http://api.positionstack.com/v1/forward?access_key=e85667213ea7694381b81204fec0766c&limit=5&country=EE&region=Harjumaa&query="

coords <- lapply(POED$Address, function(adr) {
  print(adr)

  res <- str_c(url_api, str_replace_all(adr, " ", "%20")) %>% GET()
  fromJSON(rawToChar(res$content))$data %>%
    as_tibble() %>%
    filter(is.na(locality)) %>%
    select(1, 2) %>%
    slice(1)
})


df_POED <- reduce(coords, rbind) %>%
  cbind(POED, .) %>%
  as_tibble() %>% 
  filter(latitude > 59.35 & latitude < 59.5)


library(sf)
setwd("~/Documents/DataScience/poed")
df_TLN <- st_read("asumid/t02_41_asum.shp") %>%
  st_transform(ds, crs = 4326)


x_cent <- mean(c(59.35181, 59.49814))
y_cent <- mean(c(24.55035, 24.92628))
marg <- .2
     
df_TLN %>%
  filter(asumi_nimi != "Aegna") %>%
  ggplot() +
  geom_sf(fill = "#454545", color = "#242424") +
  geom_point(data = df_POED, aes(x = longitude, y = latitude, color = Chain), size = 2.5) +
  scale_color_manual(values = c("#135b9b", "#059937", "#b02026", "#f2e20a")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "#242424"),
        plot.background = element_rect(fill = "#242424"),
        legend.background = element_rect(fill = "#242424"),
        legend.key = element_rect(fill = "#242424"),
        legend.key.size = unit(4,"point"),
        legend.text = element_text(color = "white", size = 10),
        legend.position = c(.8, .25)) 


ggsave("poed.png",
       width = 8, height = 8)


