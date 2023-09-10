

# https://data.ny.gov/Transportation/MTA-Daily-Ridership-Data-Beginning-2020/vxuj-8kew
# https://data.ny.gov/Transportation/MTA-Daily-Ridership-Data-Beginning-2020/vxuj-8kew/data
# https://data.ny.gov/api/views/vxuj-8kew/rows.csv?accessType=DOWNLOAD&sorting=true

# MTA_Daily_Ridership_Data__Beginning_2020.csv


# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))


# locations ---------------------------------------------------------------
# https://data.ny.gov/Transportation/MTA-Daily-Ridership-Data-Beginning-2020/vxuj-8kew
# https://data.ny.gov/Transportation/MTA-Daily-Ridership-Data-Beginning-2020/vxuj-8kew/data
# https://data.ny.gov/api/views/vxuj-8kew/rows.csv?accessType=DOWNLOAD&sorting=true

# MTA_Daily_Ridership_Data__Beginning_2020.csv

url <- "https://data.ny.gov/api/views/vxuj-8kew/rows.csv?accessType=DOWNLOAD&sorting=true"
fname <- "MTA_Daily_Ridership_Data__Beginning_2020.csv"

mta_path <- here::here("data", fname)

#.. graph theme items ----
legend_none <- theme(legend.position = "None")
legend_notitle <- theme(legend.title = element_blank())
caption_left <- theme(plot.caption = element_text(hjust = 0))

# UPDATE: download data --------------------------------------------------------
download.file(url, destfile=mta_path, mode="wb")


# get data ----------------------------------------------------------------

df1 <- read_csv(mta_path)

glimpse(df1)
ht(df1)
count(df1 |> mutate(date=as.Date(Date, format="%m/%d/%Y")), date) |> ht()

# xmap <- read_csv(
# "vname, vlabel
# sub_number, Subways: Total Estimated Ridership
# sub_pctpre, Subways: % of Comparable Pre-Pandemic Day
# bus_number, Buses: Total Estimated Ridership
# bus_pctpre, Buses: % of Comparable Pre-Pandemic Day
# lirr_number, LIRR: Total Estimated Ridership
# lirr_pctpre, LIRR: % of 2019 Monthly Weekday/Saturday/Sunday Average
# mn_number, Metro-North: Total Estimated Ridership
# mn_pctpre, Metro-North: % of 2019 Monthly Weekday/Saturday/Sunday Average
# access_number, Access-A-Ride: Total Scheduled Trips
# access_pctpre, Access-A-Ride: % of Comparable Pre-Pandemic Day
# bridge_number, Bridges and Tunnels: Total Traffic
# bridge_pctpre, Bridges and Tunnels: % of Comparable Pre-Pandemic Day
# ")
# xmap


(longnames <- names(df1))
shortnames <- c("date", 
               "sub_number", "sub_pctpre",
               "bus_number", "bus_pctpre",
               "lirr_number", "lirr_pctpre",
               "mn_number", "mn_pctpre",
               "access_number", "access_pctpre",
               "bridge_number", "bridge_pctpre",
               "sir_number", "sir_pctpre")

cbind(shortnames, longnames) # verify that names line up

# Rename using a named vector and `all_of()`
lookup <- setNames(longnames, shortnames)

df2 <- df1 |> 
  rename(all_of(lookup)) |> 
  mutate(date=as.Date(date, format="%m/%d/%Y")) |> 
  arrange(date)
glimpse(df2)
tmp <- count(df2, date)
ht(tmp)
summary(df2)
skim(df2)
# smooth <- loess(sub_pctpre ~ date, data = df2 |> filter(date >= "2021-06-01"))

# calculate trends
df3 <- df2 |>
  select(-starts_with("access")) |> 
  mutate(dow=wday(date, label=FALSE),
         ldow=case_when(dow %in% c(1, 7) ~ "Weekend",
                        dow %in% 2:6 ~ "Weekday")) |> 
  pivot_longer(-c(date, dow, ldow)) |> 
  mutate(vlabel=factor(name, levels=shortnames, labels=longnames)) |> 
  group_by(name, vlabel, ldow) |> 
  arrange(date) |> 
  do(cbind(., stldf(.$value, 365))) |> 
  ungroup()
glimpse(df3)
summary(df3)
count(df3, name, vlabel)

# plot --------------------------------------------------------------------

capt1a <- "https://data.ny.gov/Transportation/MTA-Daily-Ridership-Data-Beginning-2020/vxuj-8kew"
(capt1 <- paste0("Source: MTA (", capt1a, "), with loess trend curves added"))

capt2a <- "Note: vertical axis limited to 140% to exclude outliers."
(capt2 <- paste0(capt2a, "\n", capt1))

ncol <- 2
psize <- 0.6
lwidth <- .6
wrap <- 35

p1 <- df3 |>
  filter(str_detect(name, "number"),
         date >= "2020-03-01") |> 
  ggplot(aes(date, value, colour=ldow)) +
  scale_colour_manual(values=c("red", "blue")) +
  geom_point(size=psize) +
  geom_smooth(se=FALSE, 
              span = 0.6, # default span = 0.75.
              linewidth=lwidth,
              data=function(data) data |> filter(date >= "2020-07-01")) +
  scale_x_date(name=NULL, date_breaks = "3 months", date_labels = "%Y-%b") +
  scale_y_continuous(name="Usage (thousands)",
                     # breaks=seq(0, 2, .2),
                     labels = label_number(accuracy=1, scale = 1e-3)) +
  ggtitle("Daily MTA system usage",
          subtitle="Trips, traffic, or ridership (thousands)") +
  labs(caption=capt1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45, vjust = 0, hjust=0)) +
  legend_notitle +
  caption_left +
  facet_wrap(~str_wrap(vlabel, wrap), ncol=ncol, scales = "free")
p1

p2 <- df3 |>
  # filter(name=="bus_pctpre") |> 
  filter(str_detect(name, "pctpre"),
         date >= "2020-03-01") |> 
  # mutate(ldow=str_wrap(ldow, width=40)) |> 
  ggplot(aes(date, value, colour=ldow)) +
  scale_colour_manual(values=c("red", "blue")) +
  geom_point(size=psize) +
  # geom_line(colour="darkgreen",
  #           aes(y=trend), data=function(data) data |> filter(date >= "2020-08-01")) +
  geom_smooth(se=FALSE, 
              span = 0.6, # default span = 0.75.
              linewidth=lwidth,
              data=function(data) data |> filter(date >= "2020-07-01")) +
  scale_x_date(name=NULL, date_breaks = "3 months", date_labels = "%Y-%b") +
  scale_y_continuous(name=NULL,
                     breaks=seq(0, 2, .2),
                     labels = label_percent(accuracy=1),
                     limits=c(0, 1.4)) +
  geom_hline(yintercept = 1, linetype="solid", linewidth=.4, colour="grey20") +
  ggtitle("Status of recovery in use of MTA systems",
          subtitle="Daily activity as % of comparable pre-pandemic period") +
  labs(caption=capt2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45, vjust = 0, hjust=0)) +
  legend_notitle +
    caption_left +
  facet_wrap(~str_wrap(vlabel, wrap), ncol=ncol)
p2

# p2a <- p2 + theme(strip.text = element_text(size=10, face="bold"))
# p2a

p12 <- p1 + p2
p12
# saveRDS(p12, here::here("results", "mta_recovery.rds"))
ggsave(filename=here::here("web", "images", "mta_recovery.png"),
       plot=p12, 
       scale=1.25,
       width=16, height=10)


# end plot ----------------------------------------------------------------
