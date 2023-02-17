
# libraires ---------------------------------------------------------------

source(here::here("r", "libraries.r"))


# locations ---------------------------------------------------------------

url <- "https://wfhresearch.com/wp-content/uploads/2023/01/WFHtimeseries_monthly.xlsx"
library(googlesheets4)
read_sheet(url)

# get data ----------------------------------------------------------------

df1 <- read_excel(here::here("data", "WFHtimeseries_monthly.xlsx"),
           sheet="WFH by city",
           range="a1:m28")

glimpse(df1)

df2 <- df1 |> 
  pivot_longer(-date) |> 
  mutate(date=as.Date(date),
         fname=case_when(str_detect(name, "top10") ~ "Top 10 cities",
                         str_detect(name, "NewYork") ~ "NYC area",
                         TRUE ~ NA_character_))

p <- df2 |> 
  filter(!is.na(fname)) |> 
  ggplot(aes(date, value, colour=fname)) +
  geom_line() +
  geom_point() +
  scale_x_date(name=NULL, date_breaks = "3 months", date_labels = "%Y-%b") +
  scale_y_continuous(name="% of full-paid days WFH", breaks=seq(0, 100, 2)) +
  theme_bw() +
  ggtitle("Prevalence of Work From Home, NYC area and top 10 cities")

ggsave(filename=here::here("web", "images", "nyc_top10_wfh.png"),
       plot=p, 
       scale=1,
       width=16, height=10)


