setwd('~')
library(dplyr)

crsp = read.csv('data/crsp_1970-2012.csv',stringsAsFactors = FALSE)
crsp = crsp[crsp$SICCD<6000 || crsp$SICCD>=7000,]
crsp$date = as.POSIXct(crsp$date)
crsp$date_ym = format(crsp$date,"%Y-%m")
crsp = crsp[order(crsp$date_ym),]
crsp = crsp %>%
  group_by(PERMNO) %>%
  mutate(PRC_next_month = lead(PRC, 1)) %>%
  ungroup

crsp$size = log(abs(crsp$PRC)*crsp$SHROUT)

crsp.size = crsp[crsp$size>0 & !is.na(crsp$size) & !is.na(crsp$PRC_next_month),]
crsp.size = crsp.size %>%
  group_by(date_ym) %>%
  mutate(size.decile = ntile(size, 10)) %>%
  ungroup %>%
  group_by(date_ym,size.decile)

portfolios.by_size = summarise(crsp.size,ret=100*(sum(abs(PRC_next_month),na.rm=T)/sum(abs(PRC),na.rm=T)-1))
portfolios.by_size = portfolios.by_size[order(portfolios.by_size$date_ym),]
portfolios.by_size = portfolios.by_size %>%
  group_by(size.decile) %>%
  mutate(cum_ret = cumsum(ret)) %>%
  ungroup

portfolios.by_size = portfolios.by_size[portfolios.by_size$date_ym<'2012-12',]
portfolios.by_size$date_ym = sprintf('%s-01',portfolios.by_size$date_ym)
portfolios.by_size$date_ym = as.POSIXct(portfolios.by_size$date_ym)
portfolios.by_size$size.decile = as.factor(portfolios.by_size$size.decile)


library(ggplot2)
ggplot(portfolios.by_size, aes(x = date_ym, y = cum_ret, colour=size.decile)) +
  geom_line()

top_portfolio = portfolios.by_size[portfolios.by_size$size.decile==1,]
bottom_portfolio = portfolios.by_size[portfolios.by_size$size.decile==10,]

potfolio.diff = data.frame(date_ym=top_portfolio$date_ym)
potfolio.diff$cum_ret = top_portfolio$cum_ret - bottom_portfolio$cum_ret
ggplot(potfolio.diff, aes(x = date_ym, y = cum_ret)) +
  geom_line()


