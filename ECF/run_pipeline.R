dict = readRDS("final_dict.RDS")
min_ev_long = quantile(dict$order$long_ev,0.66)
min_ev_short = quantile(dict$order$short_ev,0.66)
min_sharpe_long = 0.2
min_sharpe_short = 0.2
sh_mul_long = 1
sh_mul_short = 1
cd = 4
max_ = 4
lev = 0.003
source("R_library.R")

### readcsv from predetermined space!

running = read.csv("tmp.csv")

running = TA(running)

last = tail(running,1)




index = read_dict(last)

long_trade = select_long(index)
short_trade = select_short(index)
# dict$expected_values_long[[index]]
# dict$expected_values_short[[index]]

to_json = list(
  long = long_trade,
  short = short_trade,
  min_long_ev = min_ev_long,
  min_short_ev = min_ev_short,
  index = index
)

saveRDS(to_json,"app_data.RDS")
