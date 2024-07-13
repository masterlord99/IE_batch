source("R_library.R")
Rcpp::sourceCpp("Cpp_lib.cpp")

all = 1:15
n_max_dict = 50

## read data
data = readRDS("ECF.RDS")

## apply TA

data = lapply(data,TA)

## create space

## test sharpe
data = lapply(data,na.omit)

copy_data = data

# data = lapply(data,function(x)x[c(T,rep(F,11)),])
# data = data[c(-4)] ## last 2 are out of sample for this test
saveRDS(object = data,file = "TA.RDS")


dict_table = lapply(data,function(x){
  tmp = x[,all]
  uniq = tmp[!duplicated(tmp),]
  uniq
}) %>% do.call(rbind,.)

dict_table = dict_table[!duplicated(dict_table),]

## allow

allow =c()

pb <- progress_bar$new(
  format = "  Progress [:bar] :percent eta: :eta",
  total = nrow(dict_table),
  width = 60
)
for(i in 1:nrow(dict_table)){
  n = 0
  cand = dict_table[i,]
  
  
  for(j in 1:length(data)){
    tmp = data[[j]][,all] %>% as.matrix()
    cand = matrix(cand[1,],byrow = T,ncol=ncol(tmp),nrow=nrow(tmp))
    out = tmp == cand
    # out = apply(out,1,all)
    out = rowSums(out)>(length(all)-1)
    index = which(out)
    n = n + length(index)
  }
  allow = c(allow,n)
  pb$tick()
}

dict_table = dict_table[allow >n_max_dict,]



dict_ = dict_template
dict_$ind = all
dict_$table = dict_table

system.time({
  dict = create_dict(dict_,data)
})

## remove non sign

str(dict,1)
ind = lapply(1:length(dict$sharpes_short),function(x){
  evl = dict$expected_values_long[[x]]
  evs = dict$expected_values_short[[x]]
  if(sum(sign(evl)) + 1 >= ncol(evl)*nrow(evl)){
    return(T)
  }
  if(abs(sum(sign(evs))) + 5 >= ncol(evl)*nrow(evl)){
    return(T)
  }
  return(F)
}) %>% unlist()

dict$table = dict$table[ind,]
dict$expected_values_long = dict$expected_values_long[ind]
dict$expected_values_short = dict$expected_values_short[ind]
dict$sharpes_long = dict$sharpes_long[ind]
dict$sharpes_short = dict$sharpes_short[ind]


sharpe_short = lapply(dict$sharpes_short,function(x)c(x)) %>% unlist() %>% ecdf
sharpe_long = lapply(dict$sharpes_long,function(x)c(x)) %>% unlist() %>% ecdf
dict$sharpes_long = lapply(dict$sharpes_long,function(x){
  apply(x,2,sharpe_long)
})
dict$sharpes_short = lapply(dict$sharpes_short,function(x){
  apply(x,2,sharpe_short)
})
dict$order = list(
  long_ev = dict$expected_values_long %>% lapply(.,mean) %>% unlist(),
  short_ev = dict$expected_values_short %>% lapply(.,mean) %>% unlist(),
  long_sharpe = dict$sharpes_long %>% lapply(.,mean) %>% unlist(),
  short_sharpe = dict$sharpes_short %>% lapply(.,mean) %>% unlist()
)
dict$table = as.matrix(dict$table)



saveRDS(dict,"final_dict.RDS")



## run backtest and save
## params

min_ev_long = quantile(dict$order$long_ev,0.66)
min_ev_short = quantile(dict$order$short_ev,0.66)
min_sharpe_long = 0.2
min_sharpe_short = 0.2
sh_mul_long = 0.1
sh_mul_short = 0.1
cd = 8
max_ = 5
lev = 0.003

data = copy_data

long_dict = lapply(1:nrow(dict$table),select_long)
short_dict = lapply(1:nrow(dict$table),select_short)
bt = eval()


# mini_eval(bt,data,1)
visualize_eval(bt,data)
# plot_expectations(dict,1)
saveRDS(bt,"backtest.RDS")

# for(i in 1:nrow(dict$table)){
#   plot_expectations(dict,i)
#   readline(":")
# }
