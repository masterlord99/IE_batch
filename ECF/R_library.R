library(TTR)
library(magrittr)
library(progress)
library(ggplot2)
library(dplyr)
library(grid)


TA = function(x){
  ## params
  SMA = c(8,16)
  EMA = c(2,4)
  sma = lapply(SMA,function(y){
    TTR::SMA(x$Close,y)
  })
  ema = lapply(EMA,function(y){
    TTR::EMA(x$Close,y)
  })
  out = list()
  nm = c()
  for(em in ema){
    for(sm in sma){
      out[[length(out)+1]] = em/sm
    }
  }
  for(em in EMA){
    for(sm in SMA){
      nm = c(nm,paste0("EMA_",em,"_SMA_",sm))
    }
  }
  out = do.call(cbind,out)
  colnames(out)=nm
  bb = TTR::BBands(x[,c("High","Low","Close")],n = 16)[,4]
  vol = SMA(x$Volume,3)/SMA(x$Volume,24)
  df = data.frame(
    out>1,
    bb_range= bb < 0.9 & bb > 0.1,
    volume = vol > 1
  )
  df = apply(df,2,as.numeric) %>% as.data.frame()
  lower = df
  lower$Time = x$Time
  
  ## higher time frame
  x_clone = x
  n = 16
  x_clone[1:(n-1),1:5] = NA
  for(j in n:nrow(x_clone)){
    tmp = x[(j-n+1):j,]
    x_clone$Open[j] = tmp$Open[1]
    x_clone$Close[j] = tmp$Close[n]
    x_clone$Low[j] = min(tmp$Low,na.rm=T)
    x_clone$High[j] = max(tmp$High,na.rm=T)
    x_clone$Volume[j] = sum(tmp$Volume,na.rm = T)
    
  }
  ## roll
  x$ATR =(ATR(x_clone[,c("High","Low","Close")],n=10))[,"atr"]
  
  higher = x_clone
  out1 = data.frame()
  for(j in 1:n){
    
    ind = rep(F,n)
    ind[j] = T
    
    x_clone = higher[ind,]
    
    ## params
    SMA = c(20,40)
    EMA = c(4,8)
    sma = lapply(SMA,function(y){
      TTR::SMA(x_clone$Close,y)
    })
    ema = lapply(EMA,function(y){
      TTR::EMA(x_clone$Close,y)
    })
    out = list()
    nm = c()
    for(em in ema){
      for(sm in sma){
        out[[length(out)+1]] = em/sm
      }
    }
    for(em in EMA){
      for(sm in SMA){
        nm = c(nm,paste0("EMA_",em,"_SMA_",sm))
      }
    }
    out = do.call(cbind,out)
    colnames(out)=nm
    rsi = RSI(x_clone$Close,n=10)
    # obv = c(0,diff(OBV(x_clone$Close,x_clone$Volume)))
    stoch = stoch(x_clone[,c("High","Low","Close")])[,c(1,3)]
    bb = TTR::BBands(x_clone[,c("High","Low","Close")],n = 20)[,4]
    vol = SMA(x_clone$Volume,2)/SMA(x_clone$Volume,10)
    df = data.frame(
      out>1,
      rsi_cap =rsi <70 & rsi >30,
      stoch_momentum = stoch[,1]/stoch[,2] > 1, ## fastk/slowD
      bb_range_80 = bb > 0.8,
      bb_range_20 = bb < 0.2,
      volume = vol > 1
    )
    df = apply(df,2,as.numeric) %>% as.data.frame()
    df$Time = x_clone$Time
    out1 = rbind(out1,df)
  }
  
  higher = out1[order(out1$Time),]
  colnames(higher) = paste0("hour_",colnames(higher))
  
  df = cbind(lower,higher)
  df$Time = NULL
  df$hour_Time = NULL
  
  rownames(df) = x$Time
  df = cbind(df,x)
  df
}




dict_template = list(
  ind = NA,
  table = NA,
  expected_values_long = list(),
  sharpes_long = list(),
  expected_values_short = list(),
  sharpes_short = list(),
  length = c()
)

create_dict = function(dict,data){
  
  
  pb <- progress_bar$new(
    format = "  Progress [:bar] :percent eta: :eta",
    total = nrow(dict$table),
    width = 60
  )
  
  for(i in 1:nrow(dict$table)){
    cand = dict$table[i,]
    
    mean_short = matrix(0,5,5)
    sharpe_short = matrix(0,5,5)
    
    mean_long = matrix(0,5,5)
    sharpe_long = matrix(0,5,5)
    
    n = 0
    m = 0
    
    for(j in 1:length(data)){
      tmp = data[[j]][,dict$ind] %>% as.matrix()
      cand = matrix(cand[1,],byrow = T,ncol=ncol(tmp),nrow=nrow(tmp))
      out = tmp == cand
      # out = apply(out,1,all)
      out = rowSums(out)>=(length(dict$ind))
      index = which(out)
      if(length(index)>0){
        n = n + length(index)
        m = m + 1
        cpp_ind = index - 1
        cpp_thresh = data[[j]]$ATR[cpp_ind+1]/data[[j]]$Close[cpp_ind+1]
        cpp_thresh = matrix(c(
          cpp_thresh,
          cpp_thresh*3,
          cpp_thresh*5,
          cpp_thresh*10,
          cpp_thresh*100
        ),ncol = 5,nrow = length(cpp_thresh))
        eval_long = get_expectations(
          inds = cpp_ind,
          ts = data[[j]]$Close,
          thresh = cpp_thresh,
          relative = T,
          go_long = T,
          n = 4*10*5
        )
        eval_short = get_expectations(
          inds = cpp_ind,
          ts = data[[j]]$Close,
          thresh = cpp_thresh,
          relative = T,
          go_long = F,
          n = 4*10*5
        )
        mean_short = mean_short + eval_short$mean
        mean_long = mean_long + eval_long$mean
        
        if(length(index)>10){
          sharpe_short = sharpe_short + eval_short$sharpe
          sharpe_long = sharpe_long + eval_long$sharpe
        }
        
      }
      
    }
    mean_long = mean_long/m
    mean_short = mean_short/m
    
    sharpe_long = sharpe_long/m
    sharpe_short = sharpe_short/m
    
    rownames(mean_long) = paste0("SL_",c(1,3,5,10,100))
    colnames(mean_long) = paste0("TP_",c(1,3,5,10,100))
    
    rownames(mean_short) = paste0("SL_",c(1,3,5,10,100))
    colnames(mean_short) = paste0("TP_",c(1,3,5,10,100))
    
    rownames(sharpe_long) = paste0("SL_",c(1,3,5,10,100))
    colnames(sharpe_long) = paste0("TP_",c(1,3,5,10,100))
    
    rownames(sharpe_short) = paste0("SL_",c(1,3,5,10,100))
    colnames(sharpe_short) = paste0("TP_",c(1,3,5,10,100))
    
    pb$tick()
    
    dict$expected_values_long[[length(dict$expected_values_long)+1]] = mean_long
    dict$expected_values_short[[length(dict$expected_values_short)+1]] = mean_short
    dict$sharpes_long[[length(dict$sharpes_long)+1]] = sharpe_long
    dict$sharpes_short[[length(dict$sharpes_short)+1]] = sharpe_short
    dict$length = c(dict$length,n)
    
  }
  return(dict)
}




read_dict = function(x){
  match = matrix(x[,1:(ncol(x)-7)],nrow = nrow(dict$table),ncol=ncol(x)-7,byrow = T) == dict$table 
  i = which(rowSums(match) == ncol(x)-7)
  if(length(i)>0){
    return(i)
  }else{
    return(NA)
  }
}



select_long = function(i){
  ev = dict$expected_values_long[[i]][1:max_,1:max_]
  sh = dict$sharpes_long[[i]][1:max_,1:max_]
  run_max_i = 0
  run_max_j = 0
  run_max = 0
  for(i in 1:max_){
    for(j in 1:max_){
      cand = ev[i,j]*100 + sh[i,j]*sh_mul_long
      if(cand > run_max & (ev[i,j] > min_ev_long & sh[i,j] > min_sharpe_long)){## clear minimum cond
        run_max_i= i
        run_max_j = j
        run_max = cand
      }
    }
  }
  cc = gsub(x=colnames(ev),replacement = "",pattern = "TP_",fixed=T) %>% as.numeric()
  go_long = F
  if(run_max > 0){
    go_long = T
  }
  list(
    go_long = go_long,
    tp = cc[run_max_j],
    sl=cc[run_max_i]
  )
}

select_short = function(i){
  ev = dict$expected_values_short[[i]][1:max_,1:max_]
  sh = dict$sharpes_short[[i]][1:max_,1:max_]
  run_max_i = 0
  run_max_j = 0
  run_max = 0
  for(i in 1:max_){
    for(j in 1:max_){
      cand = ev[i,j]*100 + sh[i,j]*sh_mul_short
      if(cand > run_max & (ev[i,j] > min_ev_short & sh[i,j] > min_sharpe_short)){## clear minimum cond
        run_max_i= i
        run_max_j = j
        run_max = cand
      }
    }
  }
  cc = gsub(x=colnames(ev),replacement = "",pattern = "TP_",fixed=T) %>% as.numeric()
  go_short = F
  if(run_max > 0){
    go_short = T
  }
  list(
    go_short = go_short,
    tp = cc[run_max_j],
    sl=cc[run_max_i]
  )
}


eval = function(){
  
  all_money = list(
  )
  pb <- progress_bar$new(
    format = "  Progress [:bar] :percent eta: :eta",
    total = sum(lapply(data,nrow) %>% unlist())+100,
    width = 60
  )
  
  for(z in 1:length(data)){
    
    money = 1
    pnl = c()
    
    tmp = data[[z]]
    cooldown_long = rep(0,length(long_dict))
    cooldown_short = rep(0,length(short_dict))
    
    
    for(i in 1:nrow(tmp)){
      x = tmp[i,]
      ind = read_dict(x)
      if(!is.na(ind)){
        ## go long=
        lc = long_dict[[ind]]
        if(lc$go_long & cooldown_long[ind]<=0){
          tp = x$ATR/x$Close*lc$tp
          sl = -x$ATR/x$Close*lc$sl
          ts = tmp$Close[i:(min(i+4*10*5,nrow(tmp)))]
          roi = get_x_before_y_long(ts=ts,x=tp,y=sl,relative = T)
          roi = roi*lev
          money = money + roi
          cooldown_long[ind] = cd+1
        }
        
        ls = short_dict[[ind]]
        if(ls$go_short & cooldown_short[ind]<=0){
          tp = x$ATR/x$Close*ls$tp
          sl = -x$ATR/x$Close*ls$sl
          ts = tmp$Close[i:(min(i+4*10*5,nrow(tmp)))]
          roi = get_x_before_y_short(ts=ts,x=tp,y=sl,relative = T)
          roi = roi*lev
          money = money + roi
          cooldown_short[ind] = cd +1
        }
      }
      cooldown_long = cooldown_long - 1
      cooldown_short = cooldown_short - 1
      
      pnl = c(pnl,money)
      pb$tick()
    }
    all_money[[length(all_money)+1]] = pnl
  }
  return(all_money)
}



mini_eval = function(x,data,j){
  
  tmp = data.frame(PNL=(x[[j]]-1)*100,Close=data[[j]]$Close,Time=data[[j]]$Time)
  
  scale_factor <- max(tmp$PNL) / max(tmp$Close)
  m = 12*10*7
  ts = tmp$PNL[seq(1,nrow(tmp),by=m/7)]
  sharpe = mean(diff(ts))/sd(diff(ts))*sqrt(52)
  ts = tmp$PNL[seq(1,nrow(tmp),by=m/7)]
  mdd = stocks::mdd(gains=diff(ts/100))*100
  
  # Create the plot
  p <- ggplot(tmp, aes(x = Time)) +
    geom_line(aes(y = PNL, colour = "PNL (%)"), size = 1, linetype = "dotted") +
    geom_line(aes(y = Close * scale_factor, colour = "Close"), size = 0.6, linetype = "twodash") +
    scale_y_continuous(
      name = "PNL Curve",
      sec.axis = sec_axis(~./scale_factor, name = "Close")
    ) +
    labs(
      subtitle = "ECF futures day & swing trading",
      title = paste0(
        "Sharpe: ", round(sharpe,2),
        "\nMDD: ", round(mdd,2), " %"
      ),
      color = "Metric",
      x = "Time",
      y = "Scaled Value"
    ) +
    theme_classic() +
    scale_color_manual(values = c("PNL (%)" = "green3", "Close" = "darkblue")) +
    theme(legend.position = "top")
  return(p)
}


kayle_multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

visualize_eval = function(x,data){
  
  
  # Print the plot
  
  kayle_multiplot(
    mini_eval(x,data,1),
    mini_eval(x,data,2),
    mini_eval(x,data,3),
    mini_eval(x,data,4),
    mini_eval(x,data,5),
    mini_eval(x,data,6),
    cols=3
  )
  
}



plot_expectations = function(d,i){
  
  n = d$expected_values_long[[i]] %>% ncol()
  n1 = d$expected_values_long[[i]] %>% nrow()
  
  cn = colnames(d$expected_values_long[[i]])
  rn = rownames(d$expected_values_long[[i]])
  
  rn = gsub(rn,pattern="SL_",replacement="") %>% as.numeric()
  cn = gsub(cn,pattern="TP_",replacement="") %>% as.numeric()
  
  sl = c()
  tp = c()
  el = c()
  es = c()
  sl_ = c()
  ss = c()
  
  for(i1 in 1:n){
    for(j1 in 1:n1){
      tp = c(tp,cn[i1])
      sl = c(sl,rn[j1])
      el = c(el,d$expected_values_long[[i]][j1,i1])
      es = c(es,d$expected_values_short[[i]][j1,i1])
      sl_ = c(sl_,d$sharpes_long[[i]][j1,i1])
      ss = c(ss,d$sharpes_short[[i]][j1,i1])
    }
  }
  
  
  data = data.frame(SL =sl %>% as.factor(),
                    TP = tp %>% as.factor(),
                    Expectations_long=el*100,
                    Expectations_short=es*100,
                    Sharpe_long = sl_,
                    Sharpe_short=ss
  )
  
  theme_set(theme_classic())
  
  kayle_multiplot(
    
    ggplot(data, aes(x = SL, y = TP)) + 
      geom_tile(aes(fill=round(Expectations_long,2)),colour="black",linetype="dotted",linewidth=0.25) +
      geom_text(aes(label = round(Expectations_long, 2))) +
      scale_fill_gradient2(name = "Long EV (%)",
                           low="red", high="darkgreen", #colors in the scale
                           breaks=seq(min(data$Expectations_long),max(data$Expectations_long),length.out=10) %>% round(.,2),
                           midpoint=0)+
      ylab("Take Profit (H ATR)") + xlab("Stop Loss (H ATR)") + ggtitle(paste0("Total (%) won [X before losing Y] ~~~ number of trades = ",d$length[i]))
    ,
    
    ggplot(data, aes(x = SL, y = TP)) + 
      geom_tile(aes(fill=round(Expectations_short,2)),colour="black",linetype="dotted",linewidth=0.25) +
      geom_text(aes(label = round(Expectations_short, 2))) +
      scale_fill_gradient2(name = "Short EV (%)",
                           low="red", high="darkgreen", #colors in the scale
                           breaks=seq(min(data$Expectations_short),max(data$Expectations_short),length.out=10) %>% round(.,2),
                           midpoint=0)+
      ylab("Take Profit (H ATR)") + xlab("Stop Loss (H ATR)") + ggtitle(paste0("Total (%) won [X before losing Y] ~~~ number of trades = ",d$length[i]))
    ,
    
    
    ggplot(data, aes(x = SL, y = TP)) + 
      geom_tile(aes(fill=round(Sharpe_long,2)),colour="black",linetype="dotted",linewidth=0.25) +
      geom_text(aes(label = round(Sharpe_long, 2))) +
      scale_fill_gradient2(name = "Long Sharpe",
                           low="red", high="darkgreen", #colors in the scale
                           breaks=seq(min(data$Sharpe_long),max(data$Sharpe_long),length.out=10) %>% round(.,2),
                           midpoint=0.5)+
      ylab("Take Profit (H ATR)") + xlab("Stop Loss (H ATR)") + ggtitle(paste0("Long Sharpes ~~~ number of trades = ",d$length[i]))
    
    ,
    
    ggplot(data, aes(x = SL, y = TP)) + 
      geom_tile(aes(fill=round(Sharpe_short,2)),colour="black",linetype="dotted",linewidth=0.25) +
      geom_text(aes(label = round(Sharpe_short, 2))) +
      scale_fill_gradient2(name = "Short Sharpe",
                           low="red", high="darkgreen", #colors in the scale
                           breaks=seq(min(data$Sharpe_short),max(data$Sharpe_short),length.out=10) %>% round(.,2),
                           midpoint=0.5)+
      ylab("Take Profit (H ATR)") + xlab("Stop Loss (H ATR)") + ggtitle(paste0("Short Sharpes ~~~ number of trades = ",d$length[i]))
    
    ,
    
    cols=2
  )
}
