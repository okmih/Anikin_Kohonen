library(lattice)
library(cluster)
library(kohonen)
library(dplyr)
source('my_functions.R', echo=FALSE)

############################################################################
# ==================== Data set ====================
scale <- TRUE               # TRUE|FALSE
nfactors <- 0               # 0|1 - number of factors
file_name <- "sector120_3D"
## nfactors=1: |"iris_2var"|"iris_3var"|"iris_4var"|"wine"|"waveform_22var"|
##             |"waveform_22var_1500"|"clust4"|
## nfactors=0: |"wsample"|"triangle"|"square"|"circle"|"ring"|tors"|"rect"|
##             |"sector180"|"sector120"|"sector120_3D"|"arcs"|"yeast"|

if(nfactors==1) {
  path <- switch (file_name,
                  iris_2var = "data/iris_2var.csv",
                  iris_3var = "data/iris_3var.csv",
                  iris_4var = "data/iris_4var.csv",
                  wine = "data/wine.csv",
                  waveform_22var = "data/waveform_22var.csv",
                  waveform_22var_1500 = "data/waveform_22var_1500.csv",
                  clust4 = "data/clust4.csv"
  )
  df <- read.csv(file=path, sep=";")
  names(df)[ncol(df)] = "factor"
} else if (file_name=="yeast") {
  data("yeast")
  #  df <- as.data.frame(yeast$cln)
  #  df <- as.data.frame(yeast$clb)
  df <- as.data.frame(yeast$alpha)
  #  df <- as.data.frame(yeast$cdc15)
  #  df <- as.data.frame(yeast$cdc28)
  #  df <- as.data.frame(yeast$elu)
  df <- na.omit(df)
} else {
  path <- switch (file_name,
                  wsample = "data/wsample.csv",
                  triangle = "data/triangle.csv",
                  square = "data/square.csv",
                  circle = "data/circle.csv",
                  ring = "data/ring3000.csv",
                  sector180 = "data/sector180.csv",
                  sector120 = "data/sector120.csv",
                  sector120_3D = "data/sector120_3D.csv",
                  arcs = "data/arcs.csv",
                  tors = "data/tors.csv",
                  rect = "data/rect.csv"
  )
  df <- read.csv(file=path, sep=";")
}

ncols <- ncol(df)-nfactors  
nrows <- nrow(df)           
summary(df[,1:ncols])
if (scale) { df[,1:ncols] <- scale(df[,1:ncols]) }

cor <- cor(df[,1:ncols])-diag(ncols)
max.cor <- apply(cor,2,max)
rank <- rank(max.cor, ties.method = "first")     
df <- df %>% select(which(rank==ncols-1), which(rank==ncols), everything())

names.xy <- names(df)[1:2]
names(df)[1:2] <- c("x", "y")
df.cmeans <- colMeans(df[,1:ncols])

if (file_name=="tors") {
  library(scatterplot3d)
  scatterplot3d(df$x, df$y, df$z, angle = 25,
                highlight.3d = TRUE, pch=16, cex.symbols = 0.5)
}

############################################################################
neib8 <- TRUE               # TRUE - Moore, FALSE - von Neuman 
tor <- "no"                 # "tor"|"tor.h"|"no" 
neu_nums <- FALSE           # TRUE|FALSE
self <- TRUE                # TRUE|FALSE
ndiv <- 0                   # 0|1|2
if (self) {
  init_neu.pos <- "far"     # far|init - configuration
  init.mean <- TRUE         # TRUE|FALSE
  if (!init.mean) { 
    init.winners <- c(36,1)
    init.df <- c(25,125) 
  }
}
determ_ca <- TRUE           # TRUE|FALSE
threshold <- 5
EPS <- 0.001               
ndiv <- ndiv+1

############################################################################
edit <- TRUE                # TRUE|FALSE
view.plot <- TRUE           # TRUE|FALSE
alpha <- 20
inv.y <- FALSE              # TRUE|FALSE
dalpha <- 0
delay <- 0.8                # 0<delay<1
mg <- 20
ng <- 15
nlinks <- 45
epochs <- nlinks+20
mode <- "batch"             # "online"|"batch"
rlen <- 100
QDM_diff <- TRUE            # TRUE|FALSE

seed <- sample(1:1000, 1)
seed <- 101
set.seed(seed)

############################################################################
ndiv <- as.integer(ndiv)
N <- mg*ng
# ========== Links map ========== 
error <- try({ 
  if (tor=="tor"|tor=="tor.h") {
    nn <- 4*N
    if (neib8) {                                 # if neibh8==TRUE
      data.map <- matrix(rep("+",nn),ncol=2*mg)
      v <- rep("+",2*N)
      v[((2*N):1)%%2==1] <- N:1
      mx <- matrix(v, nrow=ng, byrow=T)
      mx <- mx[,rev(1:(2*mg))]
      data.map[(1:(2*ng))%%2==1,] <- mx
    } else {                                     # if neibh8==FALSE
      data.map <- matrix(rep("",nn),ncol=2*mg)
      v <- rep("",2*N)
      v[((2*N):1)%%2==1] <- N:1
      mx <- matrix(v, nrow=ng, byrow=T)
      mx <- mx[,rev(1:(2*mg))]
      data.map[(1:(2*ng))%%2==1,] <- mx
      mtrx <- matrix(rep(c("+",""),N), nrow=ng, byrow=T)
      data.map[(1:(2*ng))%%2==0,] <- mtrx
      mtrx <- matrix(rep(c("+",""),N), nrow=2*mg)
      data.map[,(1:(2*mg))%%2==0] <- mtrx
      rm(mtrx)
    }  
    neu.map <- data.frame(data.map, stringsAsFactors = FALSE)
    colnames(neu.map) <- paste("c",1:(2*mg), sep="")
    rownames(neu.map) <- paste("r",1:(2*ng), sep="")
    if (edit) {
      fix(neu.map)
      write.csv2(neu.map, file="neu_map.csv", row.names = FALSE)
    }
    neu.map <- as.matrix(read.csv2(file="neu_map.csv"))
    colnames(neu.map) <- paste("c",1:(2*mg), sep="")
    rownames(neu.map) <- paste("r",1:(2*ng), sep="")
    rm(v, nn, mx, data.map)
  } else {                                 # if (tor.1==FALSE)
    nn <- (2*mg-1)*(2*ng-1)
    if (neib8) {                                 # if neibh8==TRUE
      data.map <- matrix(rep("+",nn),nrow=2*ng-1)
      v <- rep("+",2*N)
      v[((2*N):1)%%2==0] <- N:1
      mx <- matrix(v, nrow=ng, byrow=T)
      mx <- mx[,rev(1:(2*mg-1))]
      data.map[(1:(2*ng-1))%%2==1,] <- mx
    } else {                                     # if neibh8==FALSE
      data.map <- matrix(rep("",nn),nrow=2*ng-1)
      v <- rep("+",2*N)
      v[((2*N):1)%%2==0] <- N:1
      mx <- matrix(v, nrow=n, byrow=T)
      mx <- mx[,rev(1:(2*mg-1))]
      data.map[(1:(2*ng-1))%%2==1,] <- mx
      mtrx <- matrix(rep(c(rep(c("+",""),mg-1),"+"),ng-1), nrow=ng-1, byrow=T)
      data.map[(1:(2*ng-1))%%2==0,] <- mtrx
      rm(mtrx)
    }
    neu.map <- data.frame(data.map, stringsAsFactors = FALSE)
    colnames(neu.map) <- paste("c",1:(2*mg-1), sep="")
    rownames(neu.map) <- paste("r",1:(2*ng-1), sep="")
    if (edit) {
      fix(neu.map)
      write.csv2(neu.map, file="neu_map.csv", row.names = FALSE)
    }
    neu.map <- as.matrix(read.csv2(file="neu_map.csv"))
    colnames(neu.map) <- paste("c",1:(2*mg-1), sep="")
    rownames(neu.map) <- paste("r",1:(2*ng-1), sep="")
    rm(v, nn, mx, data.map)
  }
}, silent = TRUE)  # конец try()
if("try-error" %in% class(error)) 
  stop("Error\nSet edit<=TRUE...", call. = FALSE)

# ================= Neuron positions =================
grid <- somgrid(mg, ng)
x <- c(min(df$x), max(df$x))
y <- c(min(df$y), max(df$y))
v1 <- as.integer((grid$pts[,"x"]-1))*(x[2]-x[1])/((mg-1)+1e-9)+x[1]
v2 <- as.integer(grid$pts[,"y"]-1)*(y[2]-y[1])/((ng-1)+1e-9)+y[1]
xlim <- c(min(v1, x[1])-0.2, max(v1, x[2])+0.3)
ylim <- c(min(v2, df$y)-0.2, max(v2, df$y)+0.2)
if (ncols>2) {
  neu <- data.frame(v1, v2, matrix(numeric(ng*(ncols-2)),ncol=ncols-2))
  colnames(neu) <- colnames(df)[1:ncols]
  colnames(neu)[1:2] <- c("x", "y")
} else {
  neu <- data.frame(v1, v2)
  colnames(neu) <- colnames(df)[1:ncols]
  colnames(neu)[1:2] <- c("x", "y")
}
neu.init <- neu
# --- Map rotation ---
if (self){
  alpha <- pi/180*alpha
  neu.x <- neu[,1]*cos(alpha)-neu[,2]*sin(alpha)
  neu.y <- neu[,1]*sin(alpha)+neu[,2]*cos(alpha)
  if (inv.y) { neu.y <- -neu.y }
  neu[,"x"] <- neu.x
  neu[,"y"] <- neu.y
  neu.init <- neu
  rm(neu.x, neu.y)
}
rm(v1, v2, x, y)

############################################################################
# =========== som.koh learning ===========
init <- list(matrix(runif(N),nrow=N,ncol=ncols))
dm <- as.matrix(df[,1:ncols])
if (mode=="online") {
  som.koh <- som(dm, grid=somgrid(mg,ng,"rectangular"), rlen=rlen)
} else {
  som.koh <- som(dm, grid=somgrid(mg,ng,"rectangular"), mode="batch", rlen=rlen)  
}  
codes.koh <- as.matrix(as.data.frame(som.koh$codes))
rownames(codes.koh) <- 1:N
print(xyplot(y ~ x, data = df, pch=16, cex=0.25, col=rgb(.6,.6,.6), asp=1, main="grid", layout=c(1,1), 
             panel=my_panel, neu=neu.init, neu.map=neu.map, neu_nums=neu_nums, tor=tor))
print(xyplot(y ~ x, data = df, pch=16, cex=0.25, col=rgb(.6,.6,.6), asp=1, panel=my_panel, 
             neu=codes.koh[,1:2], neu.map=neu.map, neu_nums=neu_nums, tor=tor))
if (self & epochs>0) {
  if (init_neu.pos=="far") { 
    neu[,1:ncols] <- neu.init[,1:ncols]/EPS
  }
  if (init.mean) {
    init.winners <- mg*floor(ng/2.)+floor(mg/2.)+1
    neu[init.winners,] <- df.cmeans
  } else {
    neu[init.winners,] <- df[init.df, 1:ncols]
  }
} else {
  neu <- codes.koh
  rownames(neu) <- rownames(codes.koh)
} 
neu.prev <- as.matrix(neu)

# =========== som.ca learning ===========
if (epochs>0) {
  #  epochs=1
  hmax <- ifelse(self & init_neu.pos=="far", round(max(ng, mg)/2), 0)
  prev.diff <- numeric(0)
  QDMi <- numeric(epochs)
  cond <- TRUE
  for (epoch in 1:epochs) {  # SOM training cycle
    if(self & (epoch==round(hmax+(nlinks-hmax)*delay))) {
      # --- Поворот карты ---
      dalpha <- pi/180*dalpha
      neu.x <- neu[,1]*cos(dalpha)-neu[,2]*sin(dalpha)
      neu.y <- neu[,1]*sin(dalpha)+neu[,2]*cos(dalpha)
      neu[,"x"] <- neu.x
      neu[,"y"] <- neu.y
    }
    if (determ_ca) {  
      wts <- EPS*neu.init
    } else {
      wts <- as.data.frame(matrix(EPS*(rnorm(N*ncols)), ncol=ncols))
    }  
    colnames(wts) <- colnames(df[,1:ncols])
    wts$wins <- wts$neibhs <- wts$alls <- integer(N) 
    dr <- list(0)
    for (i in 1:nrows) {   # current epoch
      r <- numeric(N)
      for (j in 1:ncols) { r <- r+(neu[,j]-df[i,j])^2 }
      winner <- which.min(r)
      v.winners <- numeric(N)
      if (cond) {   
        if (neib8) {
          if (tor=="tor") {
            v.winners <- neu.winners.neib8.tor(winner, neu.map)
          } else if (tor=="tor.h") {
            v.winners <- neu.winners.neib8.tor.h(winner, neu.map)
          } else {  
            v.winners <- neu.winners.neib8(winner, neu.map)
          }
        } else {
          if (tor=="tor") {
            v.winners <- neu.winners.neib4.tor(winner, neu.map)
          } else if (tor=="tor.h") {
            v.winners <- neu.winners.neib4.tor.h(winner, neu.map)
          } else {
            v.winners <- neu.winners.neib4(winner, neu.map)
          }  
        }  
      }
      v.winners[winner] <- 1
      wts[winner,"wins"] <- wts[winner,"wins"]+1
      wts[v.winners>0 & v.winners!=1,"neibhs"] <- wts[v.winners>0 & v.winners!=1,"neibhs"]+1
      wts[v.winners>0,"alls"] <- wts[v.winners>0,"alls"]+1
      wts[v.winners>0,1:ncols] <- mapply(`-`, wts[v.winners>0,1:ncols], df[i,1:ncols])
    } # / current epoch
    win.matrix <- matrix(rep(wts$alls, ncols), ncol = ncols)
    wts[wts$alls>0, 1:ncols] <- -wts[wts$alls>0, 1:ncols]/win.matrix[wts$alls>0,]
    neu[wts$alls>0, 1:ncols] <- as.matrix(wts[wts$alls>0, 1:ncols])
  if (view.plot) {
    if (nfactors>0) {
      print(xyplot(y ~ x, data = df, group = factor, pch=16, cex=0.5, asp=1,
                   panel=my_panel, neu=neu, neu.map=neu.map, neu_nums=neu_nums, tor=tor))
    } else { 
      print(xyplot(y ~ x, data = df, pch=16, cex=0.25, col=rgb(.6,.6,.6), asp=1,
                   panel=my_panel, neu=neu, neu.map=neu.map, neu_nums=neu_nums, tor=tor))
    }
}   # / if view.plot
    # ------------------------------------------------------------------ 
    QDM <- calcQDM(df[,1:ncols],neu)
    QDMi[epoch] <- QDM[[1]]
    if (QDMi[epoch]>threshold) QDMi[epoch] <- 0
    if (QDMi[epoch]<EPS) {
      if (ndiv>1) cond <- epoch%%ndiv==1 else cond <- TRUE 
    } else {                     
      if (epoch<=nlinks) {
        cond <- TRUE
      } else {
        cond <- FALSE
      }
    }  
    neu.diff <- (neu-neu.prev)*(neu-neu.prev)
    prev.diff[epoch] <-  sum(neu.diff)
    if (prev.diff[epoch]>threshold) prev.diff[epoch] <-threshold
    neu.prev <- neu  
    cat("epoch =", epoch, "\n")
    cat("QDMi =", round(QDMi[epoch],4), "\n")
    cat("prev.diff =", round(prev.diff[epoch],2), "\n\n")
  }  # / SOM training cycle
  rm(dr, win.matrix)
} # / if (epochs>0)

############################################################################
# ========== SOM visualization ==========
som <- som.koh
if (epochs==0) codes <- codes.koh
if (epochs>0) {
  codes <- as.matrix(neu)
  colnames(codes)[1:2] <- names.xy
  som$codes <- list(codes)
  df.map <- map(som,dm)
  som$distances <- df.map$distances
  som$unit.classif <- df.map$unit.classif
  som$whatmap <- df.map$whatmap
  som$user.weights <- df.map$user.weights
}
opar <- par(no.readonly=TRUE)
par(mfrow = c(2,1))
plot(som.koh, type="counts", main="counts", 
     palette.name=coolBlueHotRed)
plot(som, type="counts", main="my counts",
     palette.name=coolBlueHotRed)
#plot(som.koh, type="mapping", main="mapping", shape="straight",
#     palette.name=coolBlueHotRed)
#plot(som, type="mapping", main="my mapping", shape="straight",
#     palette.name=coolBlueHotRed)
plot(som.koh, type="quality", main="quality",
     palette.name=coolBlueHotRed)
plot(som, type="quality", main="my quality",
     palette.name=coolBlueHotRed)
plot(som.koh, type="dist.neighbours", palette.name=coolBlueHotRed, 
     main="neibh", shape="straight")
plot(som, type="dist.neighbours", palette.name=coolBlueHotRed, 
     main="my neibh", shape="straight")
plot(som.koh, type="codes", main="codes",
     palette.name=coolBlueHotRed)
plot(som, type="codes", main="my codes",
     palette.name=coolBlueHotRed)
for (i in 1:ncols) {
  plot(som.koh, type = "property", property = getCodes(som.koh, 1)[,i],
       main = paste("property: ",colnames(getCodes(som.koh, 1))[i]),
       palette.name=coolBlueHotRed)
  plot(som, type = "property", property = getCodes(som, 1)[,i],
       main = paste("my property: ",colnames(getCodes(som, 1))[i]),
       palette.name=coolBlueHotRed)
}
par(opar)

############################################################################
# ========== QDM ==========
points <- seq(0,1,1/N)
#indx <- 2
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,1), mar=c(3,5,1,1), font=2, font.axis=2, font.lab=2)
for (indx in 1:ncols) {
  cat("\nindx = ", indx, "\n")
  q.koh <- quantile(codes.koh[,indx],points)
  q <- quantile(codes[,indx],points)
  q.df <- quantile(df[,indx],points)
  diff.q.koh <- q.koh-q.df
  diff.q <- q-q.df
  if (QDM_diff) {
    ymin <- min(diff.q.koh, diff.q)
    ymax <- max(diff.q.koh, diff.q)
    plot(diff.q.koh, type ="l", col="green", lwd=2, ylim=c(ymin,ymax))
    if (epochs>0) points(diff.q, type ="l", col="black", lwd=2)
    abline(h=0)
  } else {
    ymin <- min(q.koh[1], q[1], q.df[1])
    ymax <- max(q.koh[N], q[N], q.df[N])
    plot(q.df, type="l", col="green", lwd=2, ylim=c(ymin,ymax))
    points(q.koh, type ="l", col="red", lwd=2)
    points(q, type ="l", col="black", lwd=2)
    abline(q.df[1],(q.df[N]-q.df[1])/N, h=0)
  }
}
par(opar)
QDM.koh <- calcQDM(df[,1:ncols],codes.koh)
QDM <- calcQDM(df[,1:ncols],neu)
cat("\nsom_koh.QDM =", round(QDM.koh[[1]],4), "\n")
cat("som_koh.dist.mean =", round(mean(som.koh$distances),4), "\n")
cat("som_koh.nums.mean =", round(mean(table(som.koh$unit.classif)),2), 
    "\u2213", round(sd(table(som.koh$unit.classif)),2), "\n\n")
cat("som.QDM =", round(QDM[[1]],4), "\n")
cat("som.dist.mean =", round(mean(som$distances),4), "\n")
cat("som.nums.mean =", round(mean(table(som$unit.classif)),2), 
    "\u2213", round(sd(table(som$unit.classif)),2), "\n\n")
cat("som.QDM/som_koh.QDM =", round(QDM[[1]]/QDM.koh[[1]],3), "\n")
cat("\nsom.koh.QDMj: ") 
print(round(QDM.koh[[2]],4))
cat("som.QDMj: ") 
print(round(QDM[[2]],4))
if (epochs>0) {
  cat("\nQDMi:\n")
  print(round(QDMi,4))
  QDMi.min <- min(QDMi[QDMi>EPS])
  cat("\nQDMi.min =", round(QDMi.min,4), "при indx =", 
      length(QDMi[QDMi==0])+which.min(QDMi[QDMi>EPS]), "\n")
}
cat("\ncor(q.df,q.koh) =", cor(q.df,q.koh), "\n")
if (epochs>0) {
  cat("cor(q.df,q) =", cor(q.df,q), "\n")
  cat("cor(q.df,abs(q.koh-q)) =", cor(q.df,abs(q.koh-q)), "\n")
}

############################################################################
par(mfrow = c(1,1))
print(xyplot(y ~ x, data = df, pch=16, cex=0.25, col=rgb(.6,.6,.6), asp=1, panel=my_panel, 
             neu=codes.koh[,1:2], neu.map=neu.map, neu_nums=neu_nums, tor=tor, main="som.koh"))
print(xyplot(y ~ x, data = df, pch=16, cex=0.25, col=rgb(.6,.6,.6), asp=1, main="grid", layout=c(1,1), 
             panel=my_panel, neu=neu.init, neu.map=neu.map, neu_nums=neu_nums, tor=tor))
if (nfactors>0) {
  print(xyplot(y ~ x, data = df, group = factor, pch=16, cex=0.5, asp=1, main="som.ca",
               panel=my_panel, neu=neu[,1:2], neu.map=neu.map, neu_nums=neu_nums, tor=tor))
  if (ncols>2) {
    print(xyplot(df[,3] ~ x, data = df, pch=16, cex=0.25, col=rgb(.6,.6,.6), asp=1, main="som.ca",
                 panel=my_panel, neu=neu[,c(1,3)], neu.map=neu.map, neu_nums=neu_nums, tor=tor))
    print(xyplot(df[,3] ~ y, data = df, group = factor, pch=16, cex=0.5, asp=1, main="som.ca",
                 panel=my_panel, neu=neu[,2:3], neu.map=neu.map, neu_nums=neu_nums, tor=tor))
  }
} else { 
  print(xyplot(y ~ x, data = df, pch=16, cex=0.25, col=rgb(.6,.6,.6), asp=1, main="som.ca",
               panel=my_panel, neu=neu[,1:2], neu.map=neu.map, neu_nums=neu_nums, tor=tor))
  if (ncols>2) {
    print(xyplot(df[,3] ~ x, data = df, pch=16, cex=0.25, col=rgb(.6,.6,.6), asp=1, main="som.ca",
                 panel=my_panel, neu=neu[,c(1,3)], neu.map=neu.map, neu_nums=neu_nums, tor=tor))
    print(xyplot(df[,3] ~ y, data = df, pch=16, cex=0.25, col=rgb(.6,.6,.6), asp=1, main="som.ca",
                 panel=my_panel, neu=neu[,2:3], neu.map=neu.map, neu_nums=neu_nums, tor=tor))
  }
}

############################################################################
if ( epochs>0) {
  par(mfrow = c(1,1))
  plot(prev.diff, type="l", xlab="epoch", ylab="QDM", ylim=c(0,threshold))
  plot(QDMi, type="l", xlab="epoch", lwd=2, ylab="QDM")
}

cat("\nseed =", seed, "\n\n")
