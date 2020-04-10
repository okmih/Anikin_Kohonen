# ========== Function my_panel() ==========
my_panel = function(x, y, neu, neu.map, neu_nums, tor, ...) {
  panel.xyplot(x, y, ...)
  # --------------------------------
  lpoints(neu[,1:2], pch=16, col = "black")
  # --------------------------------
  for (i in 1:(mg*ng)) {
    if (neu_nums) ltext(neu[i,1], neu[i,2], row.names(neu)[i], pos=4, 
                        cex = 0.7, col="black")
  }
  if (mg==1) {
    # =============== SOM (mg=1) ===============
    for (j in 1:mg) {
      for (i in 1:(ng-1)) {
        if (neu.map[2*i,2*j-1]=="+") {
          lsegments(neu[neu.map[2*i-1,2*j-1],1], neu[neu.map[2*i-1,2*j-1],2],
                    neu[neu.map[2*i+1,2*j-1],1], neu[neu.map[2*i+1,2*j-1],2],
                    col = "black", lwd=2)
        }
      }
    }
    if (tor=="tor"|tor=="tor.h") {
      for (j in 1:mg) {
        for (i in 1:ng) {
          if (neu.map[2*i,2*j-1]=="+") {
            if (i==ng) {
              lsegments(neu[neu.map[2*i-1,2*j-1],1], neu[neu.map[2*i-1,2*j-1],2],
                        neu[neu.map[1,2*j-1],1], neu[neu.map[1,2*j-1],2],
                        col = "black", lwd=2)
            } else {
              lsegments(neu[neu.map[2*i-1,2*j-1],1], neu[neu.map[2*i-1,2*j-1],2],
                        neu[neu.map[2*i+1,2*j-1],1], neu[neu.map[2*i+1,2*j-1],2],
                        col = "black", lwd=2)
            }
          }
        }
      }
    }  
  } else if (ng==1) {
    # =============== SOM (ng=1) ===============
    for (i in 1:ng) {
      for (j in 1:(mg-1)) {
        if (neu.map[2*i-1,2*j]=="+") {
          lsegments(neu[neu.map[2*i-1,2*j-1],1] ,neu[neu.map[2*i-1,2*j-1],2],
                    neu[neu.map[2*i-1,2*j+1],1], neu[neu.map[2*i-1,2*j+1],2],
                    col = "black", lwd=2)
        }
      }
    }
    if (tor=="tor"|tor=="tor.h") {
      for (i in 1:ng) {
        for (j in 1:mg) {
          if (neu.map[2*i-1,2*j]=="+") {
            if (j==mg) {
              lsegments(neu[neu.map[2*i-1,2*j-1],1] ,neu[neu.map[2*i-1,2*j-1],2],
                        neu[neu.map[2*i-1,1],1], neu[neu.map[2*i-1,1],2],
                        col = "black", lwd=2)
            } else {
              lsegments(neu[neu.map[2*i-1,2*j-1],1] ,neu[neu.map[2*i-1,2*j-1],2],
                        neu[neu.map[2*i-1,2*j+1],1], neu[neu.map[2*i-1,2*j+1],2],
                        col = "black", lwd=2)
            }
          }
        }
      }
    }
  } else {
    # =============== 2D SOM ===============
    for (i in 1:ng) {
      for (j in 1:(mg-1)) {
        if (neu.map[2*i-1,2*j]=="+") {
          lsegments(neu[neu.map[2*i-1,2*j-1],1] ,neu[neu.map[2*i-1,2*j-1],2],
                    neu[neu.map[2*i-1,2*j+1],1], neu[neu.map[2*i-1,2*j+1],2],
                    col = "black", lwd=2)
        }
      }
    }
    for (j in 1:mg) {
      for (i in 1:(ng-1)) {
        if (neu.map[2*i,2*j-1]=="+") {
          lsegments(neu[neu.map[2*i-1,2*j-1],1], neu[neu.map[2*i-1,2*j-1],2],
                    neu[neu.map[2*i+1,2*j-1],1], neu[neu.map[2*i+1,2*j-1],2],
                    col = "black", lwd=2)
        }
      }
    }
    if (tor=="tor"|tor=="tor.h") {
      for (i in 1:ng) {
        for (j in 1:mg) {
          if (neu.map[2*i-1,2*j]=="+") {
            if (j==mg) {
              lsegments(neu[neu.map[2*i-1,2*j-1],1] ,neu[neu.map[2*i-1,2*j-1],2],
                        neu[neu.map[2*i-1,1],1], neu[neu.map[2*i-1,1],2],
                        col = "black", lwd=2)
            } else {
              lsegments(neu[neu.map[2*i-1,2*j-1],1] ,neu[neu.map[2*i-1,2*j-1],2],
                        neu[neu.map[2*i-1,2*j+1],1], neu[neu.map[2*i-1,2*j+1],2],
                        col = "black", lwd=2)
            }
          }
        }
      }
      for (j in 1:mg) {
        for (i in 1:ng) {
          if (neu.map[2*i,2*j-1]=="+") {
            if (i==ng) {
              lsegments(neu[neu.map[2*i-1,2*j-1],1], neu[neu.map[2*i-1,2*j-1],2],
                        neu[neu.map[1,2*j-1],1], neu[neu.map[1,2*j-1],2],
                        col = "black", lwd=2)
            } else {
              lsegments(neu[neu.map[2*i-1,2*j-1],1], neu[neu.map[2*i-1,2*j-1],2],
                        neu[neu.map[2*i+1,2*j-1],1], neu[neu.map[2*i+1,2*j-1],2],
                        col = "black", lwd=2)
            }
          }
        }
      }
    }
  }
}

############################################################################
# ========== Function neu.winners.neib8() ========== 
neu.winners.neib8 <- function(x, y) {
  mg <- floor(ncol(y)/2)+1
  ng <- floor(nrow(y)/2)+1
  N <- mg*ng
  if (x%%mg==0) {
    k <- x%%mg+mg
    kk <- ng-floor(x/mg)+1
  } else {
    k <- x%%mg
    kk <- ng-floor(x/mg)
  }
  #  cat("kk =", kk, ", k =", k, ", mg =", mg, ", ng =", ng, "\n")
  
  neu.winners <- numeric(N)
  neu.winners[x] <- 1
  if (mg==1) {
    # ::::::::::::::::::: SOM (mg==1) ::::::::::::::::::: 
    if (kk==1) {
      if (y[2*ng-2,2*k-1]=="+")
        neu.winners[as.integer(y[3,2*k-1])] <- 0.99
    } else if (kk==ng) {
      if (y[2*ng-2,2*k-1]=="+")
        neu.winners[as.integer(y[2*ng-3,2*k-1])] <- 0.99
    } else {
      if (y[2*kk-2,2*k-1]=="+")
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
      if (y[2*kk,2*k-1]=="+")
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
    }
  } else if (ng==1) {
    # ::::::::::::::::::: SOM (ng==1) ::::::::::::::::::: 
    if (k!=mg) {
      if (y[2*kk-1,2*k]=="+")
        neu.winners[as.integer(y[2*kk-1,2*k+1])] <- 0.99
    }
    if (k!=1) {
      if (y[2*kk-1,2*k-2]=="+")
        neu.winners[as.integer(y[2*kk-1,2*k-3])] <- 0.99
    }
  } else {
    # ::::::::::::::::::: SOM (mg>1, ng>1) :::::::::::::::::::
    if (k!=mg) {
      if (y[2*kk-1,2*k]=="+")
        neu.winners[as.integer(y[2*kk-1,2*k+1])] <- 0.99
    } 
    if (k!=1) {
      if (y[2*kk-1,2*k-2]=="+")
        neu.winners[as.integer(y[2*kk-1,2*k-3])] <- 0.99
    }
    if (x<=mg) {
      if (y[2*kk-2,2*k-1]=="+") {
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
      }
      if (k!=mg) {
        if (y[2*kk-2,2*k]=="+")
          neu.winners[as.integer(y[2*kk-3,2*k+1])] <- 0.99
      }  
      if (k!=1) {
        if (y[2*kk-2,2*k-2]=="+")
          neu.winners[as.integer(y[2*kk-3,2*k-3])] <- 0.99
      }
    } else if (x>N-mg) {
       if (y[2*kk,2*k-1]=="+") {
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
      }  
      if (k!=mg) {
        if (y[2*kk,2*k]=="+")
          neu.winners[as.integer(y[2*kk+1,2*k+1])] <- 0.99
      }  
      if (k!=1) {
        if (y[2*kk,2*k-2]=="+")
          neu.winners[as.integer(y[2*kk+1,2*k-3])] <- 0.99
      }
    } else {
      if (y[2*kk-2,2*k-1]=="+")
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
      if (y[2*kk,2*k-1]=="+") 
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
      if (k!=mg) {
        if (y[2*kk-2,2*k]=="+") 
          neu.winners[as.integer(y[2*kk-3,2*k+1])] <- 0.99
      }  
      if (k!=1) {
        if (y[2*kk-2,2*k-2]=="+") 
          neu.winners[as.integer(y[2*kk-3,2*k-3])] <- 0.99
      }
      if (k!=mg) {
        if (y[2*kk,2*k]=="+") 
          neu.winners[as.integer(y[2*kk+1,2*k+1])] <- 0.99
      } 
      if (k!=1) {
        if (y[2*kk,2*k-2]=="+") 
          neu.winners[as.integer(y[2*kk+1,2*k-3])] <- 0.99
      }
    }  
  }    
  return(neu.winners)
}    

############################################################################
# ========== Function neu.winners.neib8.tor.h() ========== 
neu.winners.neib8.tor.h <- function(x, y) {
  mg <- floor(ncol(y)/2)
  ng <- floor(nrow(y)/2)
  N <- mg*ng
  if (x%%mg==0) {
    k <- x%%mg+mg
    kk <- ng-floor(x/mg)+1
  } else {
    k <- x%%mg
    kk <- ng-floor(x/mg)
  }
  #  cat("kk =", kk, ", k =", k, ", mg =", mg, ", ng =", ng, "\n")
  
  neu.winners <- numeric(N)
  neu.winners[x] <- 1
  if (mg==1) {
    # ::::::::::::::::::: SOM (mg==1) ::::::::::::::::::: 
    if (kk==1) {
      if (y[2*ng-2,2*k-1]=="+") 
        neu.winners[as.integer(y[3,2*k-1])] <- 0.99
      if (y[2*ng,2*k-1]=="+") 
        neu.winners[as.integer(y[2*ng-1,1])] <- 0.99
    } else if (kk==ng) {
       if (y[2*ng-2,2*k-1]=="+")
        neu.winners[as.integer(y[2*ng-3,2*k-1])] <- 0.99
      if (y[2*ng,2*k-1]=="+")
        neu.winners[as.integer(y[1,2*k-1])] <- 0.99
    } else {
      if (y[2*kk-2,2*k-1]=="+")
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
      if (y[2*kk,2*k-1]=="+")
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
    }
  } else if (ng==1) {
    # ::::::::::::::::::: SOM (ng==1) ::::::::::::::::::: 
    if (k!=mg) {
      if (y[2*kk-1,2*k]=="+") 
        neu.winners[as.integer(y[2*kk-1,2*k+1])] <- 0.99
    } else {
      if (y[2*kk-1,2*mg]=="+") 
        neu.winners[as.integer(y[2*kk-1,1])] <- 0.99
    }
    if (k!=1) {
      if (y[2*kk-1,2*k-2]=="+")
        neu.winners[as.integer(y[2*kk-1,2*k-3])] <- 0.99
    } else {
      if (y[2*kk-1,2*mg]=="+")
        neu.winners[as.integer(y[2*kk-1,2*mg-1])] <- 0.99
    }
  } else {
    # ::::::::::::::::::: SOM (mg>1, ng>1) :::::::::::::::::::
    if (k!=mg) {
      if (y[2*kk-1,2*k]=="+") 
        neu.winners[as.integer(y[2*kk-1,2*k+1])] <- 0.99
    } else { 
      neu.winners[as.integer(y[2*kk-1,1])] <- 0.99
    } 
    if (k!=1) {
      if (y[2*kk-1,2*k-2]=="+")
        neu.winners[as.integer(y[2*kk-1,2*k-3])] <- 0.99
    } else {
      neu.winners[as.integer(y[2*kk-1,2*mg-1])] <- 0.99
    }
    if (x<=mg) {
      if (y[2*kk-2,2*k-1]=="+") {
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
      }
      if (k!=mg) { 
        if (y[2*kk-2,2*k]=="+") 
          neu.winners[as.integer(y[2*kk-3,2*k+1])] <- 0.99
      } else {
        if (y[2*kk-2,2*k]=="+")
          neu.winners[as.integer(y[2*kk-3,1])] <- 0.99
      }  
      if (k!=1) { 
        if (y[2*kk-2,2*k-2]=="+") 
          neu.winners[as.integer(y[2*kk-3,2*k-3])] <- 0.99
      } else {
        if (y[2*kk-2,2*mg]=="+") 
          neu.winners[as.integer(y[2*kk-3,2*mg-1])] <- 0.99
      }
    } else if (x>N-mg) {
      if (y[2*kk,2*k-1]=="+") {
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
      }  
      if (k!=mg) { 
        if (y[2*kk,2*k]=="+") 
          neu.winners[as.integer(y[2*kk+1,2*k+1])] <- 0.99
      } else {
        if (y[2*kk,2*mg]=="+") 
          neu.winners[as.integer(y[2*kk+1,1])] <- 0.99
      }  
      if (k!=1) { 
        if (y[2*kk,2*k-2]=="+") 
          neu.winners[as.integer(y[2*kk+1,2*k-3])] <- 0.99
      } else {
        if (y[2*kk,2*mg]=="+") 
          neu.winners[as.integer(y[2*kk+1,2*mg-1])] <- 0.99
      }
    } else {
      if (y[2*kk-2,2*k-1]=="+") 
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
      if (y[2*kk,2*k-1]=="+") 
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
      if (k!=mg) { 
        if (y[2*kk-2,2*k]=="+") 
          neu.winners[as.integer(y[2*kk-3,2*k+1])] <- 0.99
      } else {
        if (y[2*kk-2,2*mg]=="+") 
          neu.winners[as.integer(y[2*kk-3,1])] <- 0.99
      }  
      if (k!=1) { 
        if (y[2*kk-2,2*k-2]=="+") 
          neu.winners[as.integer(y[2*kk-3,2*k-3])] <- 0.99
      } else {
        if (y[2*kk-2,2*mg]=="+") 
          neu.winners[as.integer(y[2*kk-3,2*mg-1])] <- 0.99
      }
      if (k!=mg) { 
        if (y[2*kk,2*k]=="+") 
          neu.winners[as.integer(y[2*kk+1,2*k+1])] <- 0.99
      } else {
        if (y[2*kk,2*mg]=="+") 
          neu.winners[as.integer(y[2*kk+1,1])] <- 0.99
      } 
      if (k!=1) { 
        if (y[2*kk,2*k-2]=="+") 
          neu.winners[as.integer(y[2*kk+1,2*k-3])] <- 0.99
      } else {
        if (y[2*kk,2*mg]=="+") 
          neu.winners[as.integer(y[2*kk+1,2*mg-1])] <- 0.99
      }
      
    }  
  }    
  return(neu.winners)
}    

############################################################################
# ========== Function neu.winners.neib8.tor() ========== 
neu.winners.neib8.tor <- function(x, y) {
  mg <- floor(ncol(y)/2)
  ng <- floor(nrow(y)/2)
  N <- mg*ng
  if (x%%mg==0) {
    k <- x%%mg+mg
    kk <- n-floor(x/mg)+1
  } else {
    k <- x%%mg
    kk <- n-floor(x/mg)
  }
  #  cat("kk =", kk, ", k =", k, ", mg =", mg, ", ng =", ng, "\n")
  
  neu.winners <- numeric(N)
  neu.winners[x] <- 1
  if (mg==1) {
    # ::::::::::::::::::: SOM (mg==1) ::::::::::::::::::: 
    if (kk==1) {
      if (y[2*ng-2,2*k-1]=="+")
        neu.winners[as.integer(y[3,2*k-1])] <- 0.99
      if (y[2*ng,2*k-1]=="+") 
        neu.winners[as.integer(y[2*ng-1,1])] <- 0.99
    } else if (kk==ng) {
      if (y[2*ng-2,2*k-1]=="+") 
        neu.winners[as.integer(y[2*ng-3,2*k-1])] <- 0.99
      if (y[2*ng,2*k-1]=="+") 
        neu.winners[as.integer(y[1,2*k-1])] <- 0.99
    } else {
      if (y[2*kk-2,2*k-1]=="+")
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
      if (y[2*kk,2*k-1]=="+")  
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
    }
  } else if (ng==1) {
    # ::::::::::::::::::: SOM (ng==1) ::::::::::::::::::: 
    if (k!=mg) { 
      if (y[2*kk-1,2*k]=="+") 
        neu.winners[as.integer(y[2*kk-1,2*k+1])] <- 0.99
    } else {
      if (y[2*kk-1,2*mg]=="+")
        neu.winners[as.integer(y[2*kk-1,1])] <- 0.99
    }
    if (k!=1) {
      if (y[2*kk-1,2*k-2]=="+") 
        neu.winners[as.integer(y[2*kk-1,2*k-3])] <- 0.99
    } else {
      if (y[2*kk-1,2*mg]=="+")
        neu.winners[as.integer(y[2*kk-1,2*mg-1])] <- 0.99
    }
  } else {
    # ::::::::::::::::::: SOM (mg>1, ng>1) :::::::::::::::::::
    if (k!=mg) { 
      if (y[2*kk-1,2*k]=="+") 
        neu.winners[as.integer(y[2*kk-1,2*k+1])] <- 0.99
    } else { 
      neu.winners[as.integer(y[2*kk-1,1])] <- 0.99
    } 
    if (k!=1) { 
      if (y[2*kk-1,2*k-2]=="+") 
        neu.winners[as.integer(y[2*kk-1,2*k-3])] <- 0.99
    } else {
      neu.winners[as.integer(y[2*kk-1,2*mg-1])] <- 0.99
    }
    if (x<=mg) {
      if (y[2*kk-2,2*k-1]=="+") { 
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
      }
      if (k!=mg) { 
        if (y[2*kk-2,2*k]=="+") 
          neu.winners[as.integer(y[2*kk-3,2*k+1])] <- 0.99
      } else {
        if (y[2*kk-2,2*k]=="+") 
          neu.winners[as.integer(y[2*kk-3,1])] <- 0.99
      }  
      if (k!=1) { 
        if (y[2*kk-2,2*k-2]=="+") 
          neu.winners[as.integer(y[2*kk-3,2*k-3])] <- 0.99
      } else {
        if (y[2*kk-2,2*mg]=="+") 
          neu.winners[as.integer(y[2*kk-3,2*mg-1])] <- 0.99
      }
      if (y[2*kk,2*k-1]=="+") { 
        neu.winners[as.integer(y[1,2*k-1])] <- 0.99
      }  
      if (k!=mg) { 
        if (y[2*kk,2*k]=="+") 
          neu.winners[as.integer(y[1,2*k+1])] <- 0.99
      } else {
        if (y[2*kk,1]=="+") 
          neu.winners[as.integer(y[1,1])] <- 0.99
      } 
      if (k!=1) { 
        if (y[2*kk,2*k-2]=="+") 
          neu.winners[as.integer(y[1,2*k-3])] <- 0.99
      } else {
        if (y[2*kk,1]=="+") 
          neu.winners[as.integer(y[1,2*mg-1])] <- 0.99
      } 
    } else if (x>N-mg) {
      if (y[2*kk,2*k-1]=="+") { 
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
      }  
      if (k!=mg) { 
        if (y[2*kk,2*k]=="+")
          neu.winners[as.integer(y[2*kk+1,2*k+1])] <- 0.99
      } else {
        if (y[2*kk,2*mg]=="+") 
          neu.winners[as.integer(y[2*kk+1,1])] <- 0.99
      }  
      if (k!=1) { 
        if (y[2*kk,2*k-2]=="+") 
          neu.winners[as.integer(y[2*kk+1,2*k-3])] <- 0.99
      } else {
        if (y[2*kk,2*mg]=="+") 
          neu.winners[as.integer(y[2*kk+1,2*mg-1])] <- 0.99
      }
      if (y[2*ng,2*k-1]=="+") { 
        neu.winners[as.integer(y[2*ng-1,2*k-1])] <- 0.99
      }
      if (k!=mg) { 
        if (y[2*ng,2*k]=="+") 
          neu.winners[as.integer(y[2*ng-1,2*k+1])] <- 0.99
      } else {
        if (y[2*ng,2*mg]=="+") 
          neu.winners[as.integer(y[2*ng-1,1])] <- 0.99 
      } 
      if (k!=1) { 
        if (y[2*ng,2*k-2]=="+") 
          neu.winners[as.integer(y[2*ng-1,2*k-3])] <- 0.99
      } else {
        if (y[2*ng,2*mg]=="+") 
          neu.winners[as.integer(y[2*ng-1,2*mg-1])] <- 0.99
      }
    } else {
      if (y[2*kk-2,2*k-1]=="+") 
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
      if (y[2*kk,2*k-1]=="+") 
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
      if (k!=mg) { 
        if (y[2*kk-2,2*k]=="+") 
          neu.winners[as.integer(y[2*kk-3,2*k+1])] <- 0.99
      } else {
        if (y[2*kk-2,2*mg]=="+")
          neu.winners[as.integer(y[2*kk-3,1])] <- 0.99
      }  
      if (k!=1) { 
        if (y[2*kk-2,2*k-2]=="+")
          neu.winners[as.integer(y[2*kk-3,2*k-3])] <- 0.99
      } else {
        if (y[2*kk-2,2*mg]=="+") 
          neu.winners[as.integer(y[2*kk-3,2*mg-1])] <- 0.99
      }
      if (k!=mg) { 
        if (y[2*kk,2*k]=="+") 
          neu.winners[as.integer(y[2*kk+1,2*k+1])] <- 0.99
      } else {
        if (y[2*kk,2*mg]=="+") 
          neu.winners[as.integer(y[2*kk+1,1])] <- 0.99
      } 
      if (k!=1) { 
        if (y[2*kk,2*k-2]=="+") 
          neu.winners[as.integer(y[2*kk+1,2*k-3])] <- 0.99
      } else {
        if (y[2*kk,2*mg]=="+") 
          neu.winners[as.integer(y[2*kk+1,2*mg-1])] <- 0.99
      }
    }  
  }    
  return(neu.winners)
}    

############################################################################
# ========== Function neu.winners.neib4() ========== 
neu.winners.neib4 <- function(x, y) {
  mg <- floor(ncol(y)/2.)+1
  ng <- floor(nrow(y)/2.)+1
  N <- mg*ng
  if (x%%mg==0) {
    k <- x%%mg+mg
    kk <- ng-floor(x/mg)+1
  } else {
    k <- x%%mg
    kk <- ng-floor(x/mg)
  }
  neu.winners <- numeric(N)
  neu.winners[x] <- 1
  if (mg==1) {
    # :::::::::::::::::::: SOM (mg==1) :::::::::::::::::::: 
    if (x<=mg) {
      if (y[2*kk-2,2*k-1]=="+") 
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
    } else if (x>N-mg) { 
      if (y[2*kk,2*k-1]=="+") { 
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
      }  
    } else { 
      if (y[2*kk-2,2*k-1]=="+") 
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
      if (y[2*kk,2*k-1]=="+") { 
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
      }  
    }
  } else if (ng==1) {
    # :::::::::::::::::::: SOM (ng==1) :::::::::::::::::::: 
    if (k!=mg) { 
      if (y[2*kk-1,2*k]=="+") 
        neu.winners[as.integer(y[2*kk-1,2*k+1])] <- 0.99
    }  
    if (k!=1) { 
      if (y[2*kk-1,2*k-2]=="+") 
        neu.winners[as.integer(y[2*kk-1,2*k-3])] <- 0.99
    }
  } else {
    # :::::::::::::::::::: SOM (mg>1, ng>1) ::::::::::::::::::::
    if (k!=mg) { 
      if (y[2*kk-1,2*k]=="+") 
        neu.winners[as.integer(y[2*kk-1,2*k+1])] <- 0.99
    }  
    if (k!=1) { 
      if (y[2*kk-1,2*k-2]=="+")  
        neu.winners[as.integer(y[2*kk-1,2*k-3])] <- 0.99
    }
    #--------------------------------------------------------
    if (x<=mg) { 
      if (y[2*kk-2,2*k-1]=="+") {
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
      }
    } else if (x>N-mg) { 
      if (y[2*kk,2*k-1]=="+") {
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
      }  
    } else { # средние строки карты SOM
      if (y[2*kk-2,2*k-1]=="+")
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
      if (y[2*kk,2*k-1]=="+") { 
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
      }
    }
  }
  return(neu.winners)
}

############################################################################
# ========== Function neu.winners.neib4.tor.h() ========== 
neu.winners.neib4.tor.h <- function(x, y) {
  mg <- floor(ncol(y)/2)
  ng <- floor(nrow(y)/2)
  N <- mg*ng
  if (x%%mg==0) {
    k <- x%%mg+mg
    kk <- ng-floor(x/mg)+1
  } else {
    k <- x%%mg
    kk <- ng-floor(x/mg)
  }
  #  cat("kk =", kk, ", k =", k, ", mg =", mg, ", ng =", ng, "\n")
  
  neu.winners <- numeric(N)
  neu.winners[x] <- 1
  if (mg==1) {
    # ::::::::::::::::::: SOM (mg==1) ::::::::::::::::::: 
    if (kk==1) {
      if (y[2*ng-2,2*k-1]=="+")
        neu.winners[as.integer(y[3,2*k-1])] <- 0.99
      if (y[2*ng,2*k-1]=="+") 
        neu.winners[as.integer(y[2*ng-1,1])] <- 0.99
    } else if (kk==ng) {
      if (y[2*ng-2,2*k-1]=="+") 
        neu.winners[as.integer(y[2*ng-3,2*k-1])] <- 0.99
      if (y[2*ng,2*k-1]=="+")  
        neu.winners[as.integer(y[1,2*k-1])] <- 0.99
    } else {
      if (y[2*kk-2,2*k-1]=="+") 
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
      if (y[2*kk,2*k-1]=="+") 
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
    }
  } else if (ng==1) {
    # ::::::::::::::::::: SOM (ng==1) ::::::::::::::::::: 
    if (k!=mg) { 
      if (y[2*kk-1,2*k]=="+") 
        neu.winners[as.integer(y[2*kk-1,2*k+1])] <- 0.99
    } else {
      if (y[2*kk-1,2*mg]=="+") 
        neu.winners[as.integer(y[2*kk-1,1])] <- 0.99
    }
    if (k!=1) {
      if (y[2*kk-1,2*k-2]=="+")
        neu.winners[as.integer(y[2*kk-1,2*k-3])] <- 0.99
    } else {
      if (y[2*kk-1,2*mg]=="+") 
        neu.winners[as.integer(y[2*kk-1,2*mg-1])] <- 0.99
    }
  } else {
    # ::::::::::::::::::: SOM (mg>1, ng>1) :::::::::::::::::::
    if (k!=mg) { 
      if (y[2*kk-1,2*k]=="+") 
        neu.winners[as.integer(y[2*kk-1,2*k+1])] <- 0.99
    } else { # сшивка справа
      neu.winners[as.integer(y[2*kk-1,1])] <- 0.99
    } 
    if (k!=1) {
      if (y[2*kk-1,2*k-2]=="+")
        neu.winners[as.integer(y[2*kk-1,2*k-3])] <- 0.99
    } else { # сшивка слева
      neu.winners[as.integer(y[2*kk-1,2*mg-1])] <- 0.99
    }
    if (x<=mg) {
      if (y[2*kk-2,2*k-1]=="+") { 
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
      }
    } else if (x>N-mg) {
      if (y[2*kk,2*k-1]=="+") { 
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
      }  
    } else {
       if (y[2*kk-2,2*k-1]=="+") 
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
      if (y[2*kk,2*k-1]=="+") 
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
    }  
  }    
  return(neu.winners)
}    

############################################################################
# ========== Function neu.winners.neib4.tor() ========== 
neu.winners.neib4.tor <- function(x, y) {
  mg <- floor(ncol(y)/2)
  ng <- floor(nrow(y)/2)
  N <- mg*ng
  if (x%%mg==0) {
    k <- x%%mg+mg
    kk <- ng-floor(x/mg)+1
  } else {
    k <- x%%mg
    kk <- ng-floor(x/mg)
  }
  #  cat("kk =", kk, ", k =", k, ", mg =", mg, ", ng =", ng, "\n")
  
  neu.winners <- numeric(N)
  neu.winners[x] <- 1
  if (mg==1) {
    # ::::::::::::::::::: SOM (mg==1) ::::::::::::::::::: 
    if (kk==1) {
      if (y[2*ng-2,2*k-1]=="+") 
        neu.winners[as.integer(y[3,2*k-1])] <- 0.99
      if (y[2*ng,2*k-1]=="+") 
        neu.winners[as.integer(y[2*ng-1,1])] <- 0.99
    } else if (kk==ng) {
      if (y[2*ng-2,2*k-1]=="+") 
        neu.winners[as.integer(y[2*ng-3,2*k-1])] <- 0.99
      if (y[2*ng,2*k-1]=="+")
        neu.winners[as.integer(y[1,2*k-1])] <- 0.99
    } else {
      if (y[2*kk-2,2*k-1]=="+")
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
      if (y[2*kk,2*k-1]=="+") 
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
    }
  } else if (ng==1) {
    # ::::::::::::::::::: SOM (ng==1) ::::::::::::::::::: 
    if (k!=mg) { 
      if (y[2*kk-1,2*k]=="+") 
        neu.winners[as.integer(y[2*kk-1,2*k+1])] <- 0.99
    } else {
      if (y[2*kk-1,2*mg]=="+")
        neu.winners[as.integer(y[2*kk-1,1])] <- 0.99
    }
    if (k!=1) {
      if (y[2*kk-1,2*k-2]=="+")
        neu.winners[as.integer(y[2*kk-1,2*k-3])] <- 0.99
    } else {
      if (y[2*kk-1,2*mg]=="+") 
        neu.winners[as.integer(y[2*kk-1,2*mg-1])] <- 0.99
    }
  } else {
    # ::::::::::::::::::: SOM (mg>1, ng>1) :::::::::::::::::::
    if (k!=mg) { 
      if (y[2*kk-1,2*k]=="+") 
        neu.winners[as.integer(y[2*kk-1,2*k+1])] <- 0.99
    } else { 
      neu.winners[as.integer(y[2*kk-1,1])] <- 0.99
    } 
    if (k!=1) {
      if (y[2*kk-1,2*k-2]=="+") 
        neu.winners[as.integer(y[2*kk-1,2*k-3])] <- 0.99
    } else { 
      neu.winners[as.integer(y[2*kk-1,2*mg-1])] <- 0.99
    }
    if (x<=mg) {
      if (y[2*kk-2,2*k-1]=="+") { 
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
      }
      if (y[2*kk,2*k-1]=="+") { 
        neu.winners[as.integer(y[1,2*k-1])] <- 0.99
      }  
    } else if (x>N-mg) {
      if (y[2*kk,2*k-1]=="+") { 
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
      }  
       if (y[2*ng,2*k-1]=="+") { 
        neu.winners[as.integer(y[2*ng-1,2*k-1])] <- 0.99
      }
    } else {
      if (y[2*kk-2,2*k-1]=="+") 
        neu.winners[as.integer(y[2*kk-3,2*k-1])] <- 0.99
      if (y[2*kk,2*k-1]=="+") 
        neu.winners[as.integer(y[2*kk+1,2*k-1])] <- 0.99
    }  
  }    
  return(neu.winners)
}    

############################################################################
# ========== Функция coolBlueHotRed() ========== 
#colors function for the charts
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

############################################################################
# ======================= QDM =======================
calcQDM <- function(df, codes) {
  nrows <- dim(df)[1]
  ncols <- dim(df)[2]
  N <- dim(codes)[1]
  points <- seq(0,1,1/N)
  QDMj <- numeric(ncols)
  for (indx in 1:ncols) {
    QDMj[indx] <- sqrt(sum((quantile(codes[,indx],points)-quantile(df[,indx],points))^2)/N)
  }
  QDM <- sqrt(sum(QDMj*QDMj)/ncols)
  return(list(QDM, QDMj))
}


