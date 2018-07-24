setwd("C:/Users/Ajay khushu/Desktop/Purdue/Supply Chain/K & N Project/Previous rates")
library(spatstat.utils)
efcl <- read.csv("exportfcl(old_new).csv")
elcl <- read.csv("exportlcl(old_new).csv")
ifcl <- read.csv("importfcl(old_new).csv")
ilcl <- read.csv("importlcl(old_new).csv")

mode <- readline(prompt = "Hello, please enter E for export and I for import:")
cl <- readline(prompt = "Alright, and the type of transportation is fcl or lcl:")

mode <- tolower(mode)
cl <- tolower(cl)

if (mode == "e"){
  
  if (cl == "fcl"){
    locate <- efcl$X
    
    print(locate)
    
    i <- readline(prompt = "Please select the location :")
    i <- as.integer(i)
    j <- readline(prompt = "Enter 1 if the container must be 20GP or 2 if it must be 40 GP/HC :")
    j <- as.integer(j)
    k <- readline(prompt = "Awesome, now enter the gross weight of the cargo in ton :")
    k <- as.integer(k)
    
    if(j == 1){
      
      repeat{
        
        if(inside.range(k,c(1,7))){
          selection <- efcl[i, c(2:7)]
          break
        } else if(inside.range(k,c(7.1,14))){
          selection <- efcl[i, c(8:13)]
          break
        } else if(inside.range(k,c(14.1,21))){
          selection <- efcl[i, c(14:19)]
          break
        } else if(inside.range(k,c(21.1,25))){
          selection <- efcl[i, c(20:25)]
          break
        } else if(k > 25){
          k<- readline(prompt = "Try again with a value between 1 to 25 ton :")
          k <- as.integer(k)
        }
      }
      
    } else{
      repeat{
        
        if(inside.range(k,c(1,15))){
          selection <- efcl[i, c(26:31)]
          break
        } else if(inside.range(k,c(15.1,20))){
          selection <- efcl[i, c(32:37)]
          break
        } else if(inside.range(k,c(20.1,24))){
          selection <- efcl[i, c(38:43)]
          break
        } else if(inside.range(k,c(24.1,27))){
          selection <- efcl[i, c(44:49)]
          break
        } else if(k > 27){
          k<- readline(prompt = "Try again with a value between 1 to 25 ton :")
          k <- as.integer(k)
        }
      }
      
    }
  } else{
    
    locate <- elcl$X
    
    print(locate)
    
    i <- readline(prompt = "Please select the location :")
    i <- as.integer(i)
    
    k <- readline(prompt = "Awesome, now enter the gross weight of the cargo in Metric Tons :")
    k <- as.integer(k)
    
    
    repeat{
      
      if(inside.range(k,c(0,0.5))){
        selection <- elcl[i, c(2:5)]
        
        break
      } else if(inside.range(k,c(0.56,1))){
        selection <- elcl[i, c(6:11)]
        
        break
      } else if(inside.range(k,c(1.1,2))){
        selection <- elcl[i, c(12:17)]
        
        break
      } else if(inside.range(k,c(2.1,3))){
        selection <- elcl[i, c(18:21)]
        
        break
      } else if(inside.range(k,c(3.1,4))){
        selection <- elcl[i, c(22:27)]
        break
      } 
      else if(inside.range(k,c(4.1,7))){
        q <- readline(prompt = "Press 1 for 1109 LPT Load and 2 for 32' CBT truck :")
        q <- as.integer(q)
        if(q == 1){
          selection <- elcl[i, c(28:33)]
          break
        } else{
          selection <- elcl[i, c(60:63)]
          break
        }
      }
      else if(inside.range(k,c(7.1,7.5))){
        selection <- elcl[i, c(28:33)]
        break
      }
      else if(inside.range(k,c(7.51,9))){
        q <- readline(prompt = "Press 1 for Normal truck and 2 for Open truck :")
        q <- as.integer(q)
        if(q == 1){
          selection <- elcl[i, c(34:37)]
          break
        } else{
          selection <- elcl[i, c(38:41)]
          break
        }
      }
      else if(inside.range(k,c(9.1,15))){
        selection <- elcl[i, c(64:67)]
        break
      }
      else if(inside.range(k,c(15.1,16))){
        selection <- elcl[i, c(42:45)]
        break
      } else if(inside.range(k,c(16.1,20))){
        selection <- elcl[i, c(68:71)]
        break
      } else if(inside.range(k,c(20.1,21))){
        q <- readline(prompt = "Press 1 for TAURUS, 2 for 24 feet truck and 3 for 20 feet SIDE OPEN truck :")
        q <- as.integer(q)
        if(q == 1){
          selection <- elcl[i, c(46:49)]
          break
        } else if(q == 2){
          selection <- elcl[i, c(50:53)]
          break
        }
        else{
          selection <- elcl[i, c(54:59)]
          break
        }
      }         
      else if(k > 21){
        k<- readline(prompt = "Try again with a value between 1 to 21 MT :")
        k <- as.integer(k)
      }
    }
    
  }
} else {
  
  if (cl == "fcl"){
    locate <- ifcl$X
    
    print(locate)
    
    i <- readline(prompt = "Please select the location :")
    i <- as.integer(i)
    j <- readline(prompt = "Enter 1 if the container must be 20GP or 2 if it must be 40 GP/HC :")
    j <- as.integer(j)
    k <- readline(prompt = "Awesome, now enter the gross weight of the cargo in ton :")
    k <- as.integer(k)
    
    if(j == 1){
      repeat{
        
        if(inside.range(k,c(1,7))){
          selection <- ifcl[i, c(2:7)]
          break
        }
        else if(inside.range(k,c(7.1,14))){
          selection <- ifcl[i, c(8:13)]
          break
        } else if(inside.range(k,c(14.1,21))){
          selection <- ifcl[i, c(14:19)]
          break
        } else if(inside.range(k,c(21.1,25))){
          selection <- ifcl[i, c(20:25)]
          break
        } else if(k > 25){
          k<- readline(prompt = "Try again with a value between 1 to 25 ton :")
          k <- as.integer(k)
        }
      }
    
    } else{
      repeat{
        
        if(inside.range(k,c(1,15))){
          selection <- ifcl[i, c(26:31)]
          break
        } else if(inside.range(k,c(15.1,20))){
          selection <- ifcl[i, c(32:37)]
          break
        } else if(inside.range(k,c(20.1,24))){
          selection <- ifcl[i, c(38:43)]
          break
        } else if(inside.range(k,c(24.1,27))){
          selection <- ifcl[i, c(44:49)]
          break
        } else if(k > 27){
          k<- readline(prompt = "Try again with a value between 1 to 25 ton :")
          k <- as.integer(k)
        }
      }
    
    }
  } else{
    
    locate <- ilcl$X
    
    print(locate)
    
    i <- readline(prompt = "Please select the location :")
    i <- as.integer(i)
    
    k <- readline(prompt = "Awesome, now enter the gross weight of the cargo in Metric Tons :")
    k <- as.integer(k)
    
    
    repeat{
      
      if(inside.range(k,c(0,0.5))){
        selection <- ilcl[i, c(2:5)]
        break
      } else if(inside.range(k,c(0.51,1))){
        selection <- ilcl[i, c(6:11)]
        break
      } else if(inside.range(k,c(1.1,2))){
        selection <- ilcl[i, c(12:17)]
        break
      } else if(inside.range(k,c(2.1,3))){
        selection <- ilcl[i, c(18:21)]
        break
      } else if(inside.range(k,c(3.1,4))){
        selection <- ilcl[i, c(22:27)]
        break
      } 
      else if(inside.range(k,c(4.1,7))){
        q <- readline(prompt = "Press 1 for 1109 LPT Load and 2 for 32' CBT truck :")
        q <- as.integer(q)
        if(q == 1){
          selection <- ilcl[i, c(28:33)]
          break
        } else{
          selection <- ilcl[i, c(60:63)]
          break
        }
      }
      else if(inside.range(k,c(7.1,7.5))){
        selection <- ilcl[i, c(28:33)]
        break
      }
      else if(inside.range(k,c(7.51,9))){
        q <- readline(prompt = "Press 1 for Normal truck and 2 for Open truck :")
        q <- as.integer(q)
        if(q == 1){
          selection <- ilcl[i, c(34:37)]
          break
        } else{
          selection <- ilcl[i, c(38:41)]
          break
        }
      }
      else if(inside.range(k,c(9.1,15))){
        selection <- ilcl[i, c(64:67)]
        break
      }
      else if(inside.range(k,c(15.1,16))){
        selection <- ilcl[i, c(42:45)]
        break
      } else if(inside.range(k,c(16.1,20))){
        selection <- ilcl[i, c(68:71)]
        break
      } else if(inside.range(k,c(20.1,21))){
        q <- readline(prompt = "Press 1 for TAURUS, 2 for 24 feet truck and 3 for 20 feet SIDE OPEN truck :")
        q <- as.integer(q)
        if(q == 1){
          selection <- ilcl[i, c(46:49)]
          break
        } else if(q == 2){
          selection <- ilcl[i, c(50:53)]
          break
        }
        else{
          selection <- ilcl[i, c(54:59)]
          break
        }
      }         
      else if(k > 21){
        k<- readline(prompt = "Try again with a value between 1 to 21 MT :")
        k <- as.integer(k)
      }
    }
    
  }
}

if(length(selection) == 4){
  old <- selection[c(1,3)]
  new <- selection[c(2,4)]  
}else{
  
  old <- selection[c(1,3,5)]
  new <- selection[c(2,4,6)]
  
}


month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July',
           'August', 'September', 'October', 'November', 'December')
high_2000 <- c(32.5, 37.6, 49.9, 53.0, 69.1, 75.4, 76.5, 76.6, 70.7, 60.6, 45.1, 29.3)
low_2000 <- c(13.8, 22.3, 32.5, 37.2, 49.9, 56.1, 57.7, 58.3, 51.2, 42.8, 31.6, 15.9)
high_2007 <- c(36.5, 26.6, 43.6, 52.3, 71.5, 81.4, 80.5, 82.2, 76.0, 67.3, 46.1, 35.0)
low_2007 <- c(23.6, 14.0, 27.0, 36.8, 47.6, 57.7, 58.9, 61.2, 53.3, 48.5, 31.0, 23.6)
high_2014 <- c(28.8, 28.5, 37.0, 56.8, 69.7, 79.7, 78.5, 77.8, 74.1, 62.6, 45.3, 39.9)
low_2014 <- c(12.7, 14.3, 18.6, 35.5, 49.9, 58.0, 60.0, 58.6, 51.7, 45.2, 32.2, 29.1)

data <- data.frame(month, high_2000, low_2000, high_2007, low_2007, high_2014, low_2014)

#The default order will be alphabetized unless specified as below:
data$month <- factor(data$month, levels = data[["month"]])

p <- plot_ly(data, x = ~month, y = ~high_2014, name = 'High 2014', type = 'scatter', mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
  add_trace(y = ~low_2014, name = 'Low 2014', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
  add_trace(y = ~high_2007, name = 'High 2007', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) %>%
  add_trace(y = ~low_2007, name = 'Low 2007', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash')) %>%
  add_trace(y = ~high_2000, name = 'High 2000', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dot')) %>%
  add_trace(y = ~low_2000, name = 'Low 2000', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot')) %>%
  layout(title = "Average High and Low Temperatures in New York",
         xaxis = list(title = "Months"),
         yaxis = list (title = "Temperature (degrees F)"))










cost_range <- range(0, old, new)
plot(old, type = "o", col="blue", ylim = cost_range, axes = FALSE, ann = FALSE)

if(length(selection) == 4){
  axis(1, at = 1:2, lab = c("BLR","Saswad"))
}else{
  axis(1, at = 1:3, lab = c("BLR","Saswad","ACPL"))
}
axis(2,las = 1, at = 2000*0:cost_range[2])
box()
lines(new, type = "o", pch = 22, lty = 2 , col = "red")
title(main = "Cost variation", col.main ="red", font.main = 4)

title(xlab = "Truck service providers", col.lab = rgb(0,0.5,0))
title(ylab = "Cost", col.lab = rgb(0,0.5,0))


legend(1,cost_range[2], c("old","new"), cex = 0.8, col = c("blue", "red"), pch = 21:22, lty = 1:2)

