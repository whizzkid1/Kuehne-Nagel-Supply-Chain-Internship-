setwd("C:/Users/Ajay khushu/Desktop/Purdue/Supply Chain/K & N Project")
library(spatstat.utils)
efcl <- read.csv("export fcl.csv")
elcl <- read.csv("export lcl.csv")
ifcl <- read.csv("import fcl.csv")
ilcl <- read.csv("import lcl.csv")

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
          selection <- efcl[i, c(2:5)]
          if(all(is.na(selection))){
            minval <- min(selection)
          }else{
            minval <- min(selection, na.rm = TRUE)
          }
          break
        } else if(inside.range(k,c(7.1,14))){
          selection <- efcl[i, c(6:9)]
          if(all(is.na(selection))){
            minval <- min(selection)
          }else{
            minval <- min(selection, na.rm = TRUE)
          }
          break
        } else if(inside.range(k,c(14.1,21))){
          selection <- efcl[i, c(10:13)]
          if(all(is.na(selection))){
            minval <- min(selection)
          }else{
            minval <- min(selection, na.rm = TRUE)
          }
          break
        } else if(inside.range(k,c(21.1,25))){
          selection <- efcl[i, c(14:17)]
          if(all(is.na(selection))){
            minval <- min(selection)
          }else{
            minval <- min(selection, na.rm = TRUE)
          }
          break
        } else if(k > 25){
          k<- readline(prompt = "Try again with a value between 1 to 25 ton :")
          k <- as.integer(k)
        }
      }
      a <- match(minval,selection)
      
      if(is.na(minval)){
        print("The Cost data not available")
      }else if(minval == 100000){
        print("Heavy vehicle on spot quotation")
      } else if(a == 1){
        cat("The best choice would be Ganpati and the cost will be Rs.", minval)
      } else if(a == 2){
        cat("The best choice would be BLR and the cost will be Rs.", minval)
      } else if(a == 3){
        cat("The best choice would be ACPL and the cost will be Rs.",minval)
      } else if(a == 4){
        cat("The best choice would be Saswad and the cost will be Rs.",minval)
      }
    } else{
      repeat{
        
        if(inside.range(k,c(1,15))){
          selection <- efcl[i, c(18:21)]
          if(all(is.na(selection))){
            minval <- min(selection)
          }else{
            minval <- min(selection, na.rm = TRUE)
          }
          break
        } else if(inside.range(k,c(15.1,20))){
          selection <- efcl[i, c(22:25)]
          if(all(is.na(selection))){
            minval <- min(selection)
          }else{
            minval <- min(selection, na.rm = TRUE)
          }
          break
        } else if(inside.range(k,c(20.1,24))){
          selection <- efcl[i, c(26:29)]
          if(all(is.na(selection))){
            minval <- min(selection)
          }else{
            minval <- min(selection, na.rm = TRUE)
          }
          break
        } else if(inside.range(k,c(24.1,27))){
          selection <- efcl[i, c(30:33)]
          if(all(is.na(selection))){
            minval <- min(selection)
          }else{
            minval <- min(selection, na.rm = TRUE)
          }
          break
        } else if(k > 27){
          k<- readline(prompt = "Try again with a value between 1 to 25 ton :")
          k <- as.integer(k)
        }
      }
      a <- match(minval,selection)
      
      if(is.na(minval)){
        print("The cost data is unavailable")
      } else if(minval == 100000){
        print("Heavy vehicle on spot quotation")
      } else if(a == 1){
        cat("The best choice would be Ganpati and the cost will be Rs.",minval)
      } else if(a == 2){
        cat("The best choice would be BLR and the cost will be Rs.",minval)
      } else if(a == 3){
        cat("The best choice would be ACPL and the cost will be Rs.",minval)
      } else if(a == 4){
        cat("The best choice would be Saswad and the cost will be Rs.",minval)
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
        selection <- elcl[i, c(2:3)]
        if(all(is.na(selection))){
          minval <- min(selection)
        }else{
          minval <- min(selection, na.rm = TRUE)
        }
        print("The truck would be TATA ACE")
        break
      } else if(inside.range(k,c(0.51,1))){
        selection <- elcl[i, c(4:6)]
        if(all(is.na(selection))){
          minval <- min(selection)
        }else{
          minval <- min(selection, na.rm = TRUE)
        }
        print("The truck would be MAHINDRA PICK UP")
        break
      } else if(inside.range(k,c(1.1,2))){
        selection <- elcl[i, c(7:9)]
        if(all(is.na(selection))){
          minval <- min(selection)
        }else{
          minval <- min(selection, na.rm = TRUE)
        }
        print("The truck would be TATA 407(TEMPO)")
        break
      } else if(inside.range(k,c(2.1,3))){
        selection <- elcl[i, c(10:11)]
        if(all(is.na(selection))){
          minval <- min(selection)
        }else{
          minval <- min(selection, na.rm = TRUE)
        }
        print("The truck would be 709 TEMPO")
        break
      } else if(inside.range(k,c(3.1,4))){
        selection <- elcl[i, c(12:14)]
        if(all(is.na(selection))){
          minval <- min(selection)
        }else{
          minval <- min(selection, na.rm = TRUE)
        }
        print("The truck would be 909 CANTER")

break
      } 
      else if(inside.range(k,c(4.1,7))){
        q <- readline(prompt = "Press 1 for 1109 LPT Load and 2 for 32' CBT truck :")
        q <- as.integer(q)
        if(q == 1){
          selection <- elcl[i, c(15:17)]
          if(all(is.na(selection))){
            minval <- min(selection)
          }else{
            minval <- min(selection, na.rm = TRUE)
          }
          break
        } else{
          selection <- elcl[i, c(31:32)]
          if(all(is.na(selection))){
            minval <- min(selection)
          }else{
            minval <- min(selection, na.rm = TRUE)
          }
          break
        }
      }
      else if(inside.range(k,c(7.1,7.5))){
        selection <- elcl[i, c(15:17)]
        if(all(is.na(selection))){
          minval <- min(selection)
        }else{
          minval <- min(selection, na.rm = TRUE)
        }
        print("The truck would be 1109 LPT LOAD")
        break
      }
      else if(inside.range(k,c(7.51,9))){
        q <- readline(prompt = "Press 1 for Normal truck and 2 for Open truck :")
        q <- as.integer(q)
        if(q == 1){
          selection <- elcl[i, c(18:19)]
          if(all(is.na(selection))){
            minval <- min(selection)
          }else{
            minval <- min(selection, na.rm = TRUE)
          }
          break
        } else{
          selection <- elcl[i, c(20:21)]
          if(all(is.na(selection))){
            minval <- min(selection)
          }else{
            minval <- min(selection, na.rm = TRUE)
          }
          break
        }
      }
      else if(inside.range(k,c(9.1,15))){
        selection <- elcl[i, c(33:34)]
        if(all(is.na(selection))){
          minval <- min(selection)
        }else{
          minval <- min(selection, na.rm = TRUE)
        }
        print("The optimal choice would be a 32 feet truck :")
        break
      }
      else if(inside.range(k,c(15.1,16))){
        selection <- elcl[i, c(22:23)]
        if(all(is.na(selection))){
          minval <- min(selection)
        }else{
          minval <- min(selection, na.rm = TRUE)
        }
        print("The optimal choice of truck would be 16MT TORUS")
        break
      } else if(inside.range(k,c(16.1,20))){
        selection <- elcl[i, c(35:36)]
        if(all(is.na(selection))){
          minval <- min(selection)
        }else{
          minval <- min(selection, na.rm = TRUE)
        }
        print("The optimal choice of truck would be 40 feet TRAILOR(40*8*7)")
        break
      } else if(inside.range(k,c(20.1,21))){
        q <- readline(prompt = "Press 1 for TAURUS, 2 for 24 feet truck and 3 for 20 feet SIDE OPEN truck :")
        q <- as.integer(q)
        if(q == 1){
          selection <- elcl[i, c(24:25)]
          if(all(is.na(selection))){
            minval <- min(selection)
          }else{
            minval <- min(selection, na.rm = TRUE)
          }
          break
        } else if(q == 2){
          selection <- elcl[i, c(26:27)]
          if(all(is.na(selection))){
            minval <- min(selection)
          }else{
            minval <- min(selection, na.rm = TRUE)
          }
          break
        }
        else{
          selection <- elcl[i, c(28:30)]
          if(all(is.na(selection))){
            minval <- min(selection)
          }else{
            minval <- min(selection, na.rm = TRUE)
          }
          break
        }
      }         
      else if(k > 21){
        k<- readline(prompt = "Try again with a value between 1 to 21 MT :")
        k <- as.integer(k)
      }
    }
    a <- match(minval,selection)
      
    if(is.na(minval)){
      print("The cost data is not available ")
    } else if(minval == 100000){
      print("Heavy vehicle on spot quotation")
    } else if(a == 1){
      cat("The best choice would be BLR and the cost will be Rs.",minval)
    } else if(a == 2){
      cat("The best choice would be Saswad and the cost will be Rs.",minval)
    } else if(a == 3){
      cat("The best choice would be ACPL and the cost will be Rs.",minval)
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
          selection <- efcl[i, c(2:5)]
          if(all(is.na(selection))){
            minval <- min(selection)
            }else{
              minval <- min(selection, na.rm = TRUE)
              }
          break
          }
        else if(inside.range(k,c(7.1,14))){
          selection <- efcl[i, c(6:9)]
          if(all(is.na(selection))){
            minval <- min(selection)  
          }else{
          minval <- min(selection, na.rm = TRUE)
          }
          break
        } else if(inside.range(k,c(14.1,21))){
          selection <- efcl[i, c(10:13)]
          if(all(is.na(selection))){
            minval <- min(selection)  
          }else{
          minval <- min(selection, na.rm = TRUE)
          }
          break
        } else if(inside.range(k,c(21.1,25))){
          selection <- efcl[i, c(14:17)]
          if(all(is.na(selection))){
            minval <- min(selection)  
          }else{
          minval <- min(selection, na.rm = TRUE)
          }
          break
        } else if(k > 25){
          k<- readline(prompt = "Try again with a value between 1 to 25 ton :")
          k <- as.integer(k)
        }
      }
      a <- match(minval,selection)
      
      if(is.na(minval)){
        print("The cost data not available")
      }else if(minval == 100000){
        print("Heavy vehicle on spot quotation")
      } else if(a == 1){
        cat("The best choice would be Ganpati and the cost will be Rs.", minval)
      } else if(a == 2){
        cat("The best choice would be BLR and the cost will be Rs.", minval)
      } else if(a == 3){
        cat("The best choice would be ACPL and the cost will be Rs.",minval)
      } else if(a == 4){
        cat("The best choice would be Saswad and the cost will be Rs.",minval)
      }
    } else{
      repeat{
        
        if(inside.range(k,c(1,15))){
          selection <- efcl[i, c(18:21)]
          if(all(is.na(selection))){
            minval <- min(selection)  
          }else{
          minval <- min(selection, na.rm = TRUE)
          }
          break
        } else if(inside.range(k,c(15.1,20))){
          selection <- efcl[i, c(22:25)]
          if(all(is.na(selection))){
            minval <- min(selection)  
          }else{
          minval <- min(selection, na.rm = TRUE)
          }
          break
        } else if(inside.range(k,c(20.1,24))){
          selection <- efcl[i, c(26:29)]
          if(all(is.na(selection))){
            minval <- min(selection)  
          }else{minval <- min(selection, na.rm = TRUE)}
          break
        } else if(inside.range(k,c(24.1,27))){
          selection <- efcl[i, c(30:33)]
          if(all(is.na(selection))){
            minval <- min(selection)  
          }else{minval <- min(selection, na.rm = TRUE)}
          break
        } else if(k > 27){
          k<- readline(prompt = "Try again with a value between 1 to 25 ton :")
          k <- as.integer(k)
        }
      }
      a <- match(minval,selection)
      
      if(minval == 100000){
        print("Heavy vehicle on spot quotation")
      } else if(is.na(minval)){
        print("Data unavailable")
      }  else if(a == 1){
        cat("The best choice would be Ganpati and the cost will be Rs.",minval)
      } else if(a == 2){
        cat("The best choice would be BLR and the cost will be Rs.",minval)
      } else if(a == 3){
        cat("The best choice would be ACPL and the cost will be Rs.",minval)
      } else if(a == 4){
        cat("The best choice would be Saswad and the cost will be Rs.",minval)
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
        selection <- elcl[i, c(2:3)]
        if(all(is.na(selection))){
          minval <- min(selection)  
        }else{minval <- min(selection, na.rm = TRUE)}
        print("The truck would be TATA ACE")
        break
      } else if(inside.range(k,c(0.51,1))){
        selection <- elcl[i, c(4:6)]
        if(all(is.na(selection))){
          minval <- min(selection)  
        }else{minval <- min(selection, na.rm = TRUE)}
        print("The truck would be MAHINDRA PICK UP")
        break
      } else if(inside.range(k,c(1.1,2))){
        selection <- elcl[i, c(7:9)]
        if(all(is.na(selection))){
          minval <- min(selection)  
        }else{minval <- min(selection, na.rm = TRUE)}
        print("The truck would be TATA 407(TEMPO)")
        break
      } else if(inside.range(k,c(2.1,3))){
        selection <- elcl[i, c(10:11)]
        if(all(is.na(selection))){
          minval <- min(selection)  
        }else{minval <- min(selection, na.rm = TRUE)}
        print("The truck would be 709 TEMPO")
        break
      } else if(inside.range(k,c(3.1,4))){
        selection <- elcl[i, c(12:14)]
        if(all(is.na(selection))){
          minval <- min(selection)  
        }else{minval <- min(selection, na.rm = TRUE)}
        print("The truck would be 909 CANTER")
        break
      } 
      else if(inside.range(k,c(4.1,7))){
        q <- readline(prompt = "Press 1 for 1109 LPT Load and 2 for 32' CBT truck :")
        q <- as.integer(q)
        if(q == 1){
          selection <- elcl[i, c(15:17)]
          if(all(is.na(selection))){
            minval <- min(selection)  
          }else{minval <- min(selection, na.rm = TRUE)}
          break
        } else{
          selection <- elcl[i, c(31:32)]
          if(all(is.na(selection))){
            minval <- min(selection)  
          }else{minval <- min(selection, na.rm = TRUE)}
          break
        }
      }
      else if(inside.range(k,c(7.1,7.5))){
        selection <- elcl[i, c(15:17)]
        if(all(is.na(selection))){
          minval <- min(selection)  
        }else{minval <- min(selection, na.rm = TRUE)}
        print("The truck would be 1109 LPT LOAD")
        break
      }
      else if(inside.range(k,c(7.51,9))){
        q <- readline(prompt = "Press 1 for Normal truck and 2 for Open truck :")
        q <- as.integer(q)
        if(q == 1){
          selection <- elcl[i, c(18:19)]
          if(all(is.na(selection))){
            minval <- min(selection)  
          }else{minval <- min(selection, na.rm = TRUE)}
          break
        } else{
          selection <- elcl[i, c(20:21)]
          if(all(is.na(selection))){
            minval <- min(selection)  
          }else{minval <- min(selection, na.rm = TRUE)}
          break
        }
      }
      else if(inside.range(k,c(9.1,15))){
        selection <- elcl[i, c(33:34)]
        if(all(is.na(selection))){
          minval <- min(selection)  
        }else{minval <- min(selection, na.rm = TRUE)}
        print("The optimal choice would be a 32 feet truck.")
        break
      }
      else if(inside.range(k,c(15.1,16))){
        selection <- elcl[i, c(22:23)]
        if(all(is.na(selection))){
          minval <- min(selection)  
        }else{minval <- min(selection, na.rm = TRUE)}
        print("The optimal choice of truck would be 16MT TORUS.")
        break
      } else if(inside.range(k,c(16.1,20))){
        selection <- elcl[i, c(35:36)]
        if(all(is.na(selection))){
          minval <- min(selection)  
        }else{minval <- min(selection, na.rm = TRUE)}
        print("The optimal choice of truck would be 40 feet TRAILOR(40*8*7).")
        break
      } else if(inside.range(k,c(20.1,21))){
        q <- readline(prompt = "Press 1 for TAURUS, 2 for 24 feet truck and 3 for 20 feet SIDE OPEN truck :")
        q <- as.integer(q)
        if(q == 1){
          selection <- elcl[i, c(24:25)]
          if(all(is.na(selection))){
            minval <- min(selection)  
          }else{minval <- min(selection, na.rm = TRUE)}
          break
        } else if(q == 2){
          selection <- elcl[i, c(26:27)]
          if(all(is.na(selection))){
            minval <- min(selection)  
          }else{minval <- min(selection, na.rm = TRUE)}
          break
        }
        else{
          selection <- elcl[i, c(28:30)]
          if(all(is.na(selection))){
            minval <- min(selection)  
          }else{minval <- min(selection, na.rm = TRUE)}
          break
        }
      }         
      else if(k > 21){
        k<- readline(prompt = "Try again with a value between 1 to 21 MT :")
        k <- as.integer(k)
      }
    }
    a <- match(minval,selection)
    
    if(is.na(minval)){
      print("The cost data is not available.")
    } else if(minval == 100000){
      print("Heavy vehicle on spot quotation.")
    } else if(a == 1){
      cat("The best choice would be BLR and the cost will be Rs.",minval)
    } else if(a == 2){
      cat("The best choice would be Saswad and the cost will be Rs.",minval)
    } else if(a == 3){
      cat("The best choice would be ACPL and the cost will be Rs.",minval)
    }
  }
}

if(!is.na(minval) & minval!=100000){
  
  residue <-selection[-which(selection == minval)]
  cat("\nThe rest of the options that you have are:\n")
  print(residue)
  
}


  


