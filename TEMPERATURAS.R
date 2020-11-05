
# 1. Fahrenheit a Celsius ----

F_to_C <-function(x){
  cel <-((x-32) * 1.8)
  cel
  w <-length(cel)
  r <-rep(NA, w)
  
  for (i in 1:w ) {
    if(cel[i]== -273.15) {
      r[i] <-'Cero absoluto'
    } else if(cel[i]== 0){
      r[i] <-'Pto fusión del agua'
    } else if(cel[i]== 100){
      r[i] <-'Pto ebullición del agua'
    } else {r[i] <-'No relevante'}
  }
  data.frame (x, cel, r, stringsAsFactors = FALSE)
}





# 2. Celsius a Fahrenheit ----

C_to_F <-function(x){
  fah <-((x * 1.8) +32)
  fah
  w <-length(fah)
  r <-rep(NA, w)
  
  for (i in 1:w) {
    if(fah[i]== -459.67) {
      r[i] <-'Cero absoluto'
    } else if(fah[i]== 32){
      r[i] <-'Pto fusión del agua'
    } else if(fah[i]== 212){
      r[i] <-'Pto ebullición del agua'
    } else {r[i] <-'No relevante'}
  }
  data.frame (x, fah, r, stringsAsFactors = FALSE)
}




# 3. Fahrenheit a Kelvin ----

F_to_K <-function(x){
  kel <-( ((x - 32 ) / 1.8 ) + 273.15 )
  kel
  w <-length(kel)
  r <-rep(NA, w)
  
  for (i in 1:w) {
    if(kel[i]== 0) {
      r[i] <-'Cero absoluto'
    } else if(k[i]== 273.15){
      r[i] <-'Pto fusión del agua'
    } else if(k[i]== 373.15){
      result <-'Pto ebullición del agua'
    } else {r[i] <-'No relevante'}
  }
  data.frame (x, kel, r, stringsAsFactors = FALSE)
}




# 4. Kelvin a Fahrenheit ----

K_to_F <-function(x){
  fah <-( (x - 273.15)* 1.8 + 32 )
  fah
  w <-length(fah)
  r <-rep(NA, w)
  for (i in 1:w) {
    
    if(fah[i]== -459.67) {
      r[i] <-'Cero absoluto'
    } else if(fah[i]== 32){
      r[i] <-'Pto fusión del agua'
    } else if(fah[i]== 212){
      r[i] <-'Pto ebullición del agua'
    } else {r[i] <-'No relevante'}
  }
  data.frame(x, fah, r, stringsAsFactors = FALSE) 
}




# 5. Kelvin a Celsius ----

K_to_C <-function(x){
  cel <-(x - 273.15)
  cel
  w <-length(cel)
  r <-rep(NA, w)
  
  for (i in 1:w) {
    if(cel[i]== -273.15) {
      r[i] <-'Cero absoluto'
    } else if(cel[i]== 0 ){
      r[i] <-'Pto fusión del agua'
    } else if(cel[i]== 100){
      r[i] <-'Pto ebullición del agua'
    } else {r[i] <-'No relevante'}
  }
  data.frame(x, cel, r, stringsAsFactors = FALSE) 
}




# 6. Celsius a Kelvin ----

C_to_K <-function(x){
  kel <-(x + 273.15)
  kel
  w <-length(kel)
  r <-rep(NA, w)
  
  for (i in 1:w) {
    if(kel[i]== 0) {
      r[i] <-'Cero absoluto'
    } else if(kel[i]== 273.15){
      r[i] <-'Pto fusión del agua'
    } else if(kel[i]== 373.15){
      r[i] <-'Pto ebullición del agua'
    } else {r[i] <-'No aplica'}
  }
  data.frame(x, kel, r, stringsAsFactors = FALSE) 
}



