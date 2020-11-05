
# 1. Fahrenheit a Celsius ----

F_to_C <-function(x){
  for (i in x) {
    cel <-( (i-32) / 1.8)
    
    if(cel== -273.15) {
      result <-'Cero absoluto'
    } else if(cel== 0){
      result <-'Pto fusión del agua'
    } else if(cel== 100){
      result <-'Pto ebullición del agua'
    } else {result <-''}
    
    
    print (paste(cel,'°C', result))
   }
}




# 2. Celsius a Fahrenheit ----

C_to_F <-function(x){
  for (i in x) {
    fah <-((i * 1.8) +32)
    
    if(fah== -459.67) {
      result <-'Cero absoluto'
    } else if(fah== 32){
      result <-'Pto fusión del agua'
    } else if(fah== 212){
      result <-'Pto ebullición del agua'
    } else {result <-''}
    
    
    print (paste(fah,'°F', result))
  }
}




# 3. Fahrenheit a Kelvin ----

F_to_K <-function(x){
  for (i in x) {
    kel <-( ((i - 32 ) / 1.8 ) + 273.15)
    
    if(kel== 0) {
      result <-'Cero absoluto'
    } else if(kel== 273.15){
      result <-'Pto fusión del agua'
    } else if(kel== 373.15){
      result <-'Pto ebullición del agua'
    } else {result <-''}
    
    
    print (paste(kel,'K', result))
  }
}




# 4. Kelvin a Fahrenheit ----

K_to_F <-function(x){
  for (i in x) {
    fah <-( (i - 273.15)* 1.8 + 32 )
    
    if(fah== -459.67) {
      result <-'Cero absoluto'
    } else if(fah== 32){
      result <-'Pto fusión del agua'
    } else if(fah== 212){
      result <-'Pto ebullición del agua'
    } else {result <-''}
    
    
    print (paste(fah,'°F', result))
  }
}




# 5. Kelvin a Celsius ----

K_to_C <-function(x){
  for (i in x) {
    cel <-(i - 273.15)
    
    if(cel== -273.15) {
      result <-'Cero absoluto'
    } else if(cel== 0 ){
      result <-'Pto fusión del agua'
    } else if(cel== 100){
      result <-'Pto ebullición del agua'
    } else {result <-''}
    
    
    print (paste(cel,'°C', result))
  }
}




# 6. Celsius a Kelvin ----

C_to_K <-function(x){
  for (i in x) {
    kel <-(i + 273.15)
    
    if(kel== 0) {
      result <-'Cero absoluto'
    } else if(kel== 273.15){
      result <-'Pto fusión del agua'
    } else if(kel== 373.15){
      result <-'Pto ebullición del agua'
    } else {result <-''}
    
    
    print (paste(kel,'K', result))
  }
}



