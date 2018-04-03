Annuitas <- function(num, k, i, t, m=TRUE){
  j=i/m
  n=t*m
  v=1/(1+j)
  switch(num,
         satu = {
           an = k*(1-v^n)/(j)
           sn = k*(((1+j)^n)-1)/j
           cat("an annuitas akhir : " ,an, "\n")
           cat("sn annuitas akhir : " ,sn, "\n")
         },
         dua = {
           an = k*(1-v^n)/(j*v)
           sn = k*((1+j)^n-1)/(j*v)
           cat("an annuitas awal : " ,an, "\n")
           cat("sn annuitas awal : " ,sn, "\n")
         }
  )
}