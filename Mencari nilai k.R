# satu = PV annuitas akhir diketahui, dua = NA annuitas akhir diketahui
# tiga = PV annuitas awal diketahui, empat = NA annuitas awal diketahui
payment<- function(num, nilai,i,t, m=TRUE){
  j=i/m
  n=t*m
  v=1/(1+j)
  switch(num,
         satu = {
           k = nilai/((1-v^n)/(j))
           cat("Payment PV akhir adalah : " ,k)
         },
         dua = {
           k = nilai/((((1+j)^n)-1)/j)
           cat("Payment NA akhir adalah : " ,k)
         },
         tiga = {
           k = nilai/((1-v^n)/(j*v))
           cat("Payment PV awal adalah : " ,k)
         },
         empat = {
           k = nilai/((((1+j)^n)-1)/(j*v))
           cat("Payment NA awal adalah : " ,k)
         }
  )
}