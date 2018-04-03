#fungsi mencari nilai akumulasi
#k:modal, i:bunga, t=tahun, m:banyak pemberian bunga dalam 1 tahun

nilaiakumulasi <- function(num, k, i, t, m=TRUE)
  switch(num,
         satu = {
           bungatunggal = k*(1+i*t)
           print(bungatunggal)
         },
         dua = {
           bungamajemuknominal = k*(1+i/m)^(m*t)
           print(bungamajemuknominal)
         },
         tiga = {
           bungamajemukkontinu = k*exp(i*t)
           print(bungamajemukkontinu)
         }
  )
