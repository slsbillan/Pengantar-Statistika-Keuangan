#fungsi mencari nilai akumulasi
#k:modal, i:bunga, t=tahun, m:banyak pemberian bunga dalam 1 tahun, A:Nilai Akumulasi

presentvalue <- function(num, i, A, t, m=TRUE)
  switch(num,
         satu = {
           bungatunggal = A/(1+i*t)
           print(bungatunggal)
         },
         dua = {
           bungamajemuknominal = A/((1+(i/m))^(m*t))
           print(bungamajemuknominal)
         },
         tiga = {
           bungamajemukkontinu = A/(exp(i*t))
           print(bungamajemukkontinu)
         }
  )
