#Sabit nokta iterasyonunu hem sadece sonuç olarak
#hem de her adım için tablo oluşturarak göstereceğiz.
#Burada
#g : sabit noktasına yaklaşmak istediğimiz fonksiyon
#p0 :  başlangıç noktamız
#tol :hassasiyet eşiğimiz
#iter: maksimum yapılacak iterasyon sayısıdır.

sabit_nokta_metodu <- function(g,p0,tol,iter){
  i=1
  while(i<=iter){
    p=g(p0)
    if(abs(p-p0)<tol){
      return(p)
    }
    else{
      i=i+1
    }
    p0=p
    
  }
  cat("iterasyon sayısı aşıldı")
  return(p)
}

# "sabit_nokta_metodu" fonksiyonumuz
#g fonksiyonumuz tanımlanarak kullanılır. 

# örnek olarak g(x) =10/(4+x) fonksiyonuna p0=1.5 başlangıç değeriyle
#1e-4 hassasiyetle en fazla 30 iterasyonla yaklaşalım.
g <- function(x){10/(4+x)}
sabit_nokta_metodu(g,1.5,1e-4,30)

#Eğer her adımdaki p değerini tablo halinde görmek istersek:

sabit_nokta_metodu_tablo <- function(g,p0,tol,iter){
  
  i=1
  gn=numeric(iter)
  P0=p0
  while(i<=iter){
    p=g(p0)
    gn[i] <- p
    if(abs(p-p0)<tol){
      return(data.frame(n=0:i,
                        gn=c(P0,gn[1:i])
      ))
    }
    else{
      i=i+1
    }
    p0=p
    
  }
  cat("iterasyon sayısı aşıldı")
  return(data.frame(n=0:iter,
                    gn=c(P0,gn)))
}

#Örnekteki g fonksiyonunu tablolu olarak görmek için
# "sabit_nokta_metodu_tablo" fonksiyonu çağırılır.

sabit_nokta_metodu_tablo(g,1.5,1e-4,30)