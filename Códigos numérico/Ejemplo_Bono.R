#f es la función de la cual se busca obtener la raíz
#a y b son los extremos del intervalo, con a<b
#epsilon es la tolerancia al error
#n es el número máximo de iteraciones
library("ggplot2")
Biseccion<-function(f,a,b,epsilon,n){
  i=1
  while(i<=n){
    p=a+(b-a)/2
    if(f(p)==0 || (b-a)/2<epsilon){
      return(p)
    }else{
      i=i+1
      if(f(a)*f(p)>0){
        a=p
      }else{
        b=p
      }
    }
  }
  return('Error. Se excedió el número límite de iteraciones')
}
precio_bono<-function(tasa_yield,nominal=100,tasa_cupon_anual=0.04,pagos_al_año=2,años=1){
  return (nominal*(1/(1+tasa_yield)^(años*pagos_al_año))+nominal*(tasa_cupon_anual/pagos_al_año)*(1-((1/(1+tasa_yield)^(años*pagos_al_año))))/tasa_yield)
}
eq<-function(x){
  precio=90
  nominal=100
  tasa_cupon=0.02
  n=2
  return (nominal*(1/(1+x)^(n))+nominal*(tasa_cupon)*(1-((1/(1+x)^(n))))/x-precio)
}
nb_it<-function(a,b,epsilon){
  return(ceiling(log((b-a)/(epsilon))/log(2)))
}
ggplot(data.frame(x=c(0.000001, 0.2)), aes(x=x)) + 
  stat_function(fun=eq)+ geom_hline(yintercept=0,color='red')

nb=nb_it(0.05,0.10,10^(-10))
tasa=Biseccion(eq,0.05,0.10,10^(-7),nb)
tasa
eq(tasa) #La diferencia entre 90 y el valor con la tasa aproximada es mínima
precio_bono(tasa) #Casi 90
