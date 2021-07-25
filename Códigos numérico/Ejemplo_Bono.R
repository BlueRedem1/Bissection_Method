#f es la función de la cual se busca obtener la raíz
#a y b son los extremos del intervalo, con a<b
#epsilon es la tolerancia al error
#n es el número máximo de iteraciones
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

tasa_bono<-function(x,precio=90,nominal=100,tasa_cupon_anual=0.04,pagos_al_año=2,años=1){
  return (nominal*(1/(1+x)^(años*pagos_al_año))+nominal*(tasa_cupon_anual/pagos_al_año)*(1-((1/(1+x)^(años*pagos_al_año))))/x-precio)*100000
}
precio_bono<-function(tasa_yield,nominal=100,tasa_cupon_anual=0.04,pagos_al_año=2,años=1){
  return (nominal*(1/(1+tasa_yield)^(años*pagos_al_año))+nominal*(tasa_cupon_anual/pagos_al_año)*(1-((1/(1+tasa_yield)^(años*pagos_al_año))))/tasa_yield)
}
nb_it=ceiling(log((0.2-0.000001)/(10^(-7)))/log(2))
tasa=Biseccion(tasa_bono,0.000001,0.2,10^(-7),nb_it)
tasa_bono(Biseccion(tasa_bono,0.000001,0.2,10^(-7),1000))
precio_bono(tasa)
