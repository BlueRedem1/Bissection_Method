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
#Esta función revisa la condición de los signos
Biseccion_mejorada<-function(f,a,b,epsilon,n){
  if(f(a)*f(b)<0){
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
  }else{
    return('No se puede ejecutar. f(a) tiene el mismo signo que f(b) y no se
           puede asegurar la existencia de la raíz')
  }
}