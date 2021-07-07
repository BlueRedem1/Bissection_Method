library("ggplot2")
#f es la función de la cual se busca obtener la raíz
#a y b son los extremos del intervalo, con a<b
#epsilon es la tolerancia al error
#n es el número máximo de iteraciones
Biseccion_ejemplo<-function(f,a,b,epsilon,n){
  ejemplo_a=double()
  ejemplo_b=double()
  long_int=double()
  long_med=double()
  ejemplo_p=double()
  buena_aprox=logical()
  if(f(a)*f(b)<0){
    i=1
    while(i<=n){
      ejemplo_a[i]=a
      ejemplo_b[i]=b
      long_int[i]=b-a
      long_med[i]=(b-a)/2
      p=a+(b-a)/2
      ejemplo_p[i]=p
      if(f(p)==0 || (b-a)/2<epsilon){
        buena_aprox[i]=T
        return(data.frame(ejemplo_a,ejemplo_b,long_int,long_med,ejemplo_p,buena_aprox))
      }else{
        buena_aprox[i]=F
        i=i+1
        if(f(a)*f(p)>0){
          a=p
        }else{
          b=p
        }
      }
    }
    return(data.frame(ejemplo_a,ejemplo_b,long_int,long_med,ejemplo_p,buena_aprox))
  }else{
    return('No se puede ejecutar. f(a) tiene el mismo signo que f(b) y no se
           puede asegurar la existencia de la raíz')
  }
}
prueba<-function(x){
  return(x^3+4*x^2-10)
}
ejemplo<-Biseccion_ejemplo(prueba,1,2,10^-3,10)
ggplot(data.frame(x=c(-3, 3)), aes(x=x)) + 
  stat_function(fun=prueba)+ geom_hline(yintercept=0,color='red')