library("ggplot2")
eq = function(x){1.7+(32.17/(2*x^2))*((exp(x)-exp(-x))/2-sin(x))}
ggplot(data.frame(x=c(-1, 0)), aes(x=x)) + 
  stat_function(fun=eq)+ geom_hline(yintercept=0,color='red')
n=function(a,b,epsilon){
  return(ceiling(log((b-a)/epsilon)/log(2)))
}
n(-0.5,-0.25,10^-5)
eq = function(x){1.7+(32.17/(2*x^2))*((exp(x)-exp(-x))/2-sin(x))}
Biseccion_plano<-function(f,a,b,epsilon){
  n=n(a,b,epsilon)
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
Biseccion_plano(eq,-0.4,-0.2,10^-5)