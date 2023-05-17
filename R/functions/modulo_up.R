modulo_up<-function(n,p){ # modulo, giving p instead of 0 when k[p]=0
  
  temp<-n%%p
  temp[temp==0]<-rep(p,times=sum(temp==0))

  return(temp)
}
