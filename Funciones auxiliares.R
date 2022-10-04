#Biplot de PCA
pc.biplot.pca <- function(X,col=NULL,id=NULL,
                          adjust_axis_limits=TRUE,
                          alpha=0){
  library(ggplot2)
  library(RColorBrewer)
  
  if(sum(round(colMeans(X),5) != 0) > 0){
    stop('Colums of X must have zero mean')
  }
  
  n<-nrow(X)
  p<-ncol(X)
  
  #Computes X's SVD
  #And creates matrices with coordinates
  #First cooordinates are forced to be positive
  svd <- svd(X)
  
  #Chooses specific biplot according to alpha
  ifelse(alpha==0,{
    D_H <- diag(svd$d)[1:2,1:2]
    D_G <- diag(c(1,1))
  },{
    D_H <- diag(c(1,1))
    D_G <- diag(svd$d)[1:2,1:2]
    adjust_axis_limits = FALSE
  })
  
  #Obtains main factorization
  H <- 1/sqrt(n-1) * svd$v[,1:2] %*% D_H * sign(svd$v[rep(1,p),1:2]) #Variables
  G <- sqrt(n-1) * svd$u[,1:2] %*% D_G *  sign(svd$v[rep(1,n),1:2])  #Observations
  
  #Default color is black
  black_color <- FALSE
  if(is.null(col)==TRUE){
    col<-rep(1,n)
    black_color <- TRUE
  }
  
  #Default id is seq_along(X)
  if(is.null(id)==TRUE){
    id<-1:n
  }
  
  #Data frame with observations
  scores<-data.frame(id,G,factor(col))
  names(scores)<-c('obsname','PCA1','PCA2','col')
  
  #Data frame with coordinates of variables
  segment <- data.frame(rep(0,p),H[,1],rep(0,p),H[,2])
  names(segment) <- c('x1','x2','y1','y2')
  
  #Aadditional information
  rank <- c()
  rank[1] <- round(diag(svd$d)[1,1]/sum(svd$d)*100,2)
  rank[2] <- round(diag(svd$d)[2,2]/sum(svd$d)*100,2)
  names <- colnames(X)
  
  #Biplot
  p <- ggplot() +
    labs(x=paste("PC1-",rank[1],"%"),
         y=paste("PC2-",rank[2],"%"),
         title="PCA Biplot",
         subtitle=paste("Expl-",sum(rank[1:2]),"%"))+
    geom_text(data=scores,mapping=aes(x=PCA1,
                                      y=PCA2,
                                      label=obsname,
                                      color=col),
              alpha=0.7,size=3) +
    geom_segment(data=segment,aes(x=x1,y=y1,xend=x2,yend=y2),
                 arrow=arrow(length=unit(0.2,"cm")),
                 alpha=0.75) +
    geom_text(data=segment,
              aes(x=x2, y=y2,
                  label=names, vjust=1,hjust=1),size=3) +
    theme_minimal()+scale_color_brewer(palette="Set1")
  if(black_color==TRUE){
    p <- p + theme(legend.position='none')}
  if(adjust_axis_limits==TRUE){
    p <- p + scale_x_continuous(limits=c(-3,3)) +
      scale_y_continuous(limits=c(-3,3))
  }
  
  p
}

## Splitting de variabilidad en grupos
ss.split<-function(data, grouping){
  
  #Multivariate Sum of Squares - Last update 9/11/22
  data<-split(data.frame(data),grouping)
  
  k<-length(data); p<-ncol(data[[1]])
  n<-sapply(data,nrow); N<-sum(n)
  
  XR <- Reduce('+',lapply(data,function(x) nrow(x)*colMeans(x)))/sum(sapply(data,nrow))
  U <- Reduce('+',lapply(data,function(x) (nrow(x)-1)*cov(x)))
  H <- Reduce('+',lapply(data,function(x) nrow(x)*(colMeans(x)-XR)%*%t((colMeans(x)-XR))))
  rownames(H) <- colnames(H)
  
  return(list('total.SS'=U+H,
              'within.SS'=U,
              'between.SS'=H
  ))
}

## Analisis multivariante de la varianza
pc.manova<-function(data,grouping){
  #Multivariate Analysis of Variance - Last update 9/11/22
  #Splits data by grouping
  data<-split(data.frame(data),grouping)
  
  # Matrix calculation
  k<-length(data); p<-ncol(data[[1]])
  n<-sapply(data,nrow); N<-sum(n)
  
  #Check basics
  asymp <- TRUE
  if((p == 1) || (k == 1)){
    stop('Test is not appropriate')
  }
  if((p == 2) || ((p >= 1)&&(k == 2)) ) {
    asymp <- FALSE
  }
  
  #SS Splitting
  U<-Reduce('+',lapply(data,function(x) (nrow(x)-1)*cov(x)))
  XR<-Reduce('+',lapply(data,function(x) nrow(x)*colMeans(x)))/N
  H<-Reduce('+',lapply(data,function(x) nrow(x)*(colMeans(x)-XR)%*%t((colMeans(x)-XR))))
  
  #Test statistic and p-value - Chi square
  if(asymp==TRUE){
    gamma<-(det(U)/det(U+H))^(N/2)
    chi.obs<--2*log(gamma)
    gl2<-p+p*(p+1)/2
    gl1<-p*k+p*(p+1)/2
    p.val<-1-pchisq(chi.obs,df=gl1-gl2)
    
    return(
      cat("\n",
          "--Multivariate analysis of variance--","\n",
          "\n",
          'Asymptotic/Chi-square',"\n",
          'Number of groups=',k,"\n",
          'Chi-Statistic=',chi.obs,"\n",
          'Degrees of freedom=',trunc(gl1-gl2),"\n",
          'p-value=',p.val))
    
  }  else {
    #Test statistic and p-value - Wilks lambda
    wilks<-det(U)/det(U+H)
    if((p == 2)){
      F.obs<-((1-sqrt(wilks))/sqrt(wilks))*(N-k-1)/(k-1)
      p.val<-1-pf(F.obs,2*(k-1),2*(N-k-1))
    }
    if((p >= 1)&&(k == 2)){
      F.obs<-((1-wilks)/wilks)*(N-p-1)/p; p.val<-1-pf(F.obs,p,N-p-1)
    }
    return(
      cat("\n",
          "--Multivariate analysis of variance--","\n",
          "\n",
          'Wilks Lambda statistic',"\n",
          'Number of groups=',k,"\n",
          'F-statistic=',F.obs,"\n",
          'p-value=',p.val))
  }
  
}

## Plot en 3d
pc.plot3d <- function(X,col=NULL,id=NULL, size=6, cube=TRUE){
  
  library(plotly)
  library(RColorBrewer)
  
  n <- nrow(X)
  p <- ncol(X)
  
  data <- data.frame(scale(X, scale=FALSE))
  names(data) <- c('x1','x2','x3')
  
  if(is.null(col)==TRUE){
    data$col <- rep('black',n)
  } else {
    data$col <-col}
  
  if(is.null(id)==TRUE){
    data$id<-1:n
  } else {data$id <- id}
  
  fig <- plot_ly(data,
                 x = ~data[,1], y = ~data[,2], z = ~data[,3],
                 colors = brewer.pal(p,'Set1'), text=~id,
                 marker = list(size=size))
  fig <- fig %>% add_markers(color = ~col)
  fig <- fig %>% layout(scene = list(xaxis = list(title = colnames(X)[1],
                                                  range = c(min(data$x1),max(data$x1))),
                                     yaxis = list(title = colnames(X)[2],
                                                  range = c(min(data$x2),max(data$x2))),
                                     zaxis = list(title = colnames(X)[3],
                                                  range = c(min(data$x3),max(data$x3))),
                                     aspectmode = ifelse(cube==TRUE,'cube','auto')))
  fig
  
}

## Plots de multidimensional scaling
stress.gplot <- function(k,stress){
  data <- data.frame(
    x=k,
    y=stress
  )
  
  # Plot
  ggplot(data, aes(x=x, y=y)) +
    geom_segment( aes(x=x, xend=x, y=0, yend=y), color="grey") +
    geom_point( color="orange", size=4) +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.border = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    xlab("") +
    ylab("Stress-1")
  
}
fitting.plot <- function(delta,mds,col){
  plot(as.dist(delta),mds$confdist,
       main = 'Proximity Fitting Plot',
       pch=20, col=col,lwd=100*as.dist(mds$resmat),
       xlab = 'Original proximity',
       ylab = 'Euclidean distance')
}