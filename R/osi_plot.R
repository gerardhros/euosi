#' This function calculates the phosphate availability. 
#' 
#' @param shi (numeric) A soil health indicator value estimated with euosi
#' @param xpar (numeric) The soil property controlling the shi value
#' @param pgroup (character) A grouping factor to show different responses of shi
#' 
#' @import ggplot2
#' 
#' @export
osi_plot_shi <- function(shi, xpar, xpar_label ='soil indicator',pgroup = NULL){
  
  # check length inputs
  arg.length <- max(length(shi),length(xpar),length(pgroup))
  
  # check input, should vary between 0 and 1
  checkmate::assert_numeric(shi, lower = 0, upper = 1, any.missing = TRUE,len = arg.length)
  checkmate::assert_numeric(xpar,lower = 0,len = arg.length)
  checkmate::assert_character(pgroup,len = arg.length, null.ok = TRUE)
  
  # make internal table
  dt <- data.table(shi = shi, xpar = xpar,pgroup = pgroup)
  
  # classification colour for soil health assessments
  df.class <- data.table(xmin = rep(0,5),
                         xmax = rep(max(dt$xpar),5),
                         ymin = c(0,0.25,0.5,0.75,1.00),
                         ymax = c(0.25,0.5,0.75,1.0,1),
                         classUK = c('very low', 'low', 'moderate', 'high', 'very high'),
                         classNL = c('Vrij laag','Laag','Gemiddeld','Hoog','Vrij hoog'))
  df.class[,classUK := factor(classUK,levels=rev(c('very low', 'low', 'moderate', 'high', 'very high')))]
  
  # colors for the legend
  col_legend <- c('#238b45','#238b45','#ffffbf','#fdae61','#d7191c')
  
  # 
  
  # make soil health assessment plot
  if(is.null(pgroup)){
    
    # plot the figure
    p1 <- ggplot() +
          geom_rect(data = df.class, 
                    aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = classUK), 
                    alpha = 0.6, show.legend = FALSE) +
          geom_line(data = dt, aes(x = xpar,y=shi), linewidth = 1,show.legend = F) +
          theme_bw() + xlab(xpar_label) + ylab('shi score')+
          scale_fill_manual(name = 'soil health score', values=col_legend) +
          theme(axis.title = element_text(size=9),
                axis.text = element_text(size=9))
    
  }
  
  
  if(!is.null(pgroup)) {
    
    # circle (21), square (22), diamond (23), tirangle (24)
    plot_shapes <- c(21,21,21,22,22,22,23,23,23,24,24,24)
    plot_fill_color <- rep(c('black','white','gray75'),4)
    plot_shape_color <- rep('black',12)
    
    # subset for five symbols on the line
    dt[,id:= 1:.N,by = pgroup]
    dt2 <- dt[id %in% round(seq(1,max(id),length.out = 5))]
    dt2[,pgroup := as.factor(pgroup)]
    
    # plot the funciotn with groups
    p1 <- ggplot() +
          geom_rect(data = df.class[1,], aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
                    fill = '#d7191c',alpha = 0.6) +
          geom_rect(data = df.class[2,],aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
                    fill = '#fdae61',alpha = 0.6) +
          geom_rect(data = df.class[3,],aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
                fill = '#ffffbf',alpha = 0.6) +
          geom_rect(data = df.class[4,],aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
                fill = '#238b45',alpha = 0.6) +
          geom_line(data = dt, aes(x = xpar,y = shi,group = pgroup), linewidth = 1,show.legend = F) +
          geom_point(data=dt2,aes(x = xpar,y = shi,group = pgroup,
                                  fill=pgroup,color=pgroup,shape=pgroup),size=2,show.legend = T) +
          scale_shape_manual(values = plot_shapes,name=NULL)+
          scale_fill_manual(values = plot_fill_color,name=NULL)+
          scale_color_manual(values = plot_shape_color,name = NULL) + 
          xlab(xpar_label) + ylab('shi score')+ ylim(0,1) + theme_bw()+
          theme(legend.position = 'inside',
                legend.position.inside = c(0.7,0.3),
                #legend.position = 'right',
                legend.text = element_text(size=6),
                legend.title = element_text(size=6),
                axis.title = element_text(size=9),
                axis.text = element_text(size=9),
                legend.margin=margin(0,5,0,0),
                legend.background = element_rect(color='white')) 
  }
  
  # return the plot
  return(p1)
  
}




