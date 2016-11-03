library("ggplot2")
library("grid")

#Write a function called explore that accepts the following	
#parameters, with defaults for the last three parameters:
#1. A dataframe
#2. A plot switch that can accept three values: off, on, or grid
#3. A threshold cut-off value between 0 and 1 for correlations
#4. An optional vector that contains one or more integers that	
#represent the numbers of bins to use for a histogram. If the	
#vector is not provided, then let ggplot use it's default.

explore <- function(data, plotswitch = "off", threshold = 0, bins = NULL) {
  #thi main function will call several sub-functions that I defined below
  #@parameter: 
  #data frame
  #plot switch
  #threshold: A threshold cut of value between 0 and 1 for correlations
  #An optional vector that contains one or more integers that	
  #represent the numbers of bins to use	for	a	histogram
  
  result1 <- freq_table(data)
  result2 <- num_table(data)
  result3 <- r_square(data)
  result4 <- pearCoefs(data, threshold)
  
  plot_density_count(data,plotswitch,vector) #plot histograms of counts and density
  
  plot_gray (data, plotswitch) #plot gray bar graph for every categorical and binary variable.
  
  main = list(result1, result2, result3, result4) #construct a new list for the result from all sub-functions
  
  return (main) 
  
}


#1 A frequency table for every categorical and logical variable
freq_table <- function(data) {
  #This function takes a dataframe and return a frequecy table for every catagorical and
  #logical variable
  #@parameter: data frame
  #@return the frequency table for categorical and logical variables
  cat_and_log <- c(summary(Filter(is.factor,data)), summary(Filter(is.logical,data))) 
  #loop through the data frame and create the frequecy table for categorical and logical variables
  return(cat_and_log)
}

#2a For numerical variables, a summary statistics table for each numerical variable
num_table <- function(data) {
  #This function takes a dataframe and return a summary statistics table for each numerical variable
  #@parameter: data frame
  #@return the summary statistics table for numerical columns
  num_var <- data[sapply(data, is.numeric)]
  return(summary(num_var)) #return the summary statistics table
}

#2b A data frame that contains each pair of column names in	
#the first column (name the column ???Variable Pairs???) and the	
#associated r-square value in the second column (name the	
#column ???R-Square???).

r_square <- function(data) {
  #@parameter: data frame
  #@return a new data frame that contains each pair of columns names in the first column
  #@and the associated r-square value in the second column
  type <- sapply(data, class) #get the type of all columns
  data <- data[which(type == "numeric")] #extract the numeric columns
  colname <- colnames(data)
  pair_names <- c()
  pair_rsquare <- c()
  for (i in 1:(length(colname) - 1 )) { #looking into every column, starting with 1st column
    for (j in (i+1):length(colname)) { #looking into each column after the first column
      temp <- summary(lm(data[,i]~data[,j]))$r.squared #obtain the r-square value
      pair_names <- c(pair_names, paste(colname[i], colname[j], sep = '-')) #get each pair of the column names
      pair_rsquare <- c(pair_rsquare, temp) #form a new list with r-square values
    }
  }
  new_frame <- data.frame(pair_names, pair_rsquare) #form a new data frame with pair_names and pair_rsquare
  colnames(new_frame) <- c("Variable Pairs","R-Square") #name the columns
  return(new_frame)
}



#2c A data frame that contains each pair of column names in	
#the first column (name the column ???Variable	Pairs???) and	
#correlation coefficient (Pearson) for all coefficients whose	
#absolute value is greater than the correlation threshold (do	
#not repeat any pairs) in the second column (name the	
#column ???Pearson Exceeds Threshold???). (HINT: There is a	
#function that calculates correlation coefficients ??? look	
#carefully at what is returned and optimize how you extract	
#the correlation coefficients)

pearCoefs <- function(data,threshold) {
  #@parameter: a data frame
  #@parameter: a positive number as threshold
  #@return a data frame that contains each pair of column names in the first column and correlation
  #@coefficient (Pearson) for all coeffients whose absolute value is greater than the correlation threshold
  num_data <- data[sapply(data,is.numeric)] #get the numeric columns and store it in a new frame
  num_names <- colnames(num_data) #extract the names of numerical columns
  pairNames <- c() # create a null vector
  pairCoeffs <- c() # create a null vector
  l <- ncol(num_data) # get the number of columns and store it in l
  for (i in 1:(l-1)) { # looking into every column in num_data, starting from 1st column
    for (j in (i+1):l) { #compare each numeric column with each numeric column after it 
      corcoef <- cor(num_data[i],num_data[j],method="pearson") #calculate the correlation coeffiecient using pearson method
      if (abs(corcoef)>threshold) { #if the absolute value of the correlation coeffiecient is larger than the threshold
        pairNames <- c(pairNames,paste(num_names[i],num_names[j],sep='-')) #create the new name pairs
        pairCoeffs <- c(pairCoeffs,corcoef) # store the correlation coefficient that is larger than the threshold
      }
    }
  }
  new_frame <- data.frame(pairNames,pairCoeffs) #create a new data frame that contains pairNames and pairCoeffs
  colnames(new_frame) <- c("Variable Pairs","Pearson Exceeds Threshold") #naming
  return (new_frame) # return the new data frame
}


#3 If the plot switch parameter is "on" or	"grid", then plot a pair of	
#blue histograms with a vertical red line	at the mean (one using	
#counts and the other density) for every numerical variable at	
#each number of bins integer specified in the bin vector
#parameter. If the plot switch is set to "grid", there should be a	
#grid for each count-bin combination and a separate grid for	
#each density-bin size combination. For	example, given 5	
#numeric variables and a vector of three bin number integers,	
#the function should generate 30 individual plots or a total of 6	
#grid plots (with each grid plot containing 5 subplots).

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  #this function will draw multiple graphs in one page and also it will be used for later function
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) { #if layout is null
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage() #new page
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout)))) #plot the graph in the location we want
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


plot_density_count <- function(data,plotswitch='off',vector=NULL){
  #This function plot a pair of blue histograms with a vertical red line at the 
  #mean (one using counts and the other density) for every numerical variable at 
  #each number of bins integer specified in the bin vector parameter.
  
  #@Parameter:
  #a data frame
  #plotswitch decides what to plot
  #vector: bin numbers of historgram 
  #returns a pair of blue histograms with a vertical red line at the 
  #mean
  num = data[sapply(data,is.numeric)] #extract the numerical columns
  if(plotswitch == "on") {
    if(!is.null(vector)) { 
      for(j in 1:length(vector)) { #for loop
        for(i in 1:ncol(num)) {
          mean <- mean(num[,i]) #obtain the mean of each column
          p1 <- ggplot(num,aes(x=num[i]),color = "blue")+ #plot the histogram of counts
            geom_histogram(fill="blue",bins=vector[j])+
            ggtitle(paste(colnames(num[i]),vector[j],sep=" bins="))+
            xlab(colnames(num[i]))+
            geom_vline(xintercept = mean,col="red")  #add red line at the mean
        
          
          p2 <- ggplot(num,aes(x=num[i],..density..))+ 
            geom_histogram(fill="blue",bins=vector[j])+  #plot the histogram of density
            ggtitle(paste(colnames(num[i]),vector[j],sep=" bins="))+
            xlab(colnames(num[i]))+ 
            geom_vline(xintercept = mean,col="red") 
          
          grid.newpage()
          pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
          title <- paste(colnames(num[i]),vector[j],sep=" bin=")
          grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
          print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
          print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2)) #print p1 and p2 
          
        }
      }
    }else{ #if vector isn't NULL
      for(i in 1:ncol(num)) {
        mean <- mean(num[,i]) #obtain the mean of each numeric column
        p1 <- ggplot(num,aes(x=num[i]),color = "blue") + 
          geom_histogram(fill="blue") + #plot the histogram of counts
          ggtitle(paste(colnames(num[i]),"default bins",sep=" bins=") )+
          xlab(colnames(num[i])) +
          geom_vline(xintercept = mean,col="red") #vertical red line at mean
        p2 <- ggplot(num,aes(x=num[i],..density..)) + #plot the histogram of density
          geom_histogram(fill="blue") +
          ggtitle(paste(colnames(num[i]),"default bins",sep=" bins=")) +
          xlab(colnames(num[i])) +
          geom_vline(xintercept = mean,col="red") #vertical red line at mean
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
        title <- paste(colnames(num[i]),"default bins",sep=" bins=")
        grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
        print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
        print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2)) #print p1 and p2 
        
      }
      
    }
    
  }else{
    if(plotswitch == "grid") {  #if plot switch is "grid"
      for(j in 1:length(vector)) {
        grid.newpage()
        his_count <-list()   #create an empty list
        his_density <- list()   #create an empty list
        for(i in 1:ncol(num)){
          his_count[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") + geom_histogram(fill="blue", bins = vector[j]) + labs(title= paste(vector[j], "bins")) #plot histograms of counts and store the graphs in his_count
        }
        multiplot(plotlist = his_count, cols = 2)  
        for(i in 1:ncol(num)) {
          his_density[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue")+geom_histogram(aes(y= ..density..), fill="blue", bins = vector[j])+labs(title= paste(vector[j], "bins")) 
          #plot histograms of density and store them in his_density 
        }
        multiplot(plotlist = his_density, cols = 2)   
      }
    }
  }
}



#4. If the plot switch parameter is "on" or "grid", plot a gray bar
#graph for every categorical and binary variable.
is.binary <- function(v) { #this function will determine if the vector is binary
  
  #@parameter: a vector
  #Returns: TRUE if the vector is binary, FALSE else
  x <- unique(v)                    
  length(x) - sum(is.na(x)) == 2L #check if x has only 2 distict values
}


plot_gray <- function(data, plotswitch='off') {
  #This function will plot a gray bar graph for every categorical and binary variable.
  
  #@parameter: a dataframe, plotswitch: whether and what to plot
  #Returns what is described above
  dfm_cb <- data[,sapply(data,is.factor)|sapply(data,is.logical)|sapply(data,is.binary)] #get the categorical and logical columns
  if(plotswitch=="on"|plotswitch=="grid") {
    for(i in 1:ncol(dfm_cb)){
      p <- ggplot(dfm_cb,aes(x=dfm_cb[,i]),colour="gray") + geom_bar() + xlab(colnames(dfm_cb[i])) #plot gray bar for every categorial and binary variable
      print(p)
    }
  }
}



  