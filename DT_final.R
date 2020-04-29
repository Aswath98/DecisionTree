ID = read.csv("Data.csv")

# Finding Entropy for the labels
t = c()
u = unique.data.frame(ID['y'])
for(l in 1:length(levels(u[,1])) )
{
  value = length(which(ID[5] == levels(u[,1])[l]))
  t = append(t, value, length(t))
}

H_of_y = 0

for( j in 1:length(t) )
  H_of_y = H_of_y - round( ((t[j] / dim(ID)[1])*log2(t[j] / dim(ID)[1])),4 )

# Finding conditional entropy for each feature
Probability = function(c,d,u,D)
{
  P = matrix(0, nrow = length(u), ncol = 2 )
  yes_no = c()
  feature_count = c()
  
  #Finding Probability of feature given output
  for(i in 1:length(u))
  {
    for(j in 1:length(D))
    {
      value = length(which(c==u[i] & d==D[j]))
      yes_no = append(yes_no, value, length(yes_no))
    }
    value = length(which(c==u[i]))
    feature_count = append(feature_count, value, length(feature_count))
    P[i,1] = round( (yes_no[2] / value), 2 )
    P[i,2] = round( (yes_no[1] / value), 2 )
    yes_no = c()
  }
  # Finding specific entropy for each class in the feature 
  p = c()
  value = 0
  for( i in 1:dim(P)[1] )
  {
    for( j in 1:dim(P)[2] )
    {
      value = value + round((P[i,j]*log2(P[i,j])),4)
    }
    p = append(p, -value, length(p))
    value = 0
  }
  
  # Making the Na values in p into zero
  for( i in 1:length(p) )
    if( is.nan(p[i]) == TRUE)
      p[i] = 0.0
  # Finding Conditional probability 
  CP = 0
  for( i in 1:length(p) )
    CP = CP + ((feature_count[i]/dim(ID)[1])*p[i])
  CP = round(CP,4)
  
  return(CP)
}

# FUNCTION
dataset = function(id)
{
  P = c()
  u = unique(id['y'])
  if( (length(u['y'][,1]) == 1) )
    return(NULL)
  
  else
  {
    for( j in 1:(dim(id)[2]-1) )
    {
      c = data.matrix(id[j])
      d = data.matrix(id['y'])
      u = unique(c)
      D = unique(d)
      Prob = Probability(c,d,u,D)
      P = append(P, Prob, length(P))
    }
    
    # Finding Information Gain for each feature
    IG = c()
    for( i in 1:length(P) )
    {
      value = H_of_y - P[i]
      IG = append(IG, value, length(IG))
    }
    index = which(max(IG) == IG, arr.ind = TRUE)
    print(paste("The feature with maximum IG is =", colnames(id[index])))
    
    data = data.frame()
    U = unique.data.frame(id[index])
    
    for( l in 1:length(levels(U[,1])) )
    {
      data = id[id[,index]==levels(U[,1])[l],]
      dataset(data)
      data = data.frame()
    }
  }
  # # Buliding the decision tree
  # Name = colnames(id[index])
  # Tree = append(Tree, Name, length(Tree))
  # for( l in 1:length(unique.data.frame(id[index])) )
  #   Tree = append(Tree, , length(Tree))
}

# Calling the funtion
dataset(ID)
