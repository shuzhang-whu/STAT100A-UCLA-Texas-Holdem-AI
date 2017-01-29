###NAME:TEAM B
###PEOPLE:YUXIN TANG;PHOEBE WANG;TIANYU YE
library(holdem)
bteam <- function (numattable1, crds1, board1, round1, currentbet, mychips1, 
                   pot1, roundbets, blinds1, chips1, ind1, dealer1, tablesleft) 
{
  my.prob=(currentbet)/(currentbet+pot1)
  max.straight.prob=0.18
  min.straight.prob=0.13
  max.flush.prob=0.204
  min.flush.prob=0.159
  a1 = 0
  if (board1[1,1]==0)
  { 
    if ((crds1[1,1] == crds1[2,1])) 
      a1 = mychips1
  
    if ((crds1[1,2] == crds1[2,2])&((crds1[1,1]>=10)|(crds1[2,1]>=10)))
      a1 = mychips1
    
    if (((crds1[1,1] == 14)&(crds1[2,1] >= 11))|((crds1[2,1] == 14)&(crds1[1,1] >= 11)))
      a1 = mychips1
  
    if ((crds1[1,1]>=10)&(crds1[2,1]>=10))
      a1 = mychips1
  
    if ((mychips1 < 3*blinds1) & ((crds1[1,1] >= 9)|(crds1[2,1] >= 9)))
      a1 = mychips1
  }
  
  if (board1[1,1]!=0&board1[4,1]==0)
  {
    crdNum=c(crds1[1,1],crds1[2,1],board1[1,1],board1[2,1],board1[3,1])
    crdSuit=c(crds1[1,2],crds1[2,2],board1[1,2],board1[2,2],board1[3,2])

    crdhash=rep(0,14)
    crdhash[crds1[1,1]]=crdhash[crds1[1,1]]+1
    crdhash[crds1[2,1]]=crdhash[crds1[2,1]]+1
    crdhash[board1[1,1]]=crdhash[board1[1,1]]+1
    crdhash[board1[2,1]]=crdhash[board1[2,1]]+1
    crdhash[board1[3,1]]=crdhash[board1[3,1]]+1
    
    suithash=rep(0,4)
    suithash[crds1[1,2]]=suithash[crds1[1,2]]+1
    suithash[crds1[2,2]]=suithash[crds1[2,2]]+1
    suithash[board1[1,2]]=suithash[board1[1,2]]+1
    suithash[board1[2,2]]=suithash[board1[2,2]]+1
    suithash[board1[3,2]]=suithash[board1[3,2]]+1
    
    
    crdseq=(crdhash!=0)
    

    for (i in 1:4)
    {
       if (suithash[i]>=4)
         a1=mychips1
    }
 
    standard=rep(TRUE,4)
    for (i in 1:11)
    {
      fourelement=c(crdseq[i],crdseq[i+1],crdseq[i+2],crdseq[i+3])
      if (all(standard==fourelement))
        a1=mychips1
    }

    doublecount=0
    for (i in 1:14)
    {
      if (crdhash[i]>=3)
        a1=mychips1
      if (crdhash[i]==2)
        doublecount=doublecount+1
    }
 
    if(doublecount>=2)
      a1=mychips1
  }
  if (board1[1,1]!=0&board1[4,1]!=0)
  {
    
    crdNum=c(crds1[1,1],crds1[2,1],board1[1,1],board1[2,1],board1[3,1],board1[4,1])
    crdSuit=c(crds1[1,2],crds1[2,2],board1[1,2],board1[2,2],board1[3,2],board1[4,2])
    
    crdhash=rep(0,14)
    crdhash[crds1[1,1]]=crdhash[crds1[1,1]]+1
    crdhash[crds1[2,1]]=crdhash[crds1[2,1]]+1
    crdhash[board1[1,1]]=crdhash[board1[1,1]]+1
    crdhash[board1[2,1]]=crdhash[board1[2,1]]+1
    crdhash[board1[3,1]]=crdhash[board1[3,1]]+1
    crdhash[board1[4,1]]=crdhash[board1[4,1]]+1

    suithash=rep(0,4)
    suithash[crds1[1,2]]=suithash[crds1[1,2]]+1
    suithash[crds1[2,2]]=suithash[crds1[2,2]]+1
    suithash[board1[1,2]]=suithash[board1[1,2]]+1
    suithash[board1[2,2]]=suithash[board1[2,2]]+1
    suithash[board1[3,2]]=suithash[board1[3,2]]+1
    suithash[board1[4,2]]=suithash[board1[4,2]]+1
    
    crdseq=(crdhash!=0)
    for (i in 1:4)
    {
      if(my.prob<max.flush.prob)
      {
        if (suithash[i]>=4)
            a1=mychips1
      }
      if(my.prob>=max.flush.prob)
      {
        if (suithash[i]>=5)
          a1=mychips1
      }
    }
    standardfour=rep(TRUE,4)
    standardfive=rep(TRUE,5)
    for (i in 1:10)
    {
      if (my.prob<max.straight.prob)
      {
        fourelement=c(crdseq[i],crdseq[i+1],crdseq[i+2],crdseq[i+3])
        if (all(standardfour==fourelement))
          a1=mychips1 
      }
      if (my.prob>=max.straight.prob)
      {
        fiveelement=c(crdseq[i],crdseq[i+1],crdseq[i+2],crdseq[i+3],crdseq[i+4])
        if (all(standardfive==fiveelement))
          a1=mychips1
      }
    }
    doublecount=0
    for (i in 1:14)
    {
      if (crdhash[i]>=3)
        a1=mychips1
      if (crdhash[i]==2)
        doublecount=doublecount+1
    }
    if(doublecount>=2)
      a1=mychips1
  }
  a1
}



