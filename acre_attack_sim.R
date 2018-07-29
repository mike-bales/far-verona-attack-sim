"Psychic Assassins"=c(4,0,0,0),
"Demagogue"=c(10,1,8,0),
"Zealots"=c(4,2,6,0),
"Surveyors"=c(4,1,4,0),
"Lawyers"=c(4,1,6,0),

results <- lapply(1:1000, function(x){
  
  ##Faction Stats
  acre.stats <- c('w'=8,'f'=7,'c'=5)
  upc.stats <- c('w'=3,'f'=5,'c'=6)
  
  ##Faction asset lists
  upc.assets <- list("Covert Transit Net"=c(15,0,0,0),
                     "Base Of Influence"=c(29,0,0,0))
  
  acre.assets <- list("Hostile Takeover"=c(10,2,10,0,1),
                      "Commodities Broker1"=c(10,2,8,0,1),
                      "Commodities Broker2"=c(10,2,8,0,1),
                      "Commodities Broker3"=c(10,2,8,0,1),
                      "Space Marines1"=c(16,2,8,2,2),
                      "Space Marines2"=c(16,2,8,2,2)
                      )
  ##Round simulation
  for (x in seq_along(acre.assets)){
    
    #Get index of first upc asset with health greater than 0
    asset <- Position(function(x) x>0, sapply(upc.assets, '[[', 1))

    if (is.na(asset)) {
      break
    }
    
    #Roll ACRE attack and UPC defend
    acre.atk <- sample(1:10,1)+acre.stats[acre.assets[[x]][5]]
    upc.dfnd <- sample(1:10,1)+upc.stats[acre.assets[[x]][5]]
    
    #calculate damage
    acre.dmg <- sum(sample(1:acre.assets[[x]][3],acre.assets[[x]][2],replace = TRUE)) + acre.assets[[x]][4]
    upc.dmg <- sum(sample(1:upc.assets[[asset]][3],upc.assets[[asset]][2],replace = TRUE)) + upc.assets[[asset]][4]
    
    #apply damage
    if (acre.atk > upc.dfnd) {
      upc.assets[[asset]][1] <- upc.assets[[asset]][1] - acre.dmg
    }else if (upc.dfnd > acre.atk){
      acre.assets[[x]][1] <- acre.assets[[x]][1] - upc.dmg
    }else{
      upc.assets[[asset]][1] <- upc.assets[[asset]][1] - acre.dmg
      acre.assets[[x]][1] <- acre.assets[[x]][1] - upc.dmg
    }
  }
  #Return health of each asset
  results <- list(upc.assets = sapply(upc.assets, '[[', 1), acre.assets = sapply(acre.assets, '[[', 1))
  return(results)
  
})

upc.assets.results <- lapply(results,'[[',1)
upc.boi <- sapply(upc.assets.results, '[[',2)
acre.assets.results <- lapply(results,'[[',2)

num.lost.assets <- sapply(upc.assets.results, function(i){min(which(i>0))-1})

hist(num.lost.assets)

num.lost.assets <- sapply(acre.assets.results, function(i){min(which(i>0))-1})

hist(num.lost.assets)


