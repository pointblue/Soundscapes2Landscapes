# Picked species with at least 60 presences. With a 0.8/0.2 train/test this means we get 10 test samples
runs = seq(1,100,1)

specs = c("ACWO", "AMGO", "BEWR", "BHGR", "BLPH", "BRBL", "BUSH",
          "CALT", "CAQU", "CBCH", "DEJU", "HOFI", "LEGO", "MODO",
          "NOFL", "NOMO", "NUWO", "OATI", "RWBL", "SOSP", "SPTO",
          "STJA", "WCSP", "WEBL", "WESJ")

#fams = c("Emberizidae", "Fringillidae", "Picidae", "Corvidae", "Icteridae", "Paridae")

spatRes = c("250M", "500M", "1000M")

tempRes = c("2yr")

gediUsed = c("FALSE")

vifUsed = c("VIF")

balanced = c("Bal")

varType = c("justNDVI")

# All combinations of the above vectors
allCombos = expand.grid(runs, specs, spatRes, tempRes, gediUsed, vifUsed, balanced, varType)

# Output csv
write.table(x = allCombos, file = "//minim.hpc.nau.edu/scratch/pb463/projects/S2L/SDM/SDMbatchList_2yr_Bal_justNDVI_20190429.csv", 
            row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
