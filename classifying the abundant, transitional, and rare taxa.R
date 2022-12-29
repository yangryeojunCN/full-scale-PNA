library(dplyr)

feature.table <- read.table("feature_table_filtered.txt", header= T, row.names=  1, sep="\t")
colSums(feature.table)

abundance <- feature.table / colSums(feature.table)

D1B <- abundance[, c(1:6)]
D1F <- abundance[, c(7:12)]
D2B <- abundance[, c(13:18)]
D2F <- abundance[, c(19:24)]
D3B <- abundance[, c(25:30)]
D3F <- abundance[, c(31:36)]
M1B <- abundance[, c(37:42)]
M1F <- abundance[, c(43:48)]
M2B <- abundance[, c(49:54)]
M2F <- abundance[, c(55:60)]
M3B <- abundance[, c(61:66)]
M3F <- abundance[, c(67:72)]
U1B <- abundance[, c(73:78)]
U1F <- abundance[, c(79:84)]
U2B <- abundance[, c(85:90)]
U2F <- abundance[, c(91:96)]
U3B <- abundance[, c(97:102)]
U3F <- abundance[, c(103:108)]

n <- 1
Occurrence <- function(x) sum(x != 0)

D1B$Occurrence <- apply(D1B,1, Occurrence)
D1F$Occurrence <- apply(D1F,1, Occurrence)
D2B$Occurrence <- apply(D2B,1, Occurrence)
D2F$Occurrence <- apply(D2F,1, Occurrence)
D3B$Occurrence <- apply(D3B,1, Occurrence)
D3F$Occurrence <- apply(D3F,1, Occurrence)
M1B$Occurrence <- apply(M1B,1, Occurrence)
M1F$Occurrence <- apply(M1F,1, Occurrence)
M2B$Occurrence <- apply(M2B,1, Occurrence)
M2F$Occurrence <- apply(M2F,1, Occurrence)
M3B$Occurrence <- apply(M3B,1, Occurrence)
M3F$Occurrence <- apply(M3F,1, Occurrence)
U1B$Occurrence <- apply(U1B,1, Occurrence)
U1F$Occurrence <- apply(U1F,1, Occurrence)
U2B$Occurrence <- apply(U2B,1, Occurrence)
U2F$Occurrence <- apply(U2F,1, Occurrence)
U3B$Occurrence <- apply(U3B,1, Occurrence)
U3F$Occurrence <- apply(U3F,1, Occurrence)

pick.D1B <- subset(D1B,D1B$Occurrence >= n)
pick.D1F <- subset(D1F,D1F$Occurrence >= n)
pick.D2B <- subset(D2B,D2B$Occurrence >= n)
pick.D2F <- subset(D2F,D2F$Occurrence >= n)
pick.D3B <- subset(D3B,D3B$Occurrence >= n)
pick.D3F <- subset(D3F,D3F$Occurrence >= n)
pick.M1B <- subset(M1B,M1B$Occurrence >= n)
pick.M1F <- subset(M1F,M1F$Occurrence >= n)
pick.M2B <- subset(M2B,M2B$Occurrence >= n)
pick.M2F <- subset(M2F,M2F$Occurrence >= n)
pick.M3B <- subset(M3B,M3B$Occurrence >= n)
pick.M3F <- subset(M3F,M3F$Occurrence >= n)
pick.U1B <- subset(U1B,U1B$Occurrence >= n)
pick.U1F <- subset(U1F,U1F$Occurrence >= n)
pick.U2B <- subset(U2B,U2B$Occurrence >= n)
pick.U2F <- subset(U2F,U2F$Occurrence >= n)
pick.U3B <- subset(U3B,U3B$Occurrence >= n)
pick.U3F <- subset(U3F,U3F$Occurrence >= n)

exclude.D1B <- subset(D1B,D1B$Occurrence < n)
exclude.D1F <- subset(D1F,D1F$Occurrence < n)
exclude.D2B <- subset(D2B,D2B$Occurrence < n)
exclude.D2F <- subset(D2F,D2F$Occurrence < n)
exclude.D3B <- subset(D3B,D3B$Occurrence < n)
exclude.D3F <- subset(D3F,D3F$Occurrence < n)
exclude.M1B <- subset(M1B,M1B$Occurrence < n)
exclude.M1F <- subset(M1F,M1F$Occurrence < n)
exclude.M2B <- subset(M2B,M2B$Occurrence < n)
exclude.M2F <- subset(M2F,M2F$Occurrence < n)
exclude.M3B <- subset(M3B,M3B$Occurrence < n)
exclude.M3F <- subset(M3F,M3F$Occurrence < n)
exclude.U1B <- subset(U1B,U1B$Occurrence < n)
exclude.U1F <- subset(U1F,U1F$Occurrence < n)
exclude.U2B <- subset(U2B,U2B$Occurrence < n)
exclude.U2F <- subset(U2F,U2F$Occurrence < n)
exclude.U3B <- subset(U3B,U3B$Occurrence < n)
exclude.U3F <- subset(U3F,U3F$Occurrence < n)

pick.D1B <- subset(pick.D1B,select=-Occurrence)
pick.D1F <- subset(pick.D1F,select=-Occurrence)
pick.D2B <- subset(pick.D2B,select=-Occurrence)
pick.D2F <- subset(pick.D2F,select=-Occurrence)
pick.D3B <- subset(pick.D3B,select=-Occurrence)
pick.D3F <- subset(pick.D3F,select=-Occurrence)
pick.M1B <- subset(pick.M1B,select=-Occurrence)
pick.M1F <- subset(pick.M1F,select=-Occurrence)
pick.M2B <- subset(pick.M2B,select=-Occurrence)
pick.M2F <- subset(pick.M2F,select=-Occurrence)
pick.M3B <- subset(pick.M3B,select=-Occurrence)
pick.M3F <- subset(pick.M3F,select=-Occurrence)
pick.U1B <- subset(pick.U1B,select=-Occurrence)
pick.U1F <- subset(pick.U1F,select=-Occurrence)
pick.U2B <- subset(pick.U2B,select=-Occurrence)
pick.U2F <- subset(pick.U2F,select=-Occurrence)
pick.U3B <- subset(pick.U3B,select=-Occurrence)
pick.U3F <- subset(pick.U3F,select=-Occurrence)

pick.D1B$mean.abundance <- apply(pick.D1B, 1, mean)
pick.D1F$mean.abundance <- apply(pick.D1F, 1, mean)
pick.D2B$mean.abundance <- apply(pick.D2B, 1, mean)
pick.D2F$mean.abundance <- apply(pick.D2F, 1, mean)
pick.D3B$mean.abundance <- apply(pick.D3B, 1, mean)
pick.D3F$mean.abundance <- apply(pick.D3F, 1, mean)
pick.M1B$mean.abundance <- apply(pick.M1B, 1, mean)
pick.M1F$mean.abundance <- apply(pick.M1F, 1, mean)
pick.M2B$mean.abundance <- apply(pick.M2B, 1, mean)
pick.M2F$mean.abundance <- apply(pick.M2F, 1, mean)
pick.M3B$mean.abundance <- apply(pick.M3B, 1, mean)
pick.M3F$mean.abundance <- apply(pick.M3F, 1, mean)
pick.U1B$mean.abundance <- apply(pick.U1B, 1, mean)
pick.U1F$mean.abundance <- apply(pick.U1F, 1, mean)
pick.U2B$mean.abundance <- apply(pick.U2B, 1, mean)
pick.U2F$mean.abundance <- apply(pick.U2F, 1, mean)
pick.U3B$mean.abundance <- apply(pick.U3B, 1, mean)
pick.U3F$mean.abundance <- apply(pick.U3F, 1, mean)

abundant.D1B <- pick.D1B[which(pick.D1B$mean.abundance > 0.01),]
abundant.D1F <- pick.D1F[which(pick.D1F$mean.abundance > 0.01),]
abundant.D2B <- pick.D2B[which(pick.D2B$mean.abundance > 0.01),]
abundant.D2F <- pick.D2F[which(pick.D2F$mean.abundance > 0.01),]
abundant.D3B <- pick.D3B[which(pick.D3B$mean.abundance > 0.01),]
abundant.D3F <- pick.D3F[which(pick.D3F$mean.abundance > 0.01),]
abundant.M1B <- pick.M1B[which(pick.M1B$mean.abundance > 0.01),]
abundant.M1F <- pick.M1F[which(pick.M1F$mean.abundance > 0.01),]
abundant.M2B <- pick.M2B[which(pick.M2B$mean.abundance > 0.01),]
abundant.M2F <- pick.M2F[which(pick.M2F$mean.abundance > 0.01),]
abundant.M3B <- pick.M3B[which(pick.M3B$mean.abundance > 0.01),]
abundant.M3F <- pick.M3F[which(pick.M3F$mean.abundance > 0.01),]
abundant.U1B <- pick.U1B[which(pick.U1B$mean.abundance > 0.01),]
abundant.U1F <- pick.U1F[which(pick.U1F$mean.abundance > 0.01),]
abundant.U2B <- pick.U2B[which(pick.U2B$mean.abundance > 0.01),]
abundant.U2F <- pick.U2F[which(pick.U2F$mean.abundance > 0.01),]
abundant.U3B <- pick.U3B[which(pick.U3B$mean.abundance > 0.01),]
abundant.U3F <- pick.U3F[which(pick.U3F$mean.abundance > 0.01),]

transitional.D1B <- pick.D1B[which(pick.D1B$mean.abundance>= 0.001 & pick.D1B$mean.abundance <= 0.01),]
transitional.D1F <- pick.D1F[which(pick.D1F$mean.abundance>= 0.001 & pick.D1F$mean.abundance <= 0.01),]
transitional.D2B <- pick.D2B[which(pick.D2B$mean.abundance>= 0.001 & pick.D2B$mean.abundance <= 0.01),]
transitional.D2F <- pick.D2F[which(pick.D2F$mean.abundance>= 0.001 & pick.D2F$mean.abundance <= 0.01),]
transitional.D3B <- pick.D3B[which(pick.D3B$mean.abundance>= 0.001 & pick.D3B$mean.abundance <= 0.01),]
transitional.D3F <- pick.D3F[which(pick.D3F$mean.abundance>= 0.001 & pick.D3F$mean.abundance <= 0.01),]
transitional.M1B <- pick.M1B[which(pick.M1B$mean.abundance>= 0.001 & pick.M1B$mean.abundance <= 0.01),]
transitional.M1F <- pick.M1F[which(pick.M1F$mean.abundance>= 0.001 & pick.M1F$mean.abundance <= 0.01),]
transitional.M2B <- pick.M2B[which(pick.M2B$mean.abundance>= 0.001 & pick.M2B$mean.abundance <= 0.01),]
transitional.M2F <- pick.M2F[which(pick.M2F$mean.abundance>= 0.001 & pick.M2F$mean.abundance <= 0.01),]
transitional.M3B <- pick.M3B[which(pick.M3B$mean.abundance>= 0.001 & pick.M3B$mean.abundance <= 0.01),]
transitional.M3F <- pick.M3F[which(pick.M3F$mean.abundance>= 0.001 & pick.M3F$mean.abundance <= 0.01),]
transitional.U1B <- pick.U1B[which(pick.U1B$mean.abundance>= 0.001 & pick.U1B$mean.abundance <= 0.01),]
transitional.U1F <- pick.U1F[which(pick.U1F$mean.abundance>= 0.001 & pick.U1F$mean.abundance <= 0.01),]
transitional.U2B <- pick.U2B[which(pick.U2B$mean.abundance>= 0.001 & pick.U2B$mean.abundance <= 0.01),]
transitional.U2F <- pick.U2F[which(pick.U2F$mean.abundance>= 0.001 & pick.U2F$mean.abundance <= 0.01),]
transitional.U3B <- pick.U3B[which(pick.U3B$mean.abundance>= 0.001 & pick.U3B$mean.abundance <= 0.01),]
transitional.U3F <- pick.U3F[which(pick.U3F$mean.abundance>= 0.001 & pick.U3F$mean.abundance <= 0.01),]

rare.D1B <- pick.D1B[which(pick.D1B$mean.abundance < 0.001),]
rare.D1F <- pick.D1F[which(pick.D1F$mean.abundance < 0.001),]
rare.D2B <- pick.D2B[which(pick.D2B$mean.abundance < 0.001),]
rare.D2F <- pick.D2F[which(pick.D2F$mean.abundance < 0.001),]
rare.D3B <- pick.D3B[which(pick.D3B$mean.abundance < 0.001),]
rare.D3F <- pick.D3F[which(pick.D3F$mean.abundance < 0.001),]
rare.M1B <- pick.M1B[which(pick.M1B$mean.abundance < 0.001),]
rare.M1F <- pick.M1F[which(pick.M1F$mean.abundance < 0.001),]
rare.M2B <- pick.M2B[which(pick.M2B$mean.abundance < 0.001),]
rare.M2F <- pick.M2F[which(pick.M2F$mean.abundance < 0.001),]
rare.M3B <- pick.M3B[which(pick.M3B$mean.abundance < 0.001),]
rare.M3F <- pick.M3F[which(pick.M3F$mean.abundance < 0.001),]
rare.U1B <- pick.U1B[which(pick.U1B$mean.abundance < 0.001),]
rare.U1F <- pick.U1F[which(pick.U1F$mean.abundance < 0.001),]
rare.U2B <- pick.U2B[which(pick.U2B$mean.abundance < 0.001),]
rare.U2F <- pick.U2F[which(pick.U2F$mean.abundance < 0.001),]
rare.U3B <- pick.U3B[which(pick.U3B$mean.abundance < 0.001),]
rare.U3F <- pick.U3F[which(pick.U3F$mean.abundance < 0.001),]

# 加标签，得到分类的OT表
type.abundant.D1B <- cbind.data.frame(rownames(abundant.D1B),rep("abundant", nrow(abundant.D1B)))
type.abundant.D1F <- cbind.data.frame(rownames(abundant.D1F),rep("abundant", nrow(abundant.D1F)))
type.abundant.D2B <- cbind.data.frame(rownames(abundant.D2B),rep("abundant", nrow(abundant.D2B)))
type.abundant.D2F <- cbind.data.frame(rownames(abundant.D2F),rep("abundant", nrow(abundant.D2F)))
type.abundant.D3B <- cbind.data.frame(rownames(abundant.D3B),rep("abundant", nrow(abundant.D3B)))
type.abundant.D3F <- cbind.data.frame(rownames(abundant.D3F),rep("abundant", nrow(abundant.D3F)))
type.abundant.M1B <- cbind.data.frame(rownames(abundant.M1B),rep("abundant", nrow(abundant.M1B)))
type.abundant.M1F <- cbind.data.frame(rownames(abundant.M1F),rep("abundant", nrow(abundant.M1F)))
type.abundant.M2B <- cbind.data.frame(rownames(abundant.M2B),rep("abundant", nrow(abundant.M2B)))
type.abundant.M2F <- cbind.data.frame(rownames(abundant.M2F),rep("abundant", nrow(abundant.M2F)))
type.abundant.M3B <- cbind.data.frame(rownames(abundant.M3B),rep("abundant", nrow(abundant.M3B)))
type.abundant.M3F <- cbind.data.frame(rownames(abundant.M3F),rep("abundant", nrow(abundant.M3F)))
type.abundant.U1B <- cbind.data.frame(rownames(abundant.U1B),rep("abundant", nrow(abundant.U1B)))
type.abundant.U1F <- cbind.data.frame(rownames(abundant.U1F),rep("abundant", nrow(abundant.U1F)))
type.abundant.U2B <- cbind.data.frame(rownames(abundant.U2B),rep("abundant", nrow(abundant.U2B)))
type.abundant.U2F <- cbind.data.frame(rownames(abundant.U2F),rep("abundant", nrow(abundant.U2F)))
type.abundant.U3B <- cbind.data.frame(rownames(abundant.U3B),rep("abundant", nrow(abundant.U3B)))
type.abundant.U3F <- cbind.data.frame(rownames(abundant.U3F),rep("abundant", nrow(abundant.U3F)))

type.transitional.D1B <- cbind.data.frame(rownames(transitional.D1B),rep("transitional", nrow(transitional.D1B)))
type.transitional.D1F <- cbind.data.frame(rownames(transitional.D1F),rep("transitional", nrow(transitional.D1F)))
type.transitional.D2B <- cbind.data.frame(rownames(transitional.D2B),rep("transitional", nrow(transitional.D2B)))
type.transitional.D2F <- cbind.data.frame(rownames(transitional.D2F),rep("transitional", nrow(transitional.D2F)))
type.transitional.D3B <- cbind.data.frame(rownames(transitional.D3B),rep("transitional", nrow(transitional.D3B)))
type.transitional.D3F <- cbind.data.frame(rownames(transitional.D3F),rep("transitional", nrow(transitional.D3F)))
type.transitional.M1B <- cbind.data.frame(rownames(transitional.M1B),rep("transitional", nrow(transitional.M1B)))
type.transitional.M1F <- cbind.data.frame(rownames(transitional.M1F),rep("transitional", nrow(transitional.M1F)))
type.transitional.M2B <- cbind.data.frame(rownames(transitional.M2B),rep("transitional", nrow(transitional.M2B)))
type.transitional.M2F <- cbind.data.frame(rownames(transitional.M2F),rep("transitional", nrow(transitional.M2F)))
type.transitional.M3B <- cbind.data.frame(rownames(transitional.M3B),rep("transitional", nrow(transitional.M3B)))
type.transitional.M3F <- cbind.data.frame(rownames(transitional.M3F),rep("transitional", nrow(transitional.M3F)))
type.transitional.U1B <- cbind.data.frame(rownames(transitional.U1B),rep("transitional", nrow(transitional.U1B)))
type.transitional.U1F <- cbind.data.frame(rownames(transitional.U1F),rep("transitional", nrow(transitional.U1F)))
type.transitional.U2B <- cbind.data.frame(rownames(transitional.U2B),rep("transitional", nrow(transitional.U2B)))
type.transitional.U2F <- cbind.data.frame(rownames(transitional.U2F),rep("transitional", nrow(transitional.U2F)))
type.transitional.U3B <- cbind.data.frame(rownames(transitional.U3B),rep("transitional", nrow(transitional.U3B)))
type.transitional.U3F <- cbind.data.frame(rownames(transitional.U3F),rep("transitional", nrow(transitional.U3F)))

type.rare.D1B <- cbind.data.frame(rownames(rare.D1B),rep("rare", nrow(rare.D1B)))
type.rare.D1F <- cbind.data.frame(rownames(rare.D1F),rep("rare", nrow(rare.D1F)))
type.rare.D2B <- cbind.data.frame(rownames(rare.D2B),rep("rare", nrow(rare.D2B)))
type.rare.D2F <- cbind.data.frame(rownames(rare.D2F),rep("rare", nrow(rare.D2F)))
type.rare.D3B <- cbind.data.frame(rownames(rare.D3B),rep("rare", nrow(rare.D3B)))
type.rare.D3F <- cbind.data.frame(rownames(rare.D3F),rep("rare", nrow(rare.D3F)))
type.rare.M1B <- cbind.data.frame(rownames(rare.M1B),rep("rare", nrow(rare.M1B)))
type.rare.M1F <- cbind.data.frame(rownames(rare.M1F),rep("rare", nrow(rare.M1F)))
type.rare.M2B <- cbind.data.frame(rownames(rare.M2B),rep("rare", nrow(rare.M2B)))
type.rare.M2F <- cbind.data.frame(rownames(rare.M2F),rep("rare", nrow(rare.M2F)))
type.rare.M3B <- cbind.data.frame(rownames(rare.M3B),rep("rare", nrow(rare.M3B)))
type.rare.M3F <- cbind.data.frame(rownames(rare.M3F),rep("rare", nrow(rare.M3F)))
type.rare.U1B <- cbind.data.frame(rownames(rare.U1B),rep("rare", nrow(rare.U1B)))
type.rare.U1F <- cbind.data.frame(rownames(rare.U1F),rep("rare", nrow(rare.U1F)))
type.rare.U2B <- cbind.data.frame(rownames(rare.U2B),rep("rare", nrow(rare.U2B)))
type.rare.U2F <- cbind.data.frame(rownames(rare.U2F),rep("rare", nrow(rare.U2F)))
type.rare.U3B <- cbind.data.frame(rownames(rare.U3B),rep("rare", nrow(rare.U3B)))
type.rare.U3F <- cbind.data.frame(rownames(rare.U3F),rep("rare", nrow(rare.U3F)))

colnames(type.abundant.D1B) <- c("ID", "type")
colnames(type.abundant.D1F) <- c("ID", "type")
colnames(type.abundant.D2B) <- c("ID", "type")
colnames(type.abundant.D2F) <- c("ID", "type")
colnames(type.abundant.D3B) <- c("ID", "type")
colnames(type.abundant.D3F) <- c("ID", "type")
colnames(type.abundant.M1B) <- c("ID", "type")
colnames(type.abundant.M1F) <- c("ID", "type")
colnames(type.abundant.M2B) <- c("ID", "type")
colnames(type.abundant.M2F) <- c("ID", "type")
colnames(type.abundant.M3B) <- c("ID", "type")
colnames(type.abundant.M3F) <- c("ID", "type")
colnames(type.abundant.U1B) <- c("ID", "type")
colnames(type.abundant.U1F) <- c("ID", "type")
colnames(type.abundant.U2B) <- c("ID", "type")
colnames(type.abundant.U2F) <- c("ID", "type")
colnames(type.abundant.U3B) <- c("ID", "type")
colnames(type.abundant.U3F) <- c("ID", "type")

colnames(type.transitional.D1B) <- c("ID", "type")
colnames(type.transitional.D1F) <- c("ID", "type")
colnames(type.transitional.D2B) <- c("ID", "type")
colnames(type.transitional.D2F) <- c("ID", "type")
colnames(type.transitional.D3B) <- c("ID", "type")
colnames(type.transitional.D3F) <- c("ID", "type")
colnames(type.transitional.M1B) <- c("ID", "type")
colnames(type.transitional.M1F) <- c("ID", "type")
colnames(type.transitional.M2B) <- c("ID", "type")
colnames(type.transitional.M2F) <- c("ID", "type")
colnames(type.transitional.M3B) <- c("ID", "type")
colnames(type.transitional.M3F) <- c("ID", "type")
colnames(type.transitional.U1B) <- c("ID", "type")
colnames(type.transitional.U1F) <- c("ID", "type")
colnames(type.transitional.U2B) <- c("ID", "type")
colnames(type.transitional.U2F) <- c("ID", "type")
colnames(type.transitional.U3B) <- c("ID", "type")
colnames(type.transitional.U3F) <- c("ID", "type")

colnames(type.rare.D1B) <- c("ID", "type")
colnames(type.rare.D1F) <- c("ID", "type")
colnames(type.rare.D2B) <- c("ID", "type")
colnames(type.rare.D2F) <- c("ID", "type")
colnames(type.rare.D3B) <- c("ID", "type")
colnames(type.rare.D3F) <- c("ID", "type")
colnames(type.rare.M1B) <- c("ID", "type")
colnames(type.rare.M1F) <- c("ID", "type")
colnames(type.rare.M2B) <- c("ID", "type")
colnames(type.rare.M2F) <- c("ID", "type")
colnames(type.rare.M3B) <- c("ID", "type")
colnames(type.rare.M3F) <- c("ID", "type")
colnames(type.rare.U1B) <- c("ID", "type")
colnames(type.rare.U1F) <- c("ID", "type")
colnames(type.rare.U2B) <- c("ID", "type")
colnames(type.rare.U2F) <- c("ID", "type")
colnames(type.rare.U3B) <- c("ID", "type")
colnames(type.rare.U3F) <- c("ID", "type")

type.D1B <- rbind.data.frame(type.abundant.D1B, type.transitional.D1B, type.rare.D1B)
type.D1F <- rbind.data.frame(type.abundant.D1F, type.transitional.D1F, type.rare.D1F)
type.D2B <- rbind.data.frame(type.abundant.D2B, type.transitional.D2B, type.rare.D2B)
type.D2F <- rbind.data.frame(type.abundant.D2F, type.transitional.D2F, type.rare.D2F)
type.D3B <- rbind.data.frame(type.abundant.D3B, type.transitional.D3B, type.rare.D3B)
type.D3F <- rbind.data.frame(type.abundant.D3F, type.transitional.D3F, type.rare.D3F)
type.M1B <- rbind.data.frame(type.abundant.M1B, type.transitional.M1B, type.rare.M1B)
type.M1F <- rbind.data.frame(type.abundant.M1F, type.transitional.M1F, type.rare.M1F)
type.M2B <- rbind.data.frame(type.abundant.M2B, type.transitional.M2B, type.rare.M2B)
type.M2F <- rbind.data.frame(type.abundant.M2F, type.transitional.M2F, type.rare.M2F)
type.M3B <- rbind.data.frame(type.abundant.M3B, type.transitional.M3B, type.rare.M3B)
type.M3F <- rbind.data.frame(type.abundant.M3F, type.transitional.M3F, type.rare.M3F)
type.U1B <- rbind.data.frame(type.abundant.U1B, type.transitional.U1B, type.rare.U1B)
type.U1F <- rbind.data.frame(type.abundant.U1F, type.transitional.U1F, type.rare.U1F)
type.U2B <- rbind.data.frame(type.abundant.U2B, type.transitional.U2B, type.rare.U2B)
type.U2F <- rbind.data.frame(type.abundant.U2F, type.transitional.U2F, type.rare.U2F)
type.U3B <- rbind.data.frame(type.abundant.U3B, type.transitional.U3B, type.rare.U3B)
type.U3F <- rbind.data.frame(type.abundant.U3F, type.transitional.U3F, type.rare.U3F)

colnames(type.D1B)[2] <- "D1B"
colnames(type.D1F)[2] <- "D1F"
colnames(type.D2B)[2] <- "D2B"
colnames(type.D2F)[2] <- "D2F"
colnames(type.D3B)[2] <- "D3B"
colnames(type.D3F)[2] <- "D3F"
colnames(type.M1B)[2] <- "M1B"
colnames(type.M1F)[2] <- "M1F"
colnames(type.M2B)[2] <- "M2B"
colnames(type.M2F)[2] <- "M2F"
colnames(type.M3B)[2] <- "M3B"
colnames(type.M3F)[2] <- "M3F"
colnames(type.U1B)[2] <- "U1B"
colnames(type.U1F)[2] <- "U1F"
colnames(type.U2B)[2] <- "U2B"
colnames(type.U2F)[2] <- "U2F"
colnames(type.U3B)[2] <- "U3B"
colnames(type.U3F)[2] <- "U3F"

type.D1 <- full_join(type.D1B, type.D1F,  by = "ID")
type.D2 <- full_join(type.D2B, type.D2F,  by = "ID")
type.D3 <- full_join(type.D3B, type.D3F,  by = "ID")
type.M1 <- full_join(type.M1B, type.M1F,  by = "ID")
type.M2 <- full_join(type.M2B, type.M2F,  by = "ID")
type.M3 <- full_join(type.M3B, type.M3F,  by = "ID")
type.U1 <- full_join(type.U1B, type.U1F,  by = "ID")
type.U2 <- full_join(type.U2B, type.U2F,  by = "ID")
type.U3 <- full_join(type.U3B, type.U3F,  by = "ID")

type.D12 <- full_join(type.D1, type.D2,by = "ID")
type.D <- full_join(type.D12, type.D3,by = "ID")
type.M12 <- full_join(type.M1, type.M2,by = "ID")
type.M <- full_join(type.M12, type.M3,by = "ID")
type.U12 <- full_join(type.U1, type.U2,by = "ID")
type.U <- full_join(type.U12, type.U3,by = "ID")

type.DM <- full_join(type.D, type.M,by = "ID")
classify <- full_join(type.DM, type.U,by = "ID")
rownames(classify) <- classify[,1]
classify <- classify[,-1]

classify[is.na(classify)] <- 0

sub.abundant.D1B <- subset(abundant.D1B,select=-mean.abundance)
sub.abundant.D1F <- subset(abundant.D1F,select=-mean.abundance)
sub.abundant.D2B <- subset(abundant.D2B,select=-mean.abundance)
sub.abundant.D2F <- subset(abundant.D2F,select=-mean.abundance)
sub.abundant.D3B <- subset(abundant.D3B,select=-mean.abundance)
sub.abundant.D3F <- subset(abundant.D3F,select=-mean.abundance)
sub.abundant.M1B <- subset(abundant.M1B,select=-mean.abundance)
sub.abundant.M1F <- subset(abundant.M1F,select=-mean.abundance)
sub.abundant.M2B <- subset(abundant.M2B,select=-mean.abundance)
sub.abundant.M2F <- subset(abundant.M2F,select=-mean.abundance)
sub.abundant.M3B <- subset(abundant.M3B,select=-mean.abundance)
sub.abundant.M3F <- subset(abundant.M3F,select=-mean.abundance)
sub.abundant.U1B <- subset(abundant.U1B,select=-mean.abundance)
sub.abundant.U1F <- subset(abundant.U1F,select=-mean.abundance)
sub.abundant.U2B <- subset(abundant.U2B,select=-mean.abundance)
sub.abundant.U2F <- subset(abundant.U2F,select=-mean.abundance)
sub.abundant.U3B <- subset(abundant.U3B,select=-mean.abundance)
sub.abundant.U3F <- subset(abundant.U3F,select=-mean.abundance)

sub.transitional.D1B <- subset(transitional.D1B,select=-mean.abundance)
sub.transitional.D1F <- subset(transitional.D1F,select=-mean.abundance)
sub.transitional.D2B <- subset(transitional.D2B,select=-mean.abundance)
sub.transitional.D2F <- subset(transitional.D2F,select=-mean.abundance)
sub.transitional.D3B <- subset(transitional.D3B,select=-mean.abundance)
sub.transitional.D3F <- subset(transitional.D3F,select=-mean.abundance)
sub.transitional.M1B <- subset(transitional.M1B,select=-mean.abundance)
sub.transitional.M1F <- subset(transitional.M1F,select=-mean.abundance)
sub.transitional.M2B <- subset(transitional.M2B,select=-mean.abundance)
sub.transitional.M2F <- subset(transitional.M2F,select=-mean.abundance)
sub.transitional.M3B <- subset(transitional.M3B,select=-mean.abundance)
sub.transitional.M3F <- subset(transitional.M3F,select=-mean.abundance)
sub.transitional.U1B <- subset(transitional.U1B,select=-mean.abundance)
sub.transitional.U1F <- subset(transitional.U1F,select=-mean.abundance)
sub.transitional.U2B <- subset(transitional.U2B,select=-mean.abundance)
sub.transitional.U2F <- subset(transitional.U2F,select=-mean.abundance)
sub.transitional.U3B <- subset(transitional.U3B,select=-mean.abundance)
sub.transitional.U3F <- subset(transitional.U3F,select=-mean.abundance)

sub.rare.D1B <- subset(rare.D1B,select=-mean.abundance)
sub.rare.D1F <- subset(rare.D1F,select=-mean.abundance)
sub.rare.D2B <- subset(rare.D2B,select=-mean.abundance)
sub.rare.D2F <- subset(rare.D2F,select=-mean.abundance)
sub.rare.D3B <- subset(rare.D3B,select=-mean.abundance)
sub.rare.D3F <- subset(rare.D3F,select=-mean.abundance)
sub.rare.M1B <- subset(rare.M1B,select=-mean.abundance)
sub.rare.M1F <- subset(rare.M1F,select=-mean.abundance)
sub.rare.M2B <- subset(rare.M2B,select=-mean.abundance)
sub.rare.M2F <- subset(rare.M2F,select=-mean.abundance)
sub.rare.M3B <- subset(rare.M3B,select=-mean.abundance)
sub.rare.M3F <- subset(rare.M3F,select=-mean.abundance)
sub.rare.U1B <- subset(rare.U1B,select=-mean.abundance)
sub.rare.U1F <- subset(rare.U1F,select=-mean.abundance)
sub.rare.U2B <- subset(rare.U2B,select=-mean.abundance)
sub.rare.U2F <- subset(rare.U2F,select=-mean.abundance)
sub.rare.U3B <- subset(rare.U3B,select=-mean.abundance)
sub.rare.U3F <- subset(rare.U3F,select=-mean.abundance)

sum.abundant.D1B <- colSums(sub.abundant.D1B)
sum.abundant.D1F <- colSums(sub.abundant.D1F)
sum.abundant.D2B <- colSums(sub.abundant.D2B)
sum.abundant.D2F <- colSums(sub.abundant.D2F)
sum.abundant.D3B <- colSums(sub.abundant.D3B)
sum.abundant.D3F <- colSums(sub.abundant.D3F)
sum.abundant.M1B <- colSums(sub.abundant.M1B)
sum.abundant.M1F <- colSums(sub.abundant.M1F)
sum.abundant.M2B <- colSums(sub.abundant.M2B)
sum.abundant.M2F <- colSums(sub.abundant.M2F)
sum.abundant.M3B <- colSums(sub.abundant.M3B)
sum.abundant.M3F <- colSums(sub.abundant.M3F)
sum.abundant.U1B <- colSums(sub.abundant.U1B)
sum.abundant.U1F <- colSums(sub.abundant.U1F)
sum.abundant.U2B <- colSums(sub.abundant.U2B)
sum.abundant.U2F <- colSums(sub.abundant.U2F)
sum.abundant.U3B <- colSums(sub.abundant.U3B)
sum.abundant.U3F <- colSums(sub.abundant.U3F)

sum.transitional.D1B <- colSums(sub.transitional.D1B)
sum.transitional.D1F <- colSums(sub.transitional.D1F)
sum.transitional.D2B <- colSums(sub.transitional.D2B)
sum.transitional.D2F <- colSums(sub.transitional.D2F)
sum.transitional.D3B <- colSums(sub.transitional.D3B)
sum.transitional.D3F <- colSums(sub.transitional.D3F)
sum.transitional.M1B <- colSums(sub.transitional.M1B)
sum.transitional.M1F <- colSums(sub.transitional.M1F)
sum.transitional.M2B <- colSums(sub.transitional.M2B)
sum.transitional.M2F <- colSums(sub.transitional.M2F)
sum.transitional.M3B <- colSums(sub.transitional.M3B)
sum.transitional.M3F <- colSums(sub.transitional.M3F)
sum.transitional.U1B <- colSums(sub.transitional.U1B)
sum.transitional.U1F <- colSums(sub.transitional.U1F)
sum.transitional.U2B <- colSums(sub.transitional.U2B)
sum.transitional.U2F <- colSums(sub.transitional.U2F)
sum.transitional.U3B <- colSums(sub.transitional.U3B)
sum.transitional.U3F <- colSums(sub.transitional.U3F)

sum.rare.D1B <- colSums(sub.rare.D1B)
sum.rare.D1F <- colSums(sub.rare.D1F)
sum.rare.D2B <- colSums(sub.rare.D2B)
sum.rare.D2F <- colSums(sub.rare.D2F)
sum.rare.D3B <- colSums(sub.rare.D3B)
sum.rare.D3F <- colSums(sub.rare.D3F)
sum.rare.M1B <- colSums(sub.rare.M1B)
sum.rare.M1F <- colSums(sub.rare.M1F)
sum.rare.M2B <- colSums(sub.rare.M2B)
sum.rare.M2F <- colSums(sub.rare.M2F)
sum.rare.M3B <- colSums(sub.rare.M3B)
sum.rare.M3F <- colSums(sub.rare.M3F)
sum.rare.U1B <- colSums(sub.rare.U1B)
sum.rare.U1F <- colSums(sub.rare.U1F)
sum.rare.U2B <- colSums(sub.rare.U2B)
sum.rare.U2F <- colSums(sub.rare.U2F)
sum.rare.U3B <- colSums(sub.rare.U3B)
sum.rare.U3F <- colSums(sub.rare.U3F)

sum.D1B <- t(cbind(sum.abundant.D1B, sum.transitional.D1B, sum.rare.D1B))
sum.D1F <- t(cbind(sum.abundant.D1F, sum.transitional.D1F, sum.rare.D1F))
sum.D2B <- t(cbind(sum.abundant.D2B, sum.transitional.D2B, sum.rare.D2B))
sum.D2F <- t(cbind(sum.abundant.D2F, sum.transitional.D2F, sum.rare.D2F))
sum.D3B <- t(cbind(sum.abundant.D3B, sum.transitional.D3B, sum.rare.D3B))
sum.D3F <- t(cbind(sum.abundant.D3F, sum.transitional.D3F, sum.rare.D3F))
sum.M1B <- t(cbind(sum.abundant.M1B, sum.transitional.M1B, sum.rare.M1B))
sum.M1F <- t(cbind(sum.abundant.M1F, sum.transitional.M1F, sum.rare.M1F))
sum.M2B <- t(cbind(sum.abundant.M2B, sum.transitional.M2B, sum.rare.M2B))
sum.M2F <- t(cbind(sum.abundant.M2F, sum.transitional.M2F, sum.rare.M2F))
sum.M3B <- t(cbind(sum.abundant.M3B, sum.transitional.M3B, sum.rare.M3B))
sum.M3F <- t(cbind(sum.abundant.M3F, sum.transitional.M3F, sum.rare.M3F))
sum.U1B <- t(cbind(sum.abundant.U1B, sum.transitional.U1B, sum.rare.U1B))
sum.U1F <- t(cbind(sum.abundant.U1F, sum.transitional.U1F, sum.rare.U1F))
sum.U2B <- t(cbind(sum.abundant.U2B, sum.transitional.U2B, sum.rare.U2B))
sum.U2F <- t(cbind(sum.abundant.U2F, sum.transitional.U2F, sum.rare.U2F))
sum.U3B <- t(cbind(sum.abundant.U3B, sum.transitional.U3B, sum.rare.U3B))
sum.U3F <- t(cbind(sum.abundant.U3F, sum.transitional.U3F, sum.rare.U3F))

rownames(sum.D1B) <- c("abundant", "transitional", "rare")
rownames(sum.D1F) <- c("abundant", "transitional", "rare")
rownames(sum.D2B) <- c("abundant", "transitional", "rare")
rownames(sum.D2F) <- c("abundant", "transitional", "rare")
rownames(sum.D3B) <- c("abundant", "transitional", "rare")
rownames(sum.D3F) <- c("abundant", "transitional", "rare")
rownames(sum.M1B) <- c("abundant", "transitional", "rare")
rownames(sum.M1F) <- c("abundant", "transitional", "rare")
rownames(sum.M2B) <- c("abundant", "transitional", "rare")
rownames(sum.M2F) <- c("abundant", "transitional", "rare")
rownames(sum.M3B) <- c("abundant", "transitional", "rare")
rownames(sum.M3F) <- c("abundant", "transitional", "rare")
rownames(sum.U1B) <- c("abundant", "transitional", "rare")
rownames(sum.U1F) <- c("abundant", "transitional", "rare")
rownames(sum.U2B) <- c("abundant", "transitional", "rare")
rownames(sum.U2F) <- c("abundant", "transitional", "rare")
rownames(sum.U3B) <- c("abundant", "transitional", "rare")
rownames(sum.U3F) <- c("abundant", "transitional", "rare")

sum.abundance <- cbind.data.frame(sum.D1B, sum.D1F, sum.D2B, sum.D2F, 
                                  sum.D3B, sum.D3F, sum.M1B, sum.M1F, 
                                  sum.M2B, sum.M2F, sum.M3B, sum.M3F, 
                                  sum.U1B, sum.U1F, sum.U2B, sum.U2F, 
                                  sum.U3B, sum.U3F)
colSums(sum.abundance)

count.abundant.D1B <- nrow(abundant.D1B)
count.abundant.D1F <- nrow(abundant.D1F)
count.abundant.D2B <- nrow(abundant.D2B)
count.abundant.D2F <- nrow(abundant.D2F)
count.abundant.D3B <- nrow(abundant.D3B)
count.abundant.D3F <- nrow(abundant.D3F)
count.abundant.M1B <- nrow(abundant.M1B)
count.abundant.M1F <- nrow(abundant.M1F)
count.abundant.M2B <- nrow(abundant.M2B)
count.abundant.M2F <- nrow(abundant.M2F)
count.abundant.M3B <- nrow(abundant.M3B)
count.abundant.M3F <- nrow(abundant.M3F)
count.abundant.U1B <- nrow(abundant.U1B)
count.abundant.U1F <- nrow(abundant.U1F)
count.abundant.U2B <- nrow(abundant.U2B)
count.abundant.U2F <- nrow(abundant.U2F)
count.abundant.U3B <- nrow(abundant.U3B)
count.abundant.U3F <- nrow(abundant.U3F)

count.transitional.D1B <- nrow(transitional.D1B)
count.transitional.D1F <- nrow(transitional.D1F)
count.transitional.D2B <- nrow(transitional.D2B)
count.transitional.D2F <- nrow(transitional.D2F)
count.transitional.D3B <- nrow(transitional.D3B)
count.transitional.D3F <- nrow(transitional.D3F)
count.transitional.M1B <- nrow(transitional.M1B)
count.transitional.M1F <- nrow(transitional.M1F)
count.transitional.M2B <- nrow(transitional.M2B)
count.transitional.M2F <- nrow(transitional.M2F)
count.transitional.M3B <- nrow(transitional.M3B)
count.transitional.M3F <- nrow(transitional.M3F)
count.transitional.U1B <- nrow(transitional.U1B)
count.transitional.U1F <- nrow(transitional.U1F)
count.transitional.U2B <- nrow(transitional.U2B)
count.transitional.U2F <- nrow(transitional.U2F)
count.transitional.U3B <- nrow(transitional.U3B)
count.transitional.U3F <- nrow(transitional.U3F)

count.rare.D1B <- nrow(rare.D1B)
count.rare.D1F <- nrow(rare.D1F)
count.rare.D2B <- nrow(rare.D2B)
count.rare.D2F <- nrow(rare.D2F)
count.rare.D3B <- nrow(rare.D3B)
count.rare.D3F <- nrow(rare.D3F)
count.rare.M1B <- nrow(rare.M1B)
count.rare.M1F <- nrow(rare.M1F)
count.rare.M2B <- nrow(rare.M2B)
count.rare.M2F <- nrow(rare.M2F)
count.rare.M3B <- nrow(rare.M3B)
count.rare.M3F <- nrow(rare.M3F)
count.rare.U1B <- nrow(rare.U1B)
count.rare.U1F <- nrow(rare.U1F)
count.rare.U2B <- nrow(rare.U2B)
count.rare.U2F <- nrow(rare.U2F)
count.rare.U3B <- nrow(rare.U3B)
count.rare.U3F <- nrow(rare.U3F)

count.abundant <- cbind.data.frame(count.abundant.D1B,
                                   count.abundant.D1F,
                                   count.abundant.D2B,
                                   count.abundant.D2F,
                                   count.abundant.D3B,
                                   count.abundant.D3F,
                                   count.abundant.M1B,
                                   count.abundant.M1F,
                                   count.abundant.M2B,
                                   count.abundant.M2F,
                                   count.abundant.M3B,
                                   count.abundant.M3F,
                                   count.abundant.U1B,
                                   count.abundant.U1F,
                                   count.abundant.U2B,
                                   count.abundant.U2F,
                                   count.abundant.U3B,
                                   count.abundant.U3F)
colnames(count.abundant) <- c("D1B", "D1F", "D2B", "D2F", "D3B", "D3F",
                              "M1B", "M1F", "M2B", "M2F", "M3B", "M3F",
                              "U1B", "U1F", "U2B", "U2F", "U3B", "U3F")

count.transitional <- cbind.data.frame(count.transitional.D1B,
                                       count.transitional.D1F,
                                       count.transitional.D2B,
                                       count.transitional.D2F,
                                       count.transitional.D3B,
                                       count.transitional.D3F,
                                       count.transitional.M1B,
                                       count.transitional.M1F,
                                       count.transitional.M2B,
                                       count.transitional.M2F,
                                       count.transitional.M3B,
                                       count.transitional.M3F,
                                       count.transitional.U1B,
                                       count.transitional.U1F,
                                       count.transitional.U2B,
                                       count.transitional.U2F,
                                       count.transitional.U3B,
                                       count.transitional.U3F)
colnames(count.transitional) <- c("D1B", "D1F", "D2B", "D2F", "D3B", "D3F",
                              "M1B", "M1F", "M2B", "M2F", "M3B", "M3F",
                              "U1B", "U1F", "U2B", "U2F", "U3B", "U3F")

count.rare <- cbind.data.frame(count.rare.D1B,
                               count.rare.D1F,
                               count.rare.D2B,
                               count.rare.D2F,
                               count.rare.D3B,
                               count.rare.D3F,
                               count.rare.M1B,
                               count.rare.M1F,
                               count.rare.M2B,
                               count.rare.M2F,
                               count.rare.M3B,
                               count.rare.M3F,
                               count.rare.U1B,
                               count.rare.U1F,
                               count.rare.U2B,
                               count.rare.U2F,
                               count.rare.U3B,
                               count.rare.U3F)
colnames(count.rare) <- c("D1B", "D1F", "D2B", "D2F", "D3B", "D3F",
                                  "M1B", "M1F", "M2B", "M2F", "M3B", "M3F",
                                  "U1B", "U1F", "U2B", "U2F", "U3B", "U3F")

count <- rbind.data.frame(count.abundant, count.transitional, count.rare)
rownames(count) <- c("abundant", "transitional", "rare")
type <- c("abundant", "transitional", "rare")
count <- cbind.data.frame(count, type)

write.table(classify, "feature_table_type.txt", col.names = NA, sep="\t")
write.table(sum.abundance, "feature_table_sum.abundance.txt", col.names = NA, sep="\t")
write.table(count, "feature_table_count.txt", col.names = NA, sep="\t")
