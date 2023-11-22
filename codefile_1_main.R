#..............................................................................#
#     C O D E F I L E
#     Vice Versa: The Decoupling of Content and Topic Heterogeneity in Collusion Research
#     W. Benedikt Schmal, Research Group Lead at Walter Eucken Institute, Freiburg (DE)
#     schmal@eucken.de      https://sites.google.com/view/wbschmal
#     Last changes: November 20, 2023
#..............................................................................#

## Load packages----
library(readtext)
library(readxl)
library(openxlsx)
library(ggplot2)
library(xtable)
library(tidyverse)
library(haven)
library(data.table)
library(webshot)
library(dplyr)
library(ddpcr)
library(texreg)

library(fixest)
library(did)
library(estimatr)
require(pscl)
require(MASS)
require(boot)
library(mfx)
library(marginaleffects)
library(fwildclusterboot)
library(tseries)
library(aTSA)
library(matrixStats)
library(strucchange)

library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
require(quanteda.corpora)
library(wordcloud)
library(textdata)
library(ldatuning)
library(topicmodels)
library(stm)
require(seededlda)
require(lubridate)
library(tidytext)
library(doc2vec)
library(huge)

# Working Directory
setwd("YOUR PATH")
workflow_note1 = " > > > EDIT CSV FILE BEFORE PROCEEDING < < < "

# General Preparation ----
stopwordlist = c("â", "commissionã¢€™", "ncaaã¢€™", "incumbentsã¢€™", "circuitã¢€™", "ncaaã¢€™", "incumbentsã¢€™", "among", "north-holland", "blackwell", "rand", "llc", "institute", "springer", "sciencebusiness", "academic", "press", "literature", "editorial", "mine", "supervisor", "ltd", "journal", "iii", "section", "â©", "ã", "can", "show", "collusion", "cartel", "cartels", "due", "eu", "elsevier", "collusive", "paper", "will", "may", "discuss")
set.seed(1896)

# load and prepare data
at_data <- read.csv("at_io.csv")
at_data <- subset(at_data, year>1999) # reduce to data from 2000 on
data <- at_data
datacoll <- subset(data, coll_present==1) # 777 papers, reduce to data with collusion in it (see def. in paper)

#prepare text corpus
processed <- textProcessor(datacoll$description, metadata = datacoll, customstopwords = stopwordlist )
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
data <- datacoll # reduce dataset to collusion records


#
# Find optimal K for the number of underlying topics ----
#
minK <- 5 # starting value (must by >1)
maxK <- 40 # upper value 
K10fulls <- as.data.frame(c(minK:maxK))
K10fulls <- as.data.frame(c(minK:maxK))
K10fulle <- as.data.frame(c(minK:maxK))
K10fullh <- as.data.frame(c(minK:maxK))
K10fullr <- as.data.frame(c(minK:maxK))

set.seed(1896)
storage_full <- searchK(out$documents, out$vocab, K = c(minK: maxK), prevalence =~ jrnl + year, data = meta, max.em.its = 75)
lapply(names(storage_full$results), function(x){
  storage_full$results[[x]] <<- unlist(storage_full$results[[x]])
  return(NULL)
})
Kmetrics_h <- as.data.frame(storage_full$results)
K10full <- as.data.frame(c(minK:maxK))
K10full <- cbind(K10full, Kmetrics_h$semcoh)
K10full <- cbind(K10full, Kmetrics_h$exclus)
K10full <- cbind(K10full, Kmetrics_h$heldout)
K10full <- cbind(K10full, Kmetrics_h$resid)

colnames(K10full)[1] <- "K"
colnames(K10full)[2] <- "semcoh"
colnames(K10full)[3] <- "exclus"
colnames(K10full)[4] <- "heldout"
colnames(K10full)[5] <- "resid"


# Plot semantic coherence and exclusivity across the defined K range:
help.plot = data.frame(x = c(K10full$K, K10full$K),
                       y = c(K10full[ ,3], K10full[ ,2]),
                       axis = c(rep("exclus", nrow(K10full)), rep("semcoh", nrow(K10full))))

help.plot$y[help.plot$axis == "semcoh"] <- help.plot$y[help.plot$axis == "semcoh"]/(10)
help.plot$y[help.plot$axis == "semcoh"] <- help.plot$y[help.plot$axis == "semcoh"]+16

ggplot(help.plot, aes(x = x, y = y, group = axis, color = axis, linetype = axis)) +
  geom_line() + ylab("Exclusivity") +
  scale_y_continuous(limits = c(2.5,10.5), sec.axis = sec_axis(~.-15, name = "Semantic Coherence")) +
  scale_linetype_manual("", values = c("exclus" = "solid", "semcoh" = "dashed"),
                        labels = c("exclus" = "Exclusivity", "semcoh" = "Semantic Coherence")) +
  scale_color_manual("", values = c("exclus" = "red", "semcoh" = "deepskyblue2"),
                     labels = c("exclus" = "Exclusivity", "semcoh" = "Semantic Coherence")) +
  ggtitle("\n Exclusivity and Coherence for the text corpus as a function of K \n") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(size=12), axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title.y.right = element_text(size=12, angle = 90), axis.text.y.right = element_text(size=12),
                     axis.ticks.length=unit(0.15, "cm"), legend.text = element_text(size = 12, margin = margin(r = 25, unit = "pt")),
                     legend.key.height = unit(0.75, "cm"), legend.key.width = unit(1, "cm"), legend.spacing.x = unit(0.15, "cm"),
                     legend.position = "bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,-10,0,-10))
ggsave(filename = "io_at_semcohexclus.png", plot = last_plot(), scale = 1, dpi = 600)

# compute changes in addition to nominal values of semantic coherence and exclusivity (see Appendix B)
K2 <- K10full
K2 <- K2[-c(1:2), ]

K2$SEratio <- K2$semcoh/K2$exclus
K2$DSE <- K2$SEratio - lag(K2$SEratio,1)
K2$wSER <- K2$DSE/K2$SEratio
K2[is.na(K2)] <- 0
K2 <- K2[-c(1), ]

K3 <- subset(K2, wSER<0)

ifelse(K2$semcoh[K2$K>0] == max(K2$semcoh[K2$wSER<0]), print(K2$K),print(""))

help.plot = data.frame(x = c(K2$K, K2$K),
                       y = c(K2$DSE, K2$wSER),
                       axis = c(rep("DSE", nrow(K2)), rep("wSER", nrow(K2), na.rm=TRUE)))

help.plot$y[help.plot$axis == "wSER"] <- help.plot$y[help.plot$axis == "wSER"]*10

ggplot(help.plot, aes(x = x, y = y, group = axis, color = axis, linetype = axis)) + xlab("K") + ylab(expression(Delta~"SER"))+
  geom_line() +
  scale_y_continuous(limits = c(-1,1.0), sec.axis = sec_axis(~.*.1, name = "wSER")) +
  scale_linetype_manual("", values = c("DSE" = "solid", "wSER" = "dashed"),
                        labels = c("DSE" = expression(Delta~"SER"), "wSER" = "wSER")) +
  scale_color_manual("", values = c("DSE" = "red", "wSER" = "deepskyblue2"),
                     labels = c("DSE" = expression(Delta~"SER"), "wSER" = "wSER")) +
  ggtitle(expression(Delta~"SER and wSER of the text corpus" )) +
  geom_hline(yintercept = 0, color = "gray") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(size=12), axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title.y.right = element_text(size=12, angle = 90), axis.text.y.right = element_text(size=12),
                     axis.ticks.length=unit(0.15, "cm"), legend.text = element_text(size = 12, margin = margin(r = 25, unit = "pt")),
                     legend.key.height = unit(0.75, "cm"), legend.key.width = unit(1, "cm"), legend.spacing.x = unit(0.15, "cm"),
                     legend.position = "bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,-10,0,-10))
ggsave(filename = "wdse_io_at.png", plot = last_plot(), scale = 1, dpi = 600)


#
#estimate main STM (structural topic model) ----
#
set.seed(1896)
K_star <- 21 # number of topics to be estimated
stm_iofull <- stm(out$documents, out$vocab, K = K_star, prevalence =~ jrnl + year, max.em.its = 75, data = out$meta, init.type = "Spectral")
# jrnl describes the exact journal in which a publication appeared

K21topics <- labelTopics(stm_iofull, c(1:num(K_star)))
K21topics # display topics

# compute correlations across topics
mod.out.corr <- topicCorr(stm_iofull)
mod.out.corr
xtable(mod.out.corr$cor)

# Wordcloud for 'introduction' section
# topic 16 - leniency
cloud(
  stm_iofull,
  topic = 16,
  type = c("model"),
  max.words = 75,
  colors = "darkred"
)
# note that this cloud varies in presentation even if a fixed seed is set

## Robustness Check: topics K = 10
stm_10 <- stm(out$documents, out$vocab, K = 10, prevalence =~ jrnl + year, max.em.its = 75, data = out$meta, init.type = "Spectral")
K10topics <- labelTopics(stm_10, c(1:10))
K10topics

#
### 3.2 Topic prevalence over time ----
#
# Extract Topic probabilities
topicprob <- tidy(stm_iofull, matrix = "gamma", document_names = rownames(data), log = FALSE) # 16317 obs = 777*21

tp_h <- c(1:length(data$eid))
print(length(tp_h)) # must be length of dataset >> 777 papers
dim(tp_h) <- c(max(tp_h),1)
colnames(tp_h)[1] <- "document"
write.csv(topicprob, "document_probs_replication.csv", row.names = FALSE) # use individual path to file folder, export file for transformation
print(workflow_note1)
# >> EDIT CSV file before proceeding in STATA (see STATA script "reshape topic_gammas.do" !


# re-import transformed CSV file (file transformed in STATA, see above)
data <- cbind(data, c(1:length(data$eid))) # document identifier
colnames(data)[46] <- "docum"
gamma_vals <- read.csv("document_probs_transformed_replication.csv")
gamma_vals <- cbind(gamma_vals, c(1:length(data$eid)))
colnames(gamma_vals)[K_star+2] <- "docum" 
data <- merge(data,gamma_vals, by ="docum") # add topic probabilities to main dataset

test1 <- as.data.frame(aggregate((data$topic1+data$topic2+data$topic3+data$topic4+data$topic5 + data$topic6+data$topic7+data$topic8+data$topic9+data$topic10+data$topic11 + data$topic12 + data$topic13 + data$topic14 + data$topic15 + data$topic16 + data$topic17 + data$topic18 + data$topic19 + data$topic20 + data$topic21), list(data$year), mean))
test1
# backup check: all topics have to sum up to 1. Satisfied.

#
dataB <- data #backup command to duplicate "data" file
# competition analysis
attr1 <- aggregate(dataB$topic4, list(dataB$year), mean)
attr2 <- aggregate(dataB$topic7, list(dataB$year), mean)
attr3 <- aggregate(dataB$topic11, list(dataB$year), mean)
attr4 <- aggregate(dataB$topic12, list(dataB$year), mean)
attr5 <- aggregate(dataB$topic18, list(dataB$year), mean)
attr6 <- aggregate(dataB$topic19, list(dataB$year), mean)
rules <- attr1$x + attr2$x + attr3$x + attr4$x + attr5$x + attr6$x

#exogenous topics
attr1 <- aggregate(dataB$topic1, list(dataB$year), mean)
attr2 <- aggregate(dataB$topic2, list(dataB$year), mean)
attr3 <- aggregate(dataB$topic6, list(dataB$year), mean)
exog <- attr1$x + attr2$x + attr3$x


#
### Time Series Tests ----
#
# "competition analysis" topics
quiet(te1 <- aTSA::adf.test(rules))
te1 <- do.call(rbind.data.frame, te1)
xtable(te1)
quiet(te2 <- pp.test(rules))
te3 <- tseries::kpss.test(rules)
te3 <- do.call(cbind.data.frame, te3)
te3 <- te3[, c(2,1,3)]
colnames(te1)[1] <- "lag"
colnames(te1)[2] <- "test statistic"
colnames(te1)[3] <- "p-value"
colnames(te2)[1] <- "lag"
colnames(te2)[2] <- "test statistic"
colnames(te2)[3] <- "p-value"
colnames(te3)[1] <- "lag"
colnames(te3)[2] <- "test statistic"
colnames(te3)[3] <- "p-value"
te <- rbind(te1, te2, te3)
xtable(te, digits=c(0,0,4,4), type="latex", label="tab.", caption = "ADD CAPTION")

# "external" topics (i.e., 1,2,6)
quiet(te1 <- aTSA::adf.test(exog))
te1 <- do.call(rbind.data.frame, te1)
xtable(te1)
quiet(te2 <- pp.test(exog))
te3 <- tseries::kpss.test(exog)
te3 <- do.call(cbind.data.frame, te3)
te3 <- te3[, c(2,1,3)]
colnames(te1)[1] <- "lag"
colnames(te1)[2] <- "test statistic"
colnames(te1)[3] <- "p-value"
colnames(te2)[1] <- "lag"
colnames(te2)[2] <- "test statistic"
colnames(te2)[3] <- "p-value"
colnames(te3)[1] <- "lag"
colnames(te3)[2] <- "test statistic"
colnames(te3)[3] <- "p-value"
te <- rbind(te1, te2, te3)
xtable(te, digits=c(0,0,4,4), type="latex", label="tab.", caption = "topics 1,2,6 - exog - ADD CAPTION")

# Chow Tests for structural breaks - competition analysis (footnote 19)
compmeans <- as.data.frame(aggregate(dataB$topic19, list(dataB$year), mean))
attr1 <- aggregate(dataB$topic4, list(dataB$year), mean)
attr2 <- aggregate(dataB$topic7, list(dataB$year), mean)
attr3 <- aggregate(dataB$topic11, list(dataB$year), mean)
attr4 <- aggregate(dataB$topic12, list(dataB$year), mean)
attr5 <- aggregate(dataB$topic18, list(dataB$year), mean)
attr6 <- aggregate(dataB$topic19, list(dataB$year), mean)
compmeans <- cbind(compmeans, (attr1 + attr2 + attr3 + attr4 + attr5 + attr6))
compmeans <- compmeans[,-c(2,3)]
colnames(compmeans)[1] <- "year"
colnames(compmeans)[2] <- "val"

strucchange::sctest(compmeans$val ~ compmeans$year, type = "Chow", point = 10) #2009
strucchange::sctest(compmeans$val ~ compmeans$year, type = "Chow", point = 11) #2010
strucchange::sctest(compmeans$val ~ compmeans$year, type = "Chow", point = 12) #2011

###
# Plot 1 : Topic Probabilities over time
yearlymeans <- as.data.frame(aggregate((data$topic10+data$topic13+data$topic14+data$topic15+data$topic21 + data$topic4+data$topic7+data$topic11+data$topic12+data$topic18+data$topic19), list(data$year), mean))
attr5b <-aggregate((data$topic5 + data$topic16 + data$topic20), list(data$year), mean)
attr6b <- aggregate((data$topic9 + data$topic17), list(data$year), mean)
attr7b <- aggregate((data$topic3+ data$topic8), list(data$year), mean)
attr3b <-aggregate((data$topic1 + data$topic2 + data$topic6), list(data$year), mean)
yearlymeans <- cbind(yearlymeans, c(attr5b, attr6b, attr7b, attr3b))
yearlymeans <- yearlymeans[,-c(3, 5, 7, 9)]

ggplot(yearlymeans, aes(x=Group.1)) + 
  geom_line(aes(y=(x)), col="black", linetype="solid",size = 1.4, group=1) + #outside cartel (attributes of the market + competition analysis)
  geom_line(aes(y=x.1), col="darkgray", linetype="dashed",size = 1.2, group=1) + #inside cartel
  geom_line(aes(y=x.2), col="darkgray", linetype="dotted",size = 1.2, group=1) + #interactions
  geom_line(aes(y=x.3), col="darkgray", linetype="solid",size = 1.2, group=1) + #outcomes
  geom_line(aes(y=x.4), col="darkred", linetype="solid",size = 1.4, group=1) + #case studies
  theme_bw() + theme(legend.title = element_blank()) + 
  scale_color_brewer(palette="Set1") + labs(x="Year", y="Expected Probabilites", title="Aggregated Expected Topic Probabilities by Category") + theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = "topic_shares_full.png", plot = last_plot(), scale = 1, dpi = 600) 


# Plot 2: Outside the Cartel Variables
dataB <- data
yearlymeans <- as.data.frame(aggregate(dataB$topic19, list(dataB$year), mean)) # just use it as initial variable to set up the data frame

attr1 <- aggregate(dataB$topic10, list(dataB$year), mean)
attr2 <- aggregate(dataB$topic13, list(dataB$year), mean)
attr3 <- aggregate(dataB$topic14, list(dataB$year), mean)
attr4 <- aggregate(dataB$topic15, list(dataB$year), mean)
attr5 <- aggregate(dataB$topic21, list(dataB$year), mean)

yearlymeans <- cbind(yearlymeans, (attr1 + attr2 + attr3 + attr4 + attr5))

attr1 <- aggregate(dataB$topic4, list(dataB$year), mean)
attr2 <- aggregate(dataB$topic7, list(dataB$year), mean)
attr3 <- aggregate(dataB$topic11, list(dataB$year), mean)
attr4 <- aggregate(dataB$topic12, list(dataB$year), mean)
attr5 <- aggregate(dataB$topic18, list(dataB$year), mean)
attr6 <- aggregate(dataB$topic19, list(dataB$year), mean)
yearlymeans <- cbind(yearlymeans, (attr1 + attr2 + attr3 + attr4 + attr5 + attr6))
yearlymeans <- yearlymeans[,-c(3,5)]

ggplot(yearlymeans, aes(x=Group.1)) + 
  geom_line(aes(y=x.1), col="darkgray", linetype="dashed", size = 1, group=1) + ylim(0,.6) +
  geom_line(aes(y=x.2), col="black", linetype="solid", size = 1.2, group=1) + 
  geom_smooth(aes(y=x.2),method = "lm", se = TRUE, col = "darkred", linetype="dotted", size=1.1) +
  theme_bw() + theme(legend.title = element_blank()) + theme(legend.position="bottom") + 
  scale_color_brewer(palette="Set1") + labs(x="Year", y="Expected Probabilites", title="Expected Topic Probabilities for Topics 'Outside the Cartel'") + theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = "topic_shares_exog2.png", plot = last_plot(), scale = 1, dpi = 600) # use individual path to file folder


# Plot 3: Topics outside the framework
os1 <- aggregate(dataB$topic1, list(dataB$year), mean)
os2 <- aggregate(dataB$topic2, list(dataB$year), mean)
os3 <- aggregate(dataB$topic6, list(dataB$year), mean)
ym <- as.data.frame(aggregate(dataB$topic1, list(dataB$year), mean))
ym <- cbind(ym, (os1))
ym <- cbind(ym, (os2))
ym <- cbind(ym, (os3))
ym <- ym[,-c(1,2,5,7)]

ggplot(ym, aes(x=Group.1)) + 
  geom_line(aes(y=(x)), col="darkgray", linetype="solid",size = 1, group=1) + 
  geom_line(aes(y=x.1), col="darkgray", linetype="dashed", group=1, size=1.05) + 
  geom_line(aes(y=x.2), col="darkgray", linetype="dotted", group=1, size = 1.055) + 
  geom_line(aes(y=x + x.1 + x.2), col="darkred", linetype="solid", size = 1.1, group=1) +  # sum of topics 1, 2, and 6
  geom_vline(xintercept=2009, linetype="dotted", col = "black") + 
  theme_bw() + theme(legend.title = element_blank()) + theme(legend.position="bottom") + 
  scale_color_brewer(palette="Set1") + labs(x="Year", y="Expected Probabilites", title="Aggregated Expected Probabilities: Case Study Topics") + theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = "topic_shares_outside.png", plot = last_plot(), scale = 1, dpi = 600) # use individual path to file folder

outmeans <- as.data.frame(aggregate(dataB$topic1, list(dataB$year), mean))
attr1 <- aggregate(dataB$topic1, list(dataB$year), mean)
attr2 <- aggregate(dataB$topic2, list(dataB$year), mean)
attr3 <- aggregate(dataB$topic6, list(dataB$year), mean)
outmeans <- cbind(outmeans, (attr1 + attr2 + attr3))
outmeans <- outmeans[,-c(2,3)]
colnames(outmeans)[1] <- "year"
colnames(outmeans)[2] <- "val"

strucchange::sctest(outmeans$val ~ outmeans$year, type = "Chow", point = 10) #2009 -- significant at 5% level
strucchange::sctest(outmeans$val ~ outmeans$year, type = "Chow", point = 11) #2010 -- non sign.
strucchange::sctest(outmeans$val ~ outmeans$year, type = "Chow", point = 12) #2011 -- non sign.

###

# Plot 4 - Kernel Density Plot

ltexog <- as.data.frame(log(data$topic1+data$topic2+data$topic6))
ltexog$year <- data$year
ltexog$id <- ifelse(ltexog$year<=2010, "one", "two")
colnames(ltexog)[1]<-"ex"
groupcol <- c("one" = "darkred", "two" = "black")

mean(ltexog$ex[ltexog$year<=2010])
mean(ltexog$ex[ltexog$year>2010])
ggplot(ltexog, aes(x=ex, group = id, color = id)) + 
  geom_density( size = 1) + xlim(-15, 0.02) +
  geom_vline(xintercept = mean(ltexog$ex[ltexog$year<=2010]), linetype="dashed", color = "darkred") +
  geom_vline(xintercept = mean(ltexog$ex[ltexog$year>2010]), linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dotted", col = "black") +
  scale_colour_manual(values = groupcol) +
  theme_bw()+   labs(y="Kernel Density", title="Probability Density Funtions: Case Study Topics", x="log(Exp. Pr.(Topic 1 + Topic 2 + Topic 6))") + geom_hline(yintercept=0, linetype="dotted", col = "black") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none") + theme(legend.title=element_blank())
ggsave(filename = "pdf_density_126.png", plot = last_plot(), scale = 1, dpi = 600) # use individual path to file folder

ggplot(ltexog, aes(x=ex, group = id, color = id)) + 
  stat_ecdf( size = 1) + xlim(-15, 0.02) + 
  geom_vline(xintercept = mean(ltexog$ex[ltexog$year<=2010]), linetype="dashed", color = "darkred") +
  geom_vline(xintercept = mean(ltexog$ex[ltexog$year>2010]), linetype="dashed", color = "black") +
  geom_hline(yintercept=0, linetype="dotted", col = "black") + 
  geom_vline(xintercept=0, linetype="dotted", col = "black") +
  scale_colour_manual(values = groupcol) +
  theme_bw() + labs(y="Kernel Density", title="Cumulative Density Funtions: Case Study Topics", x="log(Exp. Pr. (Topic 1 + Topic 2 + Topic 6))") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none") + theme(legend.title=element_blank())
ggsave(filename = "cdf_density_126.png", plot = last_plot(), scale = 1, dpi = 600) # use individual path to file folder

# Kolmogorov Smirnov Test 
ks.test(ltexog$ex[ltexog$id=="one"], ltexog$ex[ltexog$id=="two"], alternative = c("greater")) # D = 0.33521

#
# 3.3 - Topic Correlations with journal types ----
#
set.seed(1896)
# compute correlations with journal types
effect <- estimateEffect(1:num(K_star) ~  jtype2 + year, stm_iofull, meta = out$meta, uncertainty = "Global") # adjust stm model
coefdata <- plot(effect, covariate = "jtype2")
# jtype2 = 5 categories instead of single journals

# extract the correlations
coef_corr <- data.frame()
for (j in 1:K_star) {
  coef_corr<- rbind(coef_corr, coefdata$means[[j]])
  print(max(j))
}
coef_corr <- cbind(coef_corr, coefdata$topics)

ci_corr <- data.frame()
interval <- c("lower", "upper")
for (j in 1:K_star) {
  ci_corr<- rbind(ci_corr, coefdata$cis[[j]])
  print(max(j))
}

ci_corr <- cbind(ci_corr, matrix(rep(coefdata$topics,each = 2), ncol = 1, byrow = TRUE))
ci_corr <- cbind(ci_corr, rep(interval))

colnames(coef_corr)[1] <- "at"
colnames(coef_corr)[2] <- "field"
colnames(coef_corr)[3] <- "genint"
colnames(coef_corr)[4] <- "io"
colnames(coef_corr)[5] <- "top5"
colnames(coef_corr)[6] <- "topic"

colnames(ci_corr)[1] <- "at"
colnames(ci_corr)[2] <- "field"
colnames(ci_corr)[3] <- "genint"
colnames(ci_corr)[4] <- "io"
colnames(ci_corr)[5] <- "top5"
colnames(ci_corr)[7] <- "interval"

# vary between the following 5 "data7" frames
# Main text
data7 <- as.data.frame(cbind(coef_corr$top5, ci_corr$top5[ci_corr$interval == "lower"], ci_corr$top5[ci_corr$interval == "upper"], coef_corr$topic))
data7 <- as.data.frame(cbind(coef_corr$genint, ci_corr$genint[ci_corr$interval == "lower"], ci_corr$genint[ci_corr$interval == "upper"], coef_corr$topic))
# Appendix
data7 <- as.data.frame(cbind(coef_corr$field, ci_corr$field[ci_corr$interval == "lower"], ci_corr$field[ci_corr$interval == "upper"], coef_corr$topic))
data7 <- as.data.frame(cbind(coef_corr$io, ci_corr$io[ci_corr$interval == "lower"], ci_corr$io[ci_corr$interval == "upper"], coef_corr$topic))
data7 <- as.data.frame(cbind(coef_corr$at, ci_corr$at[ci_corr$interval == "lower"], ci_corr$at[ci_corr$interval == "upper"], coef_corr$topic))

colnames(data7)[1] <- "coef"
colnames(data7)[2] <- "lower"
colnames(data7)[3] <- "upper"
colnames(data7)[4] <- "topic"

ggplot(data7, aes(x=topic)) + geom_point(aes(y=coef), size=3, col = "darkred") + 
  geom_line(aes(y=coef), col="grey", linetype="dashed") + geom_hline(yintercept=0) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, size=1, col="darkred") +  xlab("Topic Number") + ylab("Correlation") + ylim(-.05,.252) +
  ggtitle("\n Correlation between Latent Topics and 'IO' Journals \n") + scale_x_continuous(breaks = seq(min(data7$topic),max(data7$topic), by = 1)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(size=12), axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                     axis.title.y.right = element_text(size=12, angle = 90), axis.text.y.right = element_text(size=12),
                     axis.ticks.length=unit(0.15, "cm"), legend.text = element_text(size = 12, margin = margin(r = 25, unit = "pt")),
                     legend.key.height = unit(0.75, "cm"), legend.key.width = unit(1, "cm"), legend.spacing.x = unit(0.15, "cm"),
                     legend.position = "bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,-10,0,-10))
ggsave(filename = "corr_io.png", plot = last_plot(), scale = 1, dpi = 600) # vary plot name according to data7 variation


# Add Paper categories # (See appendix)
add_cats <- read_excel("coll_papers_manual_BS.xlsx")
add_cats <- subset(add_cats, select = c(2, 38) )
data_test <- merge(data,add_cats, by="eid", all.x= TRUE) # add alternative paper classification to dataset
data_backup <- data
data <- data_test

paper_cats <- read.csv("paper_cats.csv")
groupcol <- c("empirical" = "darkred", "theoretical" = "black", "experimental" = "gray")
ggplot(paper_cats, aes(x = year, y = count, group = type, color = type)) + geom_line(size = 1.1) +
  theme_bw() + labs(y="Frequency", title="Frequency of Paper Types over Time", x="") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(size=12), axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
        axis.title.y.right = element_text(size=12, angle = 90), axis.text.y.right = element_text(size=12),
        axis.ticks.length=unit(0.15, "cm"), legend.text = element_text(size = 12, margin = margin(r = 25, unit = "pt")),
        legend.key.height = unit(0.75, "cm"), legend.key.width = unit(1, "cm"), legend.spacing.x = unit(0.15, "cm"),
        legend.position = "bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,-10,0,-10)) +
        scale_colour_manual(values = groupcol)
ggsave(filename = "paper_types.png", plot = last_plot(), scale = 1, dpi = 600)

#
# 3.4 Topic Correlation with citations ----
#
corr_auth <- read_excel("iodat_with_corresponding_author.xlsx")
colnames(corr_auth)[47] <- "docum"
corr_auth <- corr_auth %>%
  dplyr::select(docum, surname)
data <- merge(data,corr_auth, by="docum", all.x= TRUE)

data$cy <- data$citedby_count / (2022 - data$year) # use 2022 as reference year as data was obtained in April 2022
data$cy_b <- ((data$citedby_count +1) / (2022 - data$year) ) # generate log(1+y) values 

data$lcy <- as.numeric(log(data$cy))
data$lcy[is.na(data$lcy) | data$lcy=="-Inf" |data$lcy=="NaN"] = NA # replace infinite values for log(0) with "NA"
data$lcy_b <- as.numeric(log((data$citedby_count+1) / (2022 - data$year)))
data$surname[is.na(data$surname) | data$surname=="-Inf" | data$surname=="Inf" |data$surname=="NaN"] = NA
data$surname[is.na(data$surname)] <- data$author_ids[is.na(data$surname)] # replace corr. author variable with author IDs if surname corr. author is missing ("NA")

data$lt1 <- log(data$topic1)
data$lt2 <- log(data$topic2)
data$lt3 <- log(data$topic3)
data$lt4 <- log(data$topic4)
data$lt5 <- log(data$topic5)
data$lt6 <- log(data$topic6)
data$lt7 <- log(data$topic7)
data$lt8 <- log(data$topic8)
data$lt9 <- log(data$topic9)
data$lt10 <- log(data$topic10)
data$lt11 <- log(data$topic11)
data$lt12 <- log(data$topic12)
data$lt13 <- log(data$topic13)
data$lt14 <- log(data$topic14)
data$lt15 <- log(data$topic15)
data$lt16 <- log(data$topic16)
data$lt17 <- log(data$topic17)
data$lt18 <- log(data$topic18)
data$lt19 <- log(data$topic19)
data$lt19 <- log(data$topic19)
data$lt20 <- log(data$topic20)
data$lt21 <- log(data$topic21)

data$yj <- paste(data$year, data$jrnl)
data  <- tidyr::separate(data, author_names, c('at1', 'at2', 'at3', 'at4', 'at5'), sep = ';') 
hsin <- log(data$cy+sqrt((data$cy)^2+1))

# Generate citation histograms (Appendix Figs. 16 and 17)
data_c <- as.data.frame(data$citedby_count)
colnames(data_c)[1] <- "citedby_count"
ggplot(data_c, aes(x=citedby_count)) + 
  geom_histogram(aes(y=..density..), colour="black", alpha=.7, fill="darkred") + xlim(0,100) +
  geom_density(alpha=.5, fill="darkred", size=1.1) + xlab("Citations") + ylab("Density") + 
  theme_bw() +theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(size=12), axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                    axis.title.y.right = element_text(size=12, angle = 90), axis.text.y.right = element_text(size=12)) +
  ggtitle("\n Histogram of Paper Citations up to 100 citations \n")
ggsave(filename = "citecount.png", plot = last_plot(), path = "USERPATH", scale = 1, dpi = 600)

data_c <- cbind(data_c, data$lcy)
colnames(data_c)[2] <- "lcy"

ggplot(data_c, aes(x=lcy)) + 
  geom_histogram(aes(y=..density..), colour="black", alpha=.7, fill="darkred") + xlim(-4,4) +
  geom_density(alpha=.5, fill="darkred", size=1.1) + xlab("log(citations/year)") + ylab("Density") + 
  theme_bw() +theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(size=12), axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                    axis.title.y.right = element_text(size=12, angle = 90), axis.text.y.right = element_text(size=12)) +
  ggtitle("\n Histogram of Citations per Year (logarithmic) \n")
ggsave(filename = "lcy.png", plot = last_plot(), path = "USERPATH", scale = 1, dpi = 600)


# attention: switch between independent variables

# work manually through all 21 (logged) topics
fe1 <- feols(log(cy) ~ lt19 + openaccess  + I(surname)| yj , cluster = ~yj, data = data)
fe1
fe2 <- feols(log(cy + 1) ~ lt19+ openaccess + I(surname) | yj  , cluster = ~yj, data = data)
fe2
fe3 <- feols(log(cy+sqrt((cy)^2+1)) ~ lt19 + openaccess + I(surname)  | yj, cluster = ~yj, data = data)
fe3
### 3, 11, 19 significantly negative correlation with citation measure
 

##rerun regs above to store the correct values from "fe2" and "fe3"
#fe2
coef <- fe2$coefficients[1]
se <- fe2$se[1]
plot2 <- as.data.frame(cbind(coef, se))
names(plot2) <- c("coef", "se")
plot2$coef <- as.numeric(plot2$coef)
plot2$se <- as.numeric(plot2$se)
plot2$group <- "fe2"
plot2$group2 <- "T19" # adjust for each of the three topics

#fe3
coef <- fe3$coefficients[1]
se <- fe3$se[1]
plot3 <- as.data.frame(cbind(coef, se))
names(plot3) <- c("coef", "se")
plot3$coef <- as.numeric(plot3$coef)
plot3$se <- as.numeric(plot3$se)
plot3$group <- "fe3"
plot3$group2 <- "T19" # adjust for each of the three topics

# FIGURE 9
#plot <- rbind(plot2, plot3) #use this line for the first reg on lt3, afterwords the following
plot <- rbind(plot, plot2, plot3)
plot$g3 <- c(0.9,1.1,1.9,2.1,2.9,3.1) #, 3.9, 4.1)
plot$group <- rep(c("log(c/y + 1) transf.", "hyperbolic sine transf."), times = 3)
groupcol <- c("log(c/y + 1) transf." = "darkred", "hyperbolic sine transf." = "black")

ggplot(plot, aes(g3, coef)) + geom_point(aes(col=group), size=3.5) + 
  geom_errorbar(aes(col=group, ymax=coef+1.64*se, ymin=coef-1.64*se), width=0.1, size=1.1) + geom_errorbar(aes(col= group, ymax=coef+1.96*se, ymin=coef-1.96*se), width=0.1) +scale_fill_manual(values=groupcol)+
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(size=12), axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
                           axis.title.y.right = element_text(size=12, angle = 90), axis.text.y.right = element_text(size=12),
                           axis.ticks.length=unit(0.15, "cm"), legend.text = element_text(size = 12, margin = margin(r = 25, unit = "pt")),
                           legend.key.height = unit(0.75, "cm"), legend.key.width = unit(1, "cm"), legend.spacing.x = unit(0.15, "cm"),
                           legend.position = "bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,-10,0,-10))+
  labs(y="Effect on citations", title="Relationship between Topics and Citations per Year", x="") + geom_hline(yintercept=0, linetype="dashed", col = "red") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="bottom") + theme(legend.title=element_blank()) +
  scale_colour_manual(values = groupcol)
ggsave(filename = "cite_eff.png", plot = last_plot(), scale = 1, dpi = 600) 

## Topic Probabilities over time for negative topics (3 - 11 - 19)
# FIGURE 10
ym2 <- as.data.frame(aggregate(data$topic19, list(data$year), mean))
dataB <- data
attr1 <- aggregate(dataB$topic3, list(dataB$year), mean)
attr2 <- aggregate(dataB$topic11, list(dataB$year), mean)
attr3 <- aggregate(dataB$topic19, list(dataB$year), mean)

ym2 <- cbind(ym2, (attr1 + attr2 + attr3))
ym2 <- ym2[,-c(2,3)]

ggplot(ym2, aes(x=Group.1)) + 
  geom_smooth(aes(y=x),method = "lm", se = TRUE, col = "darkred", size=1.1, linetype="dotted") +
  geom_line(aes(y=x), col="black", linetype="solid", size = 1.1, group=1) + 
  theme_bw() + theme(legend.title = element_blank()) + 
  scale_color_brewer(palette="Set1") + labs(x="Year", y="Expected Probabilites", title="Aggregated Expected Topic Probabilities For Topics related to Citations") + theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = "topic_shares_negsic.png", plot = last_plot(), scale = 1, dpi = 600)

# Time series Tests for this aggregated topic probability development
# "lost" topics, i.e., 3, 11, and 19
attr1 <- aggregate(dataB$topic3, list(dataB$year), mean)
attr2 <- aggregate(dataB$topic11, list(dataB$year), mean)
attr3 <- aggregate(dataB$topic19, list(dataB$year), mean)

lost <- attr1$x + attr2$x + attr3$x

quiet(te1 <- aTSA::adf.test(lost))
te1 <- do.call(rbind.data.frame, te1)
xtable(te1)
quiet(te2 <- pp.test(lost))
te3 <- tseries::kpss.test(lost)
te3 <- do.call(cbind.data.frame, te3)
te3 <- te3[, c(2,1,3)]
colnames(te1)[1] <- "lag"
colnames(te1)[2] <- "test statistic"
colnames(te1)[3] <- "p-value"
colnames(te2)[1] <- "lag"
colnames(te2)[2] <- "test statistic"
colnames(te2)[3] <- "p-value"
colnames(te3)[1] <- "lag"
colnames(te3)[2] <- "test statistic"
colnames(te3)[3] <- "p-value"
te <- rbind(te1, te2, te3)
xtable(te, digits=c(0,0,4,4), type="latex", label="tab.", caption = "topics 3,11,19 - lost - ADD CAPTION")

#
# DOC2VEC Neural Network----
#
# I use three dimensions of the target vector 50, 100, and 500. I adjust the code accordingly

corrsbyyear <- data.frame()
set.seed(1896)
for (j in min(data$year) : max(data$year)){
  x <- as.data.frame(data$docum[data$year== j])
  x <- cbind(x, data$description[data$year== j])
  colnames(x)[1] <- "doc_id"
  colnames(x)[2] <- "text"
  corpus_dma <- corpus(x$text)
  tok_dma <- tokens(corpus_dma, remove_numbers = TRUE,  remove_punct = TRUE, remove_separators = TRUE, remove_symbols = TRUE)
  tok_dma <- tokens_wordstem(tok_dma, language = quanteda_options("language_stemmer"))
  ttt <- vapply(tok_dma, paste, collapse = " ", character(1))
  tt2 <- as.data.frame(ttt)
  x <- cbind(x, tt2$ttt)
  x <- x[-c(2)]
  colnames(x)[2] <- "text"
  model <- paragraph2vec(x = x, type = "PV-DM", dim = 50, iter = 20)  # !dimension of target vector! 50 - 100 - 500
  emb <- as.matrix(model, which = "docs")
  x1 <- paragraph2vec_similarity(emb, emb)
  diag(x1) = NA
  doccorr <- rowMeans(x1,
                      na.rm = TRUE)
  hv1 <- cbind(x$doc_id, doccorr)
  colnames(hv1)[1]<-"docum"
  corrsbyyear <- rbind(corrsbyyear, hv1)
}

data_ac <- cbind(data, corrsbyyear)
attr1 <- as.data.frame(aggregate(data_ac$doccorr, list(data_ac$year), mean))

#linear regression for change in avg content similarity over time based on the computed annual averages
reg_attr1 <- lm_robust(x ~ I(Group.1), attr1)
reg_attr1 # coef: -0.0199, SE 0.0026, p-value =  1.797591e-07 = 0.0016

ggplot(attr1, aes(x=Group.1)) + 
  geom_line(aes(y=(x)), col="black", linetype="solid", size = 1.1, group=1) +  ylim(.2,.8) + # Specification: PV-DM
  geom_smooth(aes(y=x),method = "lm", formula = y ~ poly(x, 3), se = TRUE, col = "darkred", size=1.1, linetype="dotted") +
  theme_bw() + theme(legend.title = element_blank()) + theme(legend.position="bottom") +
  scale_color_brewer(palette="Set1") + labs(x="Year", y="Relative Similarity", title="Average Similarity of Publications over Time") + theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = "neural50.png", plot = last_plot(), scale = 1, dpi = 600)

# Time series econometrics for content similarity time series
quiet(te1 <- aTSA::adf.test(attr1$x))
te1 <- do.call(rbind.data.frame, te1)
xtable(te1)
quiet(te2 <- pp.test(attr1$x))
te3 <- tseries::kpss.test(attr1$x)
te3 <- do.call(cbind.data.frame, te3)
te3 <- te3[, c(2,1,3)]
colnames(te1)[1] <- "lag"
colnames(te1)[2] <- "test statistic"
colnames(te1)[3] <- "p-value"
colnames(te2)[1] <- "lag"
colnames(te2)[2] <- "test statistic"
colnames(te2)[3] <- "p-value"
colnames(te3)[1] <- "lag"
colnames(te3)[2] <- "test statistic"
colnames(te3)[3] <- "p-value"
te <- rbind(te1, te2, te3)
xtable(te, digits=c(0,0,4,4), type="latex", label="tab.", caption = "ADD CAPTION")

#
### 3.5 Reciprocal topic prevalence over time >> done in STATA ----
#
# export datafile for STATA
df2 <-  as.data.frame(data[,c(52:72)]) # export topic probabilities again (could also be done with transformed dataset)
df2 <- cbind(df2, data$year)
write.csv(df2,"var_data.csv", row.names = FALSE)

#..............................................................................#
#.............................. END OF CODE FILE  .............................#
#..............................................................................#
