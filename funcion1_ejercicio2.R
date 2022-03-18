reg_genplots <- function (gen) {
  if (gen == "GATA3"){ total <- list (
    lapply(plots_cancer, function(x) {ggplot(x, aes(GATA3, PTEN)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(GATA3, XBP1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(GATA3, ESR1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(GATA3, MUC1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(GATA3, FN1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(GATA3, GAPDH)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}))
  }
  else if (gen == "PTEN"){total <- list (
    lapply(plots_cancer, function(x) {ggplot(x, aes(PTEN, GATA3)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(PTEN, XBP1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(PTEN, ESR1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(PTEN, MUC1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(PTEN, FN1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(PTEN, GAPDH)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}))
  }
  else if (gen == "XBP1"){total <- list (
    lapply(plots_cancer, function(x) {ggplot(x, aes(XBP1, GATA3)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(XBP1, PTEN)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(XBP1, ESR1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(XBP1, MUC1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(XBP1, FN1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(XBP1, GAPDH)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}))
  }
  else if (gen == "ESR1"){total <- list (
    lapply(plots_cancer, function(x) {ggplot(x, aes(ESR1, GATA3)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(ESR1, PTEN)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(ESR1, XBP1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(ESR1, MUC1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(ESR1, FN1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(ESR1, GAPDH)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}))
  }
  else if (gen == "MUC1"){total <- list (
    lapply(plots_cancer, function(x) {ggplot(x, aes(MUC1, GATA3)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(MUC1, PTEN)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(MUC1, XBP1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(MUC1, ESR1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(MUC1, FN1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(MUC1, GAPDH)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}))
  }
  else if (gen == "FN1"){total <- list (
    lapply(plots_cancer, function(x) {ggplot(x, aes(FN1, GATA3)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(FN1, PTEN)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(FN1, XBP1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(FN1, ESR1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(FN1, MUC1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(FN1, GAPDH)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}))
  }
  else if (gen == "GAPDH"){total <- list (
    lapply(plots_cancer, function(x) {ggplot(x, aes(GAPDH, GATA3)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(GAPDH, PTEN)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(GAPDH, XBP1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(GAPDH, ESR1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(GAPDH, MUC1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}), 
    lapply(plots_cancer, function(x) {ggplot(x, aes(GAPDH, FN1)) + 
        geom_point() + geom_smooth(method='lm', formula= y~x)}))
  }
}