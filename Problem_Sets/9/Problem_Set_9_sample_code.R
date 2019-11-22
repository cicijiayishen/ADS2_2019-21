temp <- readtxt("C:/Users/sissy/Desktop/test Git/ADS2_2019-21/Problem_Sets/9/barley.txt")
barley <- scan("C:/Users/sissy/Desktop/test Git/ADS2_2019-21/Problem_Sets/9/barley.txt")
t.test(barley,mu = 50)
head(barley)

sampling_means<-vector()
for (replicate in 1:100){
  barley_sample<-sample(barley, size = length(barley), replace = TRUE)
  sampling_means<-c(sampling_means, mean(barley_sample))
}
hist(sampling_means, xlab = "Sample means", main = "")

shapiro.test(sampling_means)

sampling_errors<-vector()
sampling_means<-vector()
for (replicate in 1:100){
  barley_sample<-sample(barley, size = length(barley), replace = TRUE)
  standard_error<-sd(barley_sample)/sqrt(length(barley_sample))
  sampling_errors<-c(sampling_errors, standard_error)
  sampling_means<-c(sampling_means, mean(barley_sample))
}
plot(sampling_means, sampling_errors, xlab = "Sample mean", ylab = "Standard error")
lmfit<-lm(sampling_errors~sampling_means)
abline(lmfit, col = 'red')

summary(lmfit)

sig_results<-vector()
for (sample_size in 5:50){
  found_sigs<-0
  for (replicate in 1:100){
    barley_sample<-sample(barley, size = sample_size, replace = FALSE)
    p_value<-t.test(barley_sample, mu = 50)$p.value
    if (p_value <= 0.05){
      found_sigs = found_sigs + 1
    }
  }
  sig_results<-c(sig_results, found_sigs)
}
plot(sig_results, type = 'l', ylab = "No. significant results/100", xlab = "Sample size")
abline(h = 95, col = 'red')

min(which(sig_results >= 95))
