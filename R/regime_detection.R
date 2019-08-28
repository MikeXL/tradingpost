data = Cl(MSFT['2018-01-01/'])
returns = dailyReturn(data)

library(depmixS4)

hmm = depmix(daily.returns ~ 1, family = gaussian(), nstates = 2, data=returns)
hmmfit = fit(hmm, verbose=F)
post.probs = posterior(hmmfit)

layout(1:3)
plot(returns, main="daily returns")
plot(post.probs$state, type="s", main = "True Regime")

matplot(post.probs[, -1], type="l", main = "Regime Posterior Probabilities")
legend('topright', c('bear', 'bull'), fill=1:2, bty='n')
