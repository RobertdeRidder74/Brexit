verwacht_remain <- p * 1500
verwacht_remain
# werwachte_remain = 722 het verwachte totale aantal kiezers in de steekproef dat voor “Remain” kiest?
#-------------------------------------------------------------------------------
standaardfout_steekproef <- sqrt(1500 * p * (1-p))
standaardfout_steekproef
# standaardfout_steekproef = 19.4
#-------------------------------------------------------------------------------
#> Wat is de verwachtingswaarde van X_roof, het aandeel kiezers dat voor “Remain” stemt?
X_roof <- p
X_roof
# X_roof = 0.481
#-------------------------------------------------------------------------------
#> Wat is de standaardfout van , het aandeel kiezers dat voor “Remain” stemt?
standaardfout_remain <- sqrt(p * (1-p) / 1500)
standaardfout_remain
# standaardfout_remain = 0.0129
#-------------------------------------------------------------------------------
#> Wat is de verwachtingswaarde van d, het verschil tussen het aandeel “Remain”-kiezers en “Leave”-kiezers?
verwachtingswaarde_d <- p-(1-p)
verwachtingswaarde_d
# verwachtingswaarde_d = -0.038
#-------------------------------------------------------------------------------
#> Wat is de standaardfout van d, het verschil tussen het aandeel “Remain”-kiezers en “Leave”-kiezers?
standaardfout_d <- 2 * sqrt(p * (1-p) / 1500)
standaardfout_d
# standaardfout_d = 0.0258
mean(spread)
# mean(spread) = 0.0201
#-------------------------------------------------------------------------------
#> wat is de standaarddeviatie van de waargenomen spreiding?

sd(spread)
# de standaarddeviatie = 0.0588
#-------------------------------------------------------------------------------
# Wat is het gemiddelde van x_hat, de schattingen van de parameter p?
mean(x_hat)
# het gemiddelde van x_hat = 0.51
#-------------------------------------------------------------------------------
#Wat is de standaarddeviatie van x_hat?
sd(x_hat)
# de standaarddeviatie van x_hat = 0.0294
#-------------------------------------------------------------------------------
# Beschouw de eerste peiling in brexit_polls, een YouGov-peiling die werd 
# uitgevoerd op dezelfde dag als het Brexit-referendum:
brexit_polls[1,] 
N <- brexit_polls[1,]$samplesize
x_hat_poll <- brexit_polls[1,]$x_hat
# Gebruik qnorm() om het 95%-betrouwbaarheidsinterval voor x_hat te berekenen. 
# Wat is de ondergrens van het 95%-betrouwbaarheidsinterval?
ondergrens <- (x_hat_poll - qnorm(0.975) * sqrt(x_hat_poll * (1 - x_hat_poll) / N)
)
ondergrens
# ondergrens = 0.506
#-------------------------------------------------------------------------------
# Wat is de bovengrens van het 95%-betrouwbaarheidsinterval?
bovengrens <- (x_hat_poll + qnorm(0.975) * sqrt(x_hat_poll * (1 - x_hat_poll) / N)
)
bovengrens
# bovengrens = 0.534
#-------------------------------------------------------------------------------
# maak een dataframe die alleen peilingen bevat van 2016-06-01 of later.
june_polls <- brexit_polls |>
  filter(enddate >= "2016-06-01")
# Bereken de geschatte standaardfout van de geschatte waarde x_hat met zijn samplesize.
june_polls <- june_polls |>
  mutate(se_x_hat = sqrt(x_hat * (1 - x_hat) / samplesize))
# Bereken de standaardfout van de speiding, gegeven se_x_hat.
june_polls <- june_polls |>
  mutate(se_spread = 2 * se_x_hat)
# Bereken de boven en ondergrenzen voor het 95% betrouwbaarheids interval van de spreiding.
june_polls <- june_polls |>
  mutate(boven = (spread + qnorm(0.975) * se_spread), onder = (spread - qnorm(0.975) * se_spread))
# Maak een kolom "hit" die aangeeft of het betrouwbaarheids interval voor 
#elke peiling de correcte spreiding d = -0.038 omvat.
june_polls <- june_polls |>
  mutate(hit = (d >= onder & d <= boven))

# Hoeveel peilingen zitten er in june_polls ?
nrow(june_polls)
# er zitten 32 peilingen in june_polls

# Welk aandeel van de peilingen heeft een betrouwbaarheidsinterval dat de waarde 0 omvat?
mean(june_polls$onder <= 0 & june_polls$boven >= 0)
# Het aandeel van peilingen dat de waarde 0 omvat = 0.625

# Welk aandeel van de peilingen voorspelt "Remain" (betrouwbaarheidsinterval volledig boven 0)?
mean(june_polls$onder > 0)
# Het aandeel dat volledig bove 0 ligt = 0.125

# Welk aandeel van de peilingen heeft een betrouwbaarheidsinterval dat de werkelijke waarde van d omvat?
mean(june_polls$hit)
# het aandeel van peilingen dat de werkelijke waarde d omvat = 0.5625

# tabel
tibble(
  aantal_peilingen = nrow(june_polls),
  aandeel_omvat_0 = mean(june_polls$onder <= 0 & june_polls$boven >= 0),
  aandeel_remain = mean(june_polls$onder > 0),
  aandeel_hit_d = mean(june_polls$hit)
)

# Groepeer en vat het june_polls‑object samen per peiler (pollster) om het aandeel hits 
# per peiler en het aantal peilingen per peiler te vinden. Gebruik arrange() om te sorteren op hit‑ratio.
june_polls %>%
  group_by(pollster) %>%
  summarise(
    aantal_peilingen = n(),
    aandeel_hits = mean(hit)
  ) %>%
  arrange(desc(aandeel_hits))

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)
# Wat is de ondergrens van het 95%-betrouwbaarheidsinterval voor online‑kiezers?

se_p_hat <- sqrt(combined_by_type$p_hat * (1 - combined_by_type$p_hat) / combined_by_type$N)
se_combined_spread <- 2 * se_p_hat
ondergrens_spread <- combined_by_type$spread - qnorm(0.975) * se_combined_spread
ondergrens_online <- ondergrens_spread[combined_by_type$poll_type == "Online"]
ondergrens_online

ondergrens_spread_tel <- combined_by_type$spread - qnorm(0.975) * se_combined_spread
ondergrens_tel <- ondergrens_spread_tel[combined_by_type$poll_type == "Telephone"]
ondergrens_tel

# Wat is de bovengrens van het 95%-betrouwbaarheidsinterval voor online‑kiezers?
bovengrens_spread <- combined_by_type$spread + qnorm(0.975) * se_combined_spread
bovengrens_online <- bovengrens_spread[combined_by_type$poll_type == "Online"]
bovengrens_online

bovengrens_spread_tel <- combined_by_type$spread + qnorm(0.975) * se_combined_spread
bovengrens_tel <- bovengrens_spread_tel[combined_by_type$poll_type == "Telephone"]
bovengrens_tel
#-------------------------------------------------------------------------------
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

# Maak een kruistabel van poll_type en hit
tab <- table(brexit_hit$poll_type, brexit_hit$hit)

# Voer chi-kwadraat toets uit
chisq.test(tab)

online_odds <- tab["Online","TRUE"] / tab["Online","FALSE"]
online_odds

telephone_odds <- tab["Telephone","TRUE"] / tab["Telephone","FALSE"]
telephone_odds

odds_ratio <- online_odds / telephone_odds
odds_ratio

# Zet data in long format
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
