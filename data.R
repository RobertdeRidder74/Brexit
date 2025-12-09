overvieuw <- "In juni 2016 hield het Verenigd Koninkrijk (VK) een referendum om
te bepalen of het land in de Europese Unie (EU) zou blijven (“Remain”) of de EU 
zou verlaten (“Leave”). Dit referendum staat algemeen bekend als Brexit. Hoewel 
de media en anderen de peilingsresultaten interpreteerden als een voorspelling 
van “Remain” (P > 0.5), was het daadwerkelijke aandeel dat voor “Remain” stemde slechts 
48,1% (P = 0.481). Het VK stemde dus om de EU te verlaten. Peilingbureaus in het VK werden 
bekritiseerd omdat zij de steun voor “Remain” hadden overschat."
#-------------------------------------------------------------------------------
# suggested libraries and options
library(tidyverse)
library(writexl)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

spread <- brexit_polls |> pull(spread)
x_hat <- (spread + 1) / 2

brexit_polls <- brexit_polls %>%
  mutate(x_hat = x_hat)
# Exporteer naar Excel
write_xlsx(brexit_polls, "C:/Users/mono-/OneDrive/Bureaublad/portfolio/Brexit analyse/data/brexit_polls.xlsx")
