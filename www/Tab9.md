## Entropija klasifikacije

- **Srednja entropija** meri prosečnu neodlučnost:
  - **Niže** = sigurnija dodela grupi,  
  - **Više** = više preklapanja (teže razdvajanje).

## Konture gustina (u logit prostoru)

- Elipse prate oblik i pravac rasipanja.  
- **Uska, izdužena elipsa** (duž dijagonale) → jaka *t–v* veza (visok
  ρ).  
- **Šira/kružnija** → slabija veza i veća nesigurnost.

## Saveti

- Za **nižu entropiju** u grupi prevare:
  - povećajte **ρ**,  
  - pomerite **μ** (kroz *α\_t, β\_t*) da se oblak prevare udalji od
    Regular.

------------------------------------------------------------------------

#### R primer (entropija)

    entropy_row <- function(p) -sum(p * log(p + 1e-15))
    mean_entropy <- function(P) mean(apply(P, 1, entropy_row))

    # primer:
    set.seed(1234)
    P <- matrix(runif(300), nrow=100, ncol=3); P <- P/rowSums(P)
    mean_entropy(P)

    ## [1] 0.9457793
