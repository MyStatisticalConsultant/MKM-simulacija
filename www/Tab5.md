## Prikazano

- **Ponderi (π\_k):** ocenjeni udeli grupa (Regular / Incremental /
  Extreme).
- **Sredine (μ\_k) u LOGIT prostoru:** vektori *\[logit\_t, logit\_v\]*
  po grupi.
- **Kovarijacione matrice (Σ\_k) u LOGIT prostoru:**
  varijanse/kovarijanse po grupi sa imenovanim osama.

Ovi parametri su rezultat OM algoritma nad podacima u logit prostoru.
Eliptični oblici grupa i njihove orijentacije odražene su u μ i Σ.

## Ocene u ORIGINALNOM prostoru (t, v)

Za lakše tumačenje, aplikacija izračunava i **sredine** i
**kovarijacione matrice** po komponentama u **originalnom** prostoru
(*t, v*), kao i **mešavinske** (globalne) momente:

- **Sredine po komponentama:**
  𝔼\[*t*<sub>*k*</sub>\], 𝔼\[*v*<sub>*k*</sub>\]
- **Kovarijacije po komponentama:**
  Var(*t*<sub>*k*</sub>), Var(*v*<sub>*k*</sub>), Cov(*t*<sub>*k*</sub>, *v*<sub>*k*</sub>)
- **Mešavinski momenti:** 𝔼\[*t*\], 𝔼\[*v*\] i Cov(*t*, *v*) pod ukupnom
  mešavinom

> Napomena: originalni prostor dobijamo nelinearnom mapom *ilogit* nad
> logit-normalnim komponentama. Zatvorene forme za momente su retko
> dostupne, pa se koristi brza i robusna **Monte-Carlo** aproksimacija
> po komponenti.

## Kako čitati

- **Veliki π\_Regular + mala Σ\_Regular** u logit prostoru → dominira
  „normalan” obrazac; u originalnom prostoru očekujte umeren 𝔼\[*t*\] i
  stabilna raspršenost.
- **Prevare**: pomeren μ (ka većim logit vrednostima) i često veće Σ; u
  originalnom prostoru to vodi većoj 𝔼\[*t*\], 𝔼\[*v*\] i izraženijoj
  korelaciji Cov(*t*, *v*).
- **Mešavinski** (globalni) momenti u originalnom prostoru pomažu pri
  brzom poređenju sa agregatnim statistikama iz realnih podataka.

------------------------------------------------------------------------

#### R skica (čitanje iz objekta)

    # Pretpostavimo da je 'res' EM rezultat:
    res$lambda         # ponderi (π)
    res$mu             # liste μ_k sa imenima c("logit_t","logit_v")
    res$sigma          # liste Σ_k sa imenovanim redovima/kolonama

    # Momenti u originalnom prostoru momenti se prikazuju i u posebnoj tabeli (Tab “Marginalni rasporedi”),
    # uključujući komponentne i mešavinske (globalne) vrednosti.
