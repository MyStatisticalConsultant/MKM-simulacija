## Šta gledate

- **X:** logit(izlaznost), **Y:** logit(udeo glasova), **Z:** posterior
  za **Grupu3 (Extreme)**.
- Boja: pripadnost OM grupi (Grupa1/Grupa2/Grupa3).

## Kako čitati

- „Greben” visokih posteriora nad delom oblaka → model je **siguran** da
  je to ta grupa.
- Pljosnate površine bez vrhova → nesigurnost klasifikacije.

> **Preuzmi HTML** čuva interaktivni 3D (PNG za plotly zahteva dodatne
> alate – dostupno na zahtev).

------------------------------------------------------------------------

#### R primer (plotly 3D – ideja)

    # library(plotly)
    # plot_ly(df, x=~x, y=~y, z=~postK3, color=~cluster,
    #         type="scatter3d", mode="markers")
