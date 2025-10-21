## Šta pokazuju

Za svaku grupu posebno (Regular, Incremental, Extreme) crtamo krivu
operativnih karakteristika prijemnika (eng. *ROC - Receiver Operating
Characteristic*) gde je „pozitivna” ta klasa, a „negativne” su ostale.

> **U statistici, ROC kriva** je grafički alat koji se koristi za
> procenu performansi binarnog modela klasifikacije (model koji predviđa
> dva moguća ishoda, kao što su pozitivno/negativno).  
> ROC kriva prikazuje stopu stvarno pozitivnih rezultata, poznatu i kao
> osetljivost ili priziv, na y-osi, u odnosu na stopu lažno pozitivnih
> rezultata, koja je jednaka 1 − specifičnost, na x-osi. Svaka tačka na
> krivoj odgovara različitom pragu koji se koristi za klasifikaciju
> predviđanja kao pozitivnih ili negativnih.

## Tumačenje

- **Dijagonala (45°)** → slučajna klasifikacija (AUC ≈ 0.5).
- **Konkavna, visoka kriva** → dobra diskriminacija (AUC → 1).
- **Savršen model** bi dostigao tačku u gornjem levom uglu grafika
  (Osetljivost/Sensitivity = 1, Specifičnost/Specificity = 0), sa AUC od
  1,0, što znači besprekornu diskriminaciju između grupa.
- Slaba ROC (≈ 0.5) ukazuje na sličnost grupa ili mali uzorak →
  pojačajte **ρ** i/ili pomerite **α\_t, β\_t**.

------------------------------------------------------------------------

#### R primer (pROC)

    # library(pROC)
    # y <- as.integer(truth == "Extreme")   # 0/1
    # s <- posterior[ , 3]                  # verovatnoće za Extreme
    # ro <- roc(y, s); auc(ro)
