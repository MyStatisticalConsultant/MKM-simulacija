# Simulacija MKM — Igralište za forenziku izbora (Shiny aplikacija)

Interaktivna Shiny aplikacija za **forenziku izbora** zasnovana na **modelu konačne mešavine (MKM) Waltera Mebanea, Jr.**
Aplikacija omogućava da **simulirate podatke na nivou biračkih mesta** u širokom rasponu scenarija — od regularnih izbora do **ekstremne prevare** — podešavanjem **parametara Beta rasporeda** koje određuju izlaznost i udeo glasova pobednika u svakoj komponenti mešavine. Zatim možete **pregledati numeričke rezimee i vizuelne dijagnostike** i u **originalnom prostoru** (izlaznost, glasovi pobednika) i u **logit prostoru**, oceniti MKM na simuliranim podacima i **izvesti** rezultate (uključujući **izveštaj o prevari**, **MKM procene** i **sirove simulirane podatke**) za dalju analizu u drugim alatima.

> Putem uživo: **https://statisticar-mkm-simulacija.share.connect.posit.cloud**  
> Povratne informacije: **Zlatko@MyStatisticalConsultant.com**

## Zašto ova aplikacija?

Istraživači, analitičari, novinari i svi koje interesuje integritet izbora mogu „da se igraju” pretpostavkama na utemeljen način:

- **Upoznajte mehaniku MKM-a** gledajući kako ponderi komponenti i **Beta(α, β)** parametri oblikuju ishode po biračkim mestima.

- **Testirajte dijagnostiku** kroz scenarije: regularno, blage nepravilnosti, obrasci „punjenja kutija”, premeštanja glasova ili **ekstremne prevare**.

- **Povežite sa postojećim tokovima rada**: izvezite tabelarne rezultate i *CSV* podatke za nastavak analize u R-u, Python-u ili specijalizovanim forenzičkim paketima.

Okosnicu čini pristup MKM (Walter Mebane, Jr.), koji **modelira ishode biračkih mesta kao mešavinu** latentnih režima (npr. regularno vs. prevara) sa **komponentno specifičnim Beta rasporedima** za izlaznost i udeo glasova pobednika.

## Šta možete da radite

- **Podesite parametre simulacije**
   - Izaberite **broj komponenti mešavine** (npr. regularno + jedan ili više režima prevare).
   - Za svaku komponentu podesite **Beta(α, β)** parametre za:
      - **Izlaznost** (kao procenat)
      - **Udeo glasova pobednika** (u odnosu na izlaznost)
   - Odredite **pondere komponenti** (udeo biračkih mesta po komponenti) i **broj biračkih mesta** za simulaciju.
   - Opcione gajke za **intenzitet/oblik prevare** (npr. maksimalna stopa „punjenja”).
- **Pokrenite simulaciju i pregledajte**:
   - **Tabelarne rezimee** (po komponentama i ukupno).
   - **Grafike u originalnom prostoru**: raspršeni/gustina/toplotna mapa odnosa izlaznost–glasovi pobednika, marginale rasporede, empirijsku funkciju rasporeda itd.
   - **Grafike u logit prostoru**: transformišite varijable u logite da biste uočili strukture tipične za MKM dijagnostiku.
- **Izvoz**
   - **Izveštaj o prevari** (ključne dijagnostike + narativni rezime)
   - **MKM ocene** (parametri, gde je moguće i standardne greške)
   - **Sirovi simulirani podaci (CSV)** za dalju analizu

## Struktura aplikacije (ukratko)

- **Bočna panel**
   - Veličina simulacije i „seed” za iniciranje simulacije
   - Ponderi komponenti
   - **Beta(α, β)** parametri za **izlaznost** i **glasove pobednika** po komponenti
   - Opcioni kontroleri specifični za prevaru (npr. maks. stopa punjenja)
   - Dugmad Pokreni/Reset
   - Dugmad za preuzimanje (Izveštaj, Procene, Podaci)
- **Glavni paneli**
   1. **Pregled** — izabrani parametri i brze statistike
   2. **Grafici (originalni prostor)** — odnos izlaznost–glasovi pobednika + marginalni rasporedi
   3. **Grafici (logit prostor)** — iste promenljive posle logit transformacije
   4. **Tabele** — rezimei po komponenti, kvantili, provere
   5. **MKM ocena** — rezultati ocene mešavine
   6. **Izveštaj o prevari** — narativ + ključne metrike; preuzimanje kao Word dokument
   7. **Podaci** — pregled + izvoz simuliranog skupa podataka

> **Savet**: Krenite od „regularnih izbora”, a zatim postepeno uvodite **scenarije prevare**. Posmatrajte kako se menjaju dijagnostike i MKM ocene kada „vrtite” **Beta** oblike i **pondere**.

## Preuzimanja

- **Izveštaj o prevari**: Word (`.docx`) dokument sa vašim parametrima, vizuelizacijama i interpretacijom.
- **MKM ocene**: Udeli i parametri komponenti (sa dijagnostikom).
- **Sirovi simulacioni podaci (CSV)**: Izlaznost i glasovi pobednika po biračkom mestu (i izvedene varijable); možete ih koristiti u drugim forenzičkim alatima.

## Pokretanje lokalno

### 1) Klonirajte repozitorijum

```bash
git clone https://github.com/MyStatisticalConsultant/MKM-simulacija.git
cd MKM-simulacija
```
### 2) Otvorite u RStudio

- Otvorite `app.R` (ili RStudio projekat ako postoji).
- Proverite `DESCRIPTION` / `renv.lock` ako postoje; u suprotnom instalirajte pakete ispod.

### 3) Instalirajte pakete

```r
install.packages(c(
  "shiny", "shinydashboard",
  "ggplot2", "scales", "gridExtra",
  "mixtools",     # ili drugi paket za mešavine/optimizaciju
  "stats4",       # MLE pomoćnici, ako se koriste
  "readr",        # za CSV import/izvoz
  "dplyr", "tidyr",
  "kableExtra", "knitr", "rmarkdown"
))
```

> Lista se može neznatno razlikovati u zavisnosti od vašeg `DESCRIPTION`. Ako koristite **renv**, pokrenite `renv::restore()`.

### 4) Pokrenite aplikaciju

```r
shiny::runApp()    # ili klik na "Run App" u RStudio
```

Konzola će prikazati lokalni URL (npr. `http://127.0.0.1:xxxx`). Otvorite ga u pretraživaču.

## Kratko uputstvo za korišćenje

1. **Izaberite broj komponenti** (npr. 3: regularno + prevara + ekstremna prevara).
2. Podesite **Beta(α, β)** za izlaznost i udeo pobednika u svakoj komponenti.
3. Odredite **pondere mešavine** i **broj biračkih mesta**.
4. Kliknite **Pokreni simulaciju**.
5. Istražite **grafike** (originalni i logit prostor) i **tabele**.
6. Kliknite **Oceni MKM** da ocenite mešavinu na svoje simulirane podatke.
7. **Preuzmite**: izveštaj o prevari, MKM ocene i sirove podatke.

## Napomene i pretpostavke

- **Mebane MKM**: Model posmatra ishode po biračkim mestima kao uzorke iz **konačne mešavine** latentnih režima sa različitim **Beta** rasporedima za izlaznost i udeo glasova pobednika. Aplikacija simulira iz ovih rasporeda i omogućava uklapanje mešavine nazad na sintetičke podatke.
- **Identifikabilnost**: Uz ekstremno preklapanje parametara ili mali uzorak, komponente se teže razdvajaju — to je deo forenzičkog učenja, a ne greška.
- **Logit prostor**: Mnoge forenzičke dijagnostike su preglednije posle logit transformacije proporcija; aplikacija uporedo prikazuje oba prostora.

## Navođenje izvora

Ako koristite ovaj alat u istraživanjima, navedite radove **Waltera R. Mebanea, Jr.** o modelima konačne mešavine u forenzici izbora, kao i ovaj repozitorijum (sa commit-om ili oznakom izdanja).

## Povratne informacije i prijava problema

- Putem uživo: https://statisticar-mkm-simulacija.share.connect.posit.cloud
- E-pošta: Zlatko@MyStatisticalConsultant.com

Molimo priložite:

- kratak opis izabranih parametara,
- broj simuliranih biračkih mesta,
- i snimak ekrana ili izvezeni CSV ako prijavljujete grešku.

## Licenca

Ovaj projekat je licenciran pod **MIT licencom** — pogledajte fajl [LICENCA](LICENSE).

<sub>© 2025 Zlatko J. Kovačić</sub>