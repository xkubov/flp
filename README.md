# FLP projects and excercises

This repository was crated for my study proposes of the FLP course on VUT FIT in Brno
in academic year 2019/2020.

# Známe nevýhody riešenia

Uvážme príklad, ktorý sa nachádza v sade testov: `tests/test04`. Turingov stroj má
následujúcu reprezentáciu:

```
S a F L
aba
```

Ak by sa jednalo o teoretický turingov stroj, daný stroj by ukončil svoju činnosť úspešne.
V mojej implementácií sa však nenachádza `blank` symbol a teda daný stroj vypadne z pásky
aj napriek tomu, že sa dostal do koncového stavu.
