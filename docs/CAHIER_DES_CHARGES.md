# Cahier des charges – Langage Lisp GLaDOS

## Objectifs

- [ ] Garantir une expérience cohérente pour les exemples `examples/*.lisp`
- [ ] Servir de référence fonctionnelle pour les évolutions du compilateur/VM
- [x] Décrire précisément le sous-ensemble de Lisp réellement supporté
- [x] Fournir une base pour enrichir les suites de tests automatisées

## Périmètre fonctionnel

- [x] Parsing de S-expressions (listes parenthésées, entiers, booléens, chaînes)
- [x] Programme principal composé de définitions (`define`) et d'expressions
- [x] Évaluation stricte avec environnement lexical (fermetures supportées)
- [x] Gestion des macros internes (`when`, `unless`, `cond`)
- [ ] Support des expressions quasi-quotées (`'`, `` ` ``, `,`) – **Hors périmètre actuel**
- [ ] Support des paires `cons` / listes natives – **Hors périmètre actuel**

## Grammaire supportée

| Catégorie            | Détails                                                                 |
|----------------------|-------------------------------------------------------------------------|
| Littéraux            | Entiers signés, booléens `#t`/`#f`, chaînes ASCII `"..."`               |
| Symboles             | Alphabétiques + caractères `+-*/<>=!?-`                                 |
| Formes spéciales     | `define`, `lambda`, `if`, `let`, `letrec`, `begin`, `quote`             |
| Application          | `(fonction arg1 … argN)` évalue strictement tous les arguments          |
| Macros intégrées     | `when`, `unless`, `cond` (développées avant la compilation)             |

## Types et valeurs

- [x] `Integer` (arithmétique en précision arbitraire via `Integer` Haskell)
- [x] `Boolean`
- [x] `String` (ASCII ; caractères accentués déconseillés pour les tests automatiques)
- [x] `Closure` (captures lexicales et appels via `lambda`)
- [x] `Builtin` (fonctions primitives)
- [ ] Pas de `Float`, `Rational`, `Symbol` distinct ou structures composites (`cons`, vecteurs)

## Primitives disponibles

| Catégorie      | Fonctions                                  | Notes                                                                 |
|----------------|---------------------------------------------|-----------------------------------------------------------------------|
| Arithmétique   | `+`, `-`, `*`, `div`, `mod`                 | Strictement binaires ; division entière ; erreur si division par 0    |
| Comparaison    | `eq?`, `<`, `>`                             | \(eq?\) gère ints, bools, strings ; sinon `#f`                        |
| Booléens       | `not`, `and`, `or`                          | `and`/`or` via macros en court-circuit successifs                     |
| Chaînes        | `string-length`, `string-append`, `substring` | Indices basés sur `Integer`, sans protections avancées               |
| Conversion     | `string->number`, `number->string`          | Limité aux entiers                                                    |
| Entrée/Sortie  | `print`, `display`, `input`, `read-line`    | `print` ajoute un saut de ligne                                      |

## Contrôle de flux

- [x] `if` – expression booléenne stricte, branches obligatoire
- [x] `let` – sucre syntaxique pour un lambda immédiat
- [x] `letrec` – traduit en liens mutuels (utilisé pour mutuelles récursions)
- [x] `begin` – séquence (les résultats intermédiaires sont ignorés)
- [ ] Absence de `set!`, `let*`, `call/cc`, `dynamic-wind`, `multiple-value`

## Macro-expansion

- [x] Expansion récursive tant qu'un identifiant macro connu (`when`, `unless`, `cond`, `and`, `or`)
- [x] Développement en termes de formes de base (`if`, `begin`, `#f`)
- [ ] Pas de macros définies côté utilisateur (`define-syntax`)

## Règles de tests (référence)

- [x] `tests/tester.py` exécute chaque programme via `./glados` avec un timeout de 5 s
- [x] Chaque test compare `stdout` complet à une chaîne attendue (sensibilité aux sauts de ligne)
- [x] Les messages doivent rester en ASCII (tests sensibles aux encodages)
- [x] Le benchmark `Palindrome Research Program` sert de cas d'intégration (strings + TCO)
- [ ] Pas encore de différentiel automatique contre une implémentation Lisp standard

## Checklist qualité

- [ ] Ajouter des tests différentiels (`sbcl`/`guile`) lorsque disponibles
- [x] Étendre le support des booléens pour court-circuiter `and`/`or`
- [ ] Supporter les annotations multi-lignes en commentaires (`;` / `#| |#`)
- [ ] Documenter la gestion des erreurs (messages, propagation)
- [ ] Ajouter des exemples d'utilisation des primitives `input` / `read-line`
