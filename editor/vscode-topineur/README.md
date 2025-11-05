# Topineur VSCode Extension

Cette extension fournit :
- coloration syntaxique complète pour les fichiers `.top`
- configuration d'éditeur (commentaires, paires de brackets, pliage)
- reconnaissance des déclarations (`package`, `import`, `def`, `object type`, `let`, etc.)
- mise en valeur des types (`Int`, `Float`, types utilisateurs, génériques `List[T]`, …), des décorateurs (`@cache`), des booléens, des opérateurs et du mot-clé `self`

## Installation rapide

1. Ouvrir VSCode.
2. Appuyer sur `F1` puis exécuter `Developer: Install Extension from Location...`.
3. Sélectionner le dossier `editor/vscode-topineur` de ce dépôt.

En alternative, depuis un terminal :

```bash
code --install-extension editor/vscode-topineur
```

(Adapter le chemin si vous n'êtes pas à la racine du dépôt.)

## Tests manuels

Une fois installée, ouvrez un exemple Topineur, par exemple `examples/all.top`, et vérifiez :
- coloration des commentaires `|-`
- coloration des mots-clés de contrôle (`if`, `while`, `for`, `end`, `top`)
- mise en évidence des noms de fonctions et objets (`def foo`, `object type Person`)
- reconnaissance des types génériques (`List[Int]`, `Tuple[Float, Float]`)
- chaînes, nombres entiers/flottants, opérateurs (`..`, `++`, `==`, `!=`, `->`, etc.)

## Personnalisation

Le fichier `syntaxes/top.tmLanguage.json` contient toutes les règles TextMate si vous souhaitez ajuster ou étendre la coloration.
