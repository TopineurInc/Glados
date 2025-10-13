# Topineur — un paradigme « Objet-Fonctionnel Contractuel » (OF-C)

Super idée — voici un paradigme de langage original qui mélange la pureté et l’abstraction de Haskell avec les forces modélisantes de la POO, tout en restant sûr, composable et pratique pour les grandes bases de code. Je l’appelle **Topineur** (paradigme **Objet-Fonctionnel Contractuel**, OF-C). Je te décris : motivation, principes, syntaxe exemple, système de types, sémantique, cas d’usage et forces/faiblesses.

---

# 1 — Motivation (pourquoi ça vaut le coup)

* Haskell → force : pureté, raisonnement mathématique, typeclasses, effets explicites. Mais modéliser des objets mutables, état encapsulé et hiérarchies de comportements peut être verbeux ou non-naturel.
* POO classique → force : encapsulation, identité, méthodes, héritage. Mais le mélange avec mutation conduits à bugs et rend le raisonnement difficile.
* **Objectif Topineur** : offrir des **objets comme valeurs** (immutables par défaut) + **méthodes pures** + **effets explicites** + **mécanismes d'encapsulation et de mutation contrôlée** (via types linéaires/capabilités). On garde le raisonnement équationnel, mais on peut modéliser des systèmes orientés objets naturellement.

---

# 2 — Principes clés du paradigme OF-C

1. **Objets = valeurs immuables** : un objet est une valeur (comme un record) qui contient état (immutable) et méthodes (fonctions pures liées).
2. **Méthodes pures par défaut** : les méthodes qui ne déclarent pas d’effets sont pures (rien de caché). Les effets sont explicités par un système d’effets (similaire à `IO`/row types).
3. **Mutation contrôlée** : mutation possible seulement via *capabilities* linéaires — un objet mutable doit être possédé linéairement (garanties d’absence d’alias).
4. **Polymorphisme multiple** : on a à la fois **typeclasses** (interfaces algébriques, comme Haskell) et **traits/objets** (pour encapsulation/état). Les deux coexistent harmonieusement.
5. **Composition plutôt qu’héritage profond** : héritage se fait par *delegation* et composition, avec des traits et mixins pures qui s’appliquent fonctionnellement.
6. **Effets déclaratifs** : gestion des effets par un système d’“effet rows” (ex : `!{IO, Network}`), traçables et contrôlables.
7. **Évaluation paresseuse configurable** : évaluation strict par défaut (meilleure prévisibilité en POO), mais paresseuse déclarable par expression/annotation ou types (comme `Lazy<T>`).
8. **Interopérabilité** : objets peuvent être convertis en ADT et vice-versa via conversions explicites.

---

# 3 — Syntaxe d’exemple (pseudo-Topineur, inspirée par Haskell/OCaml/Scala)

Définition d’un objet immuable avec méthodes pures :

```topineur
// un "object type" est un record + méthodes
object type Point {
  x: Float
  y: Float

  // méthode pure (pas d'effets)
  def translate(dx: Float, dy: Float): Point =
    Point { x = x + dx, y = y + dy }

  // méthode qui renvoie un string (pure)
  def repr(): String = "(" ++ show(x) ++ ", " ++ show(y) ++ ")"
}

// construction
let p1: Point = Point { x = 0.0, y = 0.0 }
let p2 = p1.translate(1.0, 2.0)
```

Trait (typeclass-like mais avec implémentations réutilisables) :

```topineur
trait Drawable {
  def draw(ctx: Canvas) : !{IO} Unit  // déclare un effet IO
}

// on applique le trait à Point
impl Drawable for Point {
  def draw(ctx: Canvas) : !{IO} Unit =
    ctx.strokeCircle(x, y, 2.0)
}
```

Mutation contrôlée via capability linéaire (`mut` + type linéaire `!lin`):

```topineur
// compteur mutable: la référence mutCounter est linéaire
object type MutableCounter {
  value: Int
  def inc(): !{State} MutableCounter  // renvoie une nouvelle référence possédée
}

// création d'un compteur mutable (possédé linéairement)
let mutCounter: !lin MutableCounter = MutableCounter { value = 0 }

// mutation "sécurisée" : on consomme la référence et on récupère la nouvelle
let mutCounter = mutCounter.inc()   // ok (linéaire)
```

Composition d’objets / délégation :

```topineur
object type Logger {
  level: LogLevel
  def log(msg: String): !{IO} Unit = ...
}

// composons un service qui délègue au logger
object type UserService {
  logger: Logger
  def createUser(name: String): !{IO} User =
    logger.log("creating " ++ name);
    User { name = name }
}
```

Pattern matching sur objets convertis en ADT (interop explicite) :

```topineur
match user.toADT() {
  case Guest -> ...
  case Registered(id, data) -> ...
}
```

---

# 4 — Système de types (résumé, justifié)

* **Types algébriques (ADT)** + **Object Types** (records + méthodes).
* **Typeclasses** (polymorphisme ad hoc) et **traits** (réutilisables, possiblement avec état immuable).
* **Effet rows** : fonctions / méthodes déclarent les effets qu’elles peuvent faire, p.ex. `!{IO, Net}`.
* **Linearité / Ownership** : types linéaires (`!lin T`) pour mutation sûre; empêche aliasing dangereux.
* **Subtyping contrôlé** : les object types autorisent un sous-typage structural (un objet avec des méthodes supérieures peut être utilisé comme un autre si signatures compatibles).
* **Coercions explicites** entre objets et ADT pour éviter ambiguïtés sémantiques.
* **Inférence locale** : inférence type classique, mais certaines annotations (effects, linéarité) peuvent être nécessaires.

Justification : ce mélange permet de raisonner formellement sur le code (purité, effets) tout en gardant la modélisation naturelle d’objets (encapsulation, méthodes).

---

# 5 — Sémantique d’exécution et RM (runtime)

* Objets immuables sont partagés sans verrou.
* Mutabilité via types linéaires — runtime peut optimiser en mutation in-place lorsque la linéarité garantit l’absence d’alias.
* Gestion mémoire : GC hybride + optimisations en place pour les références linéaires.
* Concurrence : modèle d’acteurs pour la concurrence d’état mutable ; les messages sont des valeurs pures (sérialisables). Les objets purs peuvent être partagés librement entre threads.
* Effets explicites facilitent l’analyse statique (safe effect masking, sandboxing).

---

# 6 — Concurrency / IO / Effets

* **Effets déclarés** facilitent la testabilité et le mock : une fonction `f: A -> B !{Net}` ne peut être utilisée sans acknowledge.
* **Actors + mailboxes** pour les composants avec identité et état mutable. Les acteurs reçoivent des messages pures et retournent des effets encapsulés.
* **Transactions logiques** : support d’update atomique pour objets mutables possédés linéairement via primitives `atomic` (pas de locking global).
* **Asynchronous composition** via `async/await` mais avec types d’effets qui conservent l’origine des effets.

---

# 7 — Exemples complets rapides

1. **Composant UI immuable** — on reçoit un événement, on renvoie un nouvel objet composant :

```topineur
object type Button {
  label: String
  onClick: Event -> !{IO} Button  // renvoie le nouveau bouton
  def render(ctx: Canvas): !{IO} Unit = ...
}

let btn = Button { label = "OK",
                   onClick = fun ev ->
                     // retourne une nouvelle version (immutabilité)
                     Button { label = "Clicked", onClick = <same> } }
```

2. **Actor sûr** (concurrence) :

```topineur
actor BankAccount {
  balance: Int  // encapsulé dans l'acteur, mutable seulement par son thread
  receive {
    case Deposit(n, replyTo) -> balance = balance + n; replyTo.send(OK)
    case Withdraw(n, replyTo) -> if balance >= n then balance -= n; replyTo.send(OK) else replyTo.send(NOK)
  }
}
```

(note : `actor` ici autorise mutation interne mais isolement garanti)

---

# 8 — Outils et ergonomie pour le développeur

* **Analyseur d’effets** : affiche la “trace d’effets” pour les chemins d’exécution.
* **Refactorings sûrs** : renommage, extraits de méthodes garantissant que les effets restent cohérents.
* **Interop** : bindings faciles pour JavaScript/Java/CLR via conversion explicite d’objets/ADT.
* **REPL** avec visualisation d’objets et “diffs” d’objets immuables pour debugger.

---

# 9 — Forces et faiblesses

Forces

* Raisonnement formel (puretés) + modélisation naturelle par objets.
* Mutabilité possible **mais sûre** grâce aux types linéaires et aux acteurs.
* Effets explicites → meilleure testabilité et sécurité.
* Composabilité élevée : traits + fonctions pures = code réutilisable et testable.

Faiblesses / défis

* Complexité du système de types (linéarité + effect rows + subtyping) peut demander annotations supplémentaires au début.
* Courbe d’apprentissage plus raide que POO classique.
* Implémentation runtime + optimisations non triviales (fusion mutation-inplace).
* Certains patterns POO « à la Java » (mutations globales, singletons) doivent être repensés.

---

# 10 — Pourquoi c’est justifié / original

* Harmonise deux mondes souvent opposés : **le raisonnement fonctionnel pur** et **la modélisation objet**. Beaucoup de langages “hybrides” (Scala, F#) font des compromis ; Topineur propose des garanties plus fortes (puretés, effets explicites, ownership linéaire) tout en gardant la modélisation par objets très naturelle.
* Offre une voie pragmatique pour des systèmes réactifs, GUI, systèmes embarqués et serveurs distribués qui veulent la sûreté mathématique *et* l’encapsulation orientée domaine.
* La combinaison « objet = valeur + méthodes pures » + « mutation contrôlée » est ergonomique pour les équipes qui veulent migrer progressivement d’un style impératif vers un style plus sûr.

---

Si tu veux, je peux :

* écrire un **mini-langage** (syntaxe complète + réponses d’exemples) en pseudo-grammaire,
* produire quelques **design patterns Topineur** (Singletons sûrs, Factory immuable, Visitor comme trait),
* ou générer un **exemple d’implémentation (petit interpréteur)** en pseudo-code pour voir la sémantique en action.

Lequel tu préfères ?
