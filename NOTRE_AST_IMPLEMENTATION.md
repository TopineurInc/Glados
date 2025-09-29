# Structures de src/AST.hs

Le fichier `src/AST.hs` définit tous les types de données utilisés dans le compilateur GLaDOS.

## SourcePos et Loc

```haskell
data SourcePos = SourcePos { spLine :: Int, spCol :: Int }
type Loc = Maybe SourcePos
```

`SourcePos` stocke une position dans le fichier source (ligne et colonne).
`Loc` est une position optionnelle. Chaque noeud peut porter sa position pour les messages d'erreur.

## SExpr et Atom

```haskell
data SExpr = SAtom Atom Loc | SList [SExpr] Loc
data Atom = AInteger Integer | ABool Bool | ASymbol String | AString String
```

`SExpr` représente une S-expression après parsing :
- `SAtom` contient une valeur primitive et sa position
- `SList` contient une liste de S-expressions et sa position

`Atom` représente les valeurs primitives :
- `AInteger` pour les nombres entiers
- `ABool` pour les booléens
- `ASymbol` pour les identificateurs et mots-clés
- `AString` pour les chaînes de caractères

## Expr

```haskell
type Name = String

data Expr
  = EInt Integer
  | EBool Bool
  | EString String
  | EVar Name
  | EList [Expr]
  | ELambda [Name] Expr
  | EDefine Name Expr
  | EIf Expr Expr Expr
  | EApp Expr [Expr]
  | EQuote SExpr
```

`Expr` représente l'arbre syntaxique abstrait après désucrage :
- `EInt`, `EBool`, `EString` : valeurs littérales
- `EVar` : référence à une variable
- `EList` : liste littérale
- `ELambda` : définition de fonction (paramètres, corps)
- `EDefine` : définition de variable globale
- `EIf` : condition (test, branche alors, branche sinon)
- `EApp` : appel de fonction (fonction, arguments)
- `EQuote` : expression quotée (non-évaluée)

## ANF

```haskell
data ANF
  = AVar Name
  | AConst Constant
  | ALet Name ANF ANF
  | AIf ANF ANF ANF
  | ACall Name [Name]
  | ALambdaFlat [Name] [Instr]
```

`ANF` (A-Normal Form) représente une forme intermédiaire simplifiée :
- `AVar` : variable
- `AConst` : constante
- `ALet` : liaison let (nom, valeur, corps)
- `AIf` : condition
- `ACall` : appel de fonction avec nom et arguments (tous des noms de variables)
- `ALambdaFlat` : fonction aplatie avec instructions directes

## Constant

```haskell
data Constant
  = CInt Integer
  | CBool Bool
  | CString String
  | CFuncRef Name
```

`Constant` représente une constante stockée dans le pool de constantes :
- `CInt`, `CBool`, `CString` : valeurs primitives
- `CFuncRef` : référence à une fonction par son nom

## Instr

```haskell
type Label = String

data Instr
  = IConst Int
  | ILoad Int
  | IStore Int
  | IPrim String
  | ICall Int Name
  | ITailCall Int Name
  | IReturn
  | IJump Int
  | IJumpIfFalse Int
  | INop
  | IMakeClosure Name [Int]
  | ILoadClosure Int
  | IStoreClosure Int
```

`Instr` représente une instruction de la machine virtuelle :
- `IConst` : charge une constante depuis le pool
- `ILoad` : charge une variable locale
- `IStore` : sauvegarde dans une variable locale
- `IPrim` : appel d'une primitive (+, -, *, /, eq?)
- `ICall` : appel de fonction normal
- `ITailCall` : appel terminal (optimisation)
- `IReturn` : retourne une valeur
- `IJump` : saut inconditionnel
- `IJumpIfFalse` : saut conditionnel
- `INop` : instruction vide
- `IMakeClosure` : crée une closure
- `ILoadClosure`, `IStoreClosure` : accès à l'environnement d'une closure

## CodeObject

```haskell
data CodeObject = CodeObject
  { coName :: Name
  , coArity :: Int
  , coMaxLocals :: Int
  , coConsts :: Vector.Vector Constant
  , coInstrs :: Vector.Vector Instr
  , coLabelMap :: Map.Map Label Int
  }
```

`CodeObject` représente une fonction compilée :
- `coName` : nom de la fonction
- `coArity` : nombre d'arguments attendus
- `coMaxLocals` : nombre de variables locales nécessaires
- `coConsts` : pool de constantes utilisées
- `coInstrs` : séquence d'instructions
- `coLabelMap` : table des labels résolus vers positions

## Value

```haskell
data Value
  = VInt Integer
  | VBool Bool
  | VString String
  | VClosure Name [Value]
  | VBuiltin Name ([Value] -> IO Value)
```

`Value` représente une valeur à l'exécution :
- `VInt`, `VBool`, `VString` : valeurs primitives
- `VClosure` : closure (fonction + environnement capturé)
- `VBuiltin` : fonction native implémentée en Haskell

## Frame

```haskell
data Frame = Frame
  { fLocals :: Vector.Vector (Maybe Value)
  , fStack :: [Value]
  , fCode :: CodeObject
  , fPC :: Int
  }
```

`Frame` représente l'état d'exécution d'une fonction :
- `fLocals` : variables locales (indexées)
- `fStack` : pile d'opérandes pour les calculs
- `fCode` : code de la fonction en cours
- `fPC` : compteur de programme (instruction courante)

## VMState

```haskell
data VMState = VMState
  { vFrames :: [Frame]
  , vGlobals :: Map.Map Name Value
  , vCodeObjects :: Map.Map Name CodeObject
  , vBuiltins :: Map.Map Name Value
  }
```

`VMState` représente l'état complet de la machine virtuelle :
- `vFrames` : pile des frames d'exécution
- `vGlobals` : variables globales définies
- `vCodeObjects` : toutes les fonctions compilées
- `vBuiltins` : fonctions natives disponibles (+, -, *, /, eq?, etc.)

## CompileError

```haskell
data CompileError
  = ParseError String Loc
  | SyntaxError String Loc
```

`CompileError` représente les erreurs de compilation :
- `ParseError` : erreur lors du parsing
- `SyntaxError` : erreur de syntaxe lors des transformations
