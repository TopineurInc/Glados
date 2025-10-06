while True:
    try:
        s = input("> ").strip()
    except EOFError:
        # Fin du flux, on sort proprement
        print()
        break

    if s.lower() in ("quit", "exit"):
        break
    if not s:
        continue

    try:
        a_str, op, b_str = s.split()
        a = float(a_str)
        b = float(b_str)

        if op == "+":
            res = a + b
        elif op == "-":
            res = a - b
        elif op == "*":
            res = a * b
        elif op == "/":
            if b == 0:
                print("Erreur : division par zéro")
                continue
            res = a / b
        else:
            print("Opérateur inconnu :", op)
            continue

        print(int(res) if res.is_integer() else res)
    except ValueError:
        print("Format attendu : nombre opérateur nombre (ex: 3 + 4)")
