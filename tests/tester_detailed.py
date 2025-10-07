#!/usr/bin/env python3
"""
GLaDOS Lisp Detailed Tester - Shows bytecode and execution trace
"""

import subprocess
import sys
import os
import tempfile

class Colors:
    GREEN = '\033[92m'
    RED = '\033[91m'
    YELLOW = '\033[93m'
    BLUE = '\033[94m'
    MAGENTA = '\033[95m'
    CYAN = '\033[96m'
    RESET = '\033[0m'
    BOLD = '\033[1m'

def run_glados(code: str, show_disasm: bool = False):
    """Run code with GLaDOS and optionally show disassembly"""
    try:
        with tempfile.NamedTemporaryFile(mode='w', suffix='.lisp', delete=False) as f:
            f.write(code)
            f.flush()
            temp_file = f.name

        # Run normal execution
        result = subprocess.run(
            ['./glados', temp_file],
            capture_output=True,
            text=True,
            timeout=5
        )

        stdout = result.stdout.strip()
        if stdout.startswith("Result: "):
            output = stdout.replace("Result: ", "")
        else:
            output = stdout

        disasm = None
        if show_disasm:
            # Get disassembly
            disasm_result = subprocess.run(
                ['./glados', '--disasm', temp_file],
                capture_output=True,
                text=True,
                timeout=5
            )
            disasm = disasm_result.stdout

        os.unlink(temp_file)

        return output, result.stderr, result.returncode, disasm
    except subprocess.TimeoutExpired:
        return None, "TIMEOUT", -1, None
    except Exception as e:
        return None, str(e), -1, None

def analyze_failing_test(name: str, code: str, expected: str, got: str):
    """Analyze why a test is failing"""
    print(f"\n{Colors.CYAN}{'─' * 70}{Colors.RESET}")
    print(f"{Colors.BOLD}{Colors.RED}FAILING TEST: {name}{Colors.RESET}")
    print(f"{Colors.CYAN}{'─' * 70}{Colors.RESET}")

    print(f"\n{Colors.YELLOW}Code:{Colors.RESET}")
    for line in code.split('\n'):
        print(f"  {line}")

    print(f"\n{Colors.GREEN}Expected:{Colors.RESET} {expected}")
    print(f"{Colors.RED}Got:{Colors.RESET}      {got}")
    print(f"{Colors.MAGENTA}Difference:{Colors.RESET} {int(expected) - int(got) if expected.isdigit() and got.isdigit() else 'N/A'}")

    # Get disassembly
    _, _, _, disasm = run_glados(code, show_disasm=True)
    if disasm:
        print(f"\n{Colors.YELLOW}Bytecode:{Colors.RESET}")
        lines = disasm.split('\n')
        # Show only first 40 lines to avoid clutter
        for line in lines[:40]:
            if 'Instructions:' in line:
                print(f"{Colors.CYAN}{line}{Colors.RESET}")
            elif line.strip().startswith(tuple('0123456789')):
                print(f"  {Colors.BLUE}{line}{Colors.RESET}")
            else:
                print(f"  {line}")

def run_detailed_tests():
    """Run tests with detailed output for failures"""

    failing_tests = []

    tests = [
        ("Factorial 1", """(define (fact n)
  (if (eq? n 0)
      1
      (* n (fact (- n 1)))))
(fact 1)""", "1"),

        ("Factorial 3", """(define (fact n)
  (if (eq? n 0)
      1
      (* n (fact (- n 1)))))
(fact 3)""", "6"),

        ("Fibonacci 2", """(define (fib n)
  (if (eq? n 0)
      0
      (if (eq? n 1)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))
(fib 2)""", "1"),

        ("Fibonacci 4", """(define (fib n)
  (if (eq? n 0)
      0
      (if (eq? n 1)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))
(fib 4)""", "3"),

        ("Sum to N (5)", """(define (sum-to n)
  (if (eq? n 0)
      0
      (+ n (sum-to (- n 1)))))
(sum-to 5)""", "15"),

        ("Power 2^3", """(define (power base exp)
  (if (eq? exp 0)
      1
      (* base (power base (- exp 1)))))
(power 2 3)""", "8"),
    ]

    print(f"\n{Colors.BOLD}{Colors.BLUE}Detailed Analysis of Failing Tests{Colors.RESET}")

    for name, code, expected in tests:
        output, err, returncode, _ = run_glados(code)

        if output != expected:
            failing_tests.append((name, code, expected, output))

    # Analyze each failing test
    for name, code, expected, got in failing_tests:
        analyze_failing_test(name, code, expected, got)

    print(f"\n{Colors.CYAN}{'═' * 70}{Colors.RESET}")
    print(f"{Colors.BOLD}Found {len(failing_tests)} failing tests{Colors.RESET}")
    print(f"{Colors.CYAN}{'═' * 70}{Colors.RESET}\n")

def run_quick_comparison():
    """Quick comparison of simple cases"""
    print(f"\n{Colors.BOLD}Quick Bug Pattern Analysis{Colors.RESET}")
    print(f"{Colors.BLUE}{'=' * 50}{Colors.RESET}\n")

    cases = [
        ("fact(0)", "(define (fact n) (if (eq? n 0) 1 (* n (fact (- n 1))))) (fact 0)", "1"),
        ("fact(1)", "(define (fact n) (if (eq? n 0) 1 (* n (fact (- n 1))))) (fact 1)", "1"),
        ("fact(2)", "(define (fact n) (if (eq? n 0) 1 (* n (fact (- n 1))))) (fact 2)", "2"),
        ("fact(3)", "(define (fact n) (if (eq? n 0) 1 (* n (fact (- n 1))))) (fact 3)", "6"),
        ("fib(0)", "(define (fib n) (if (eq? n 0) 0 (if (eq? n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))) (fib 0)", "0"),
        ("fib(1)", "(define (fib n) (if (eq? n 0) 0 (if (eq? n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))) (fib 1)", "1"),
        ("fib(2)", "(define (fib n) (if (eq? n 0) 0 (if (eq? n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))) (fib 2)", "1"),
        ("fib(3)", "(define (fib n) (if (eq? n 0) 0 (if (eq? n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))) (fib 3)", "2"),
        ("fib(4)", "(define (fib n) (if (eq? n 0) 0 (if (eq? n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))) (fib 4)", "3"),
    ]

    for name, code, expected in cases:
        output, _, _, _ = run_glados(code)
        status = f"{Colors.GREEN}✓{Colors.RESET}" if output == expected else f"{Colors.RED}✗{Colors.RESET}"
        print(f"{status} {name:10} Expected: {expected:3} Got: {output:3}")

    print(f"\n{Colors.BLUE}{'=' * 50}{Colors.RESET}\n")

if __name__ == "__main__":
    if "--analysis" in sys.argv or "-a" in sys.argv:
        run_detailed_tests()
    elif "--quick" in sys.argv or "-q" in sys.argv:
        run_quick_comparison()
    else:
        print("Usage:")
        print("  python3 tester_detailed.py --quick      Quick pattern analysis")
        print("  python3 tester_detailed.py --analysis   Detailed bytecode analysis")
        print("\nRunning quick analysis by default...\n")
        run_quick_comparison()
