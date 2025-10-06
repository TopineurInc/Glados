#!/usr/bin/env python3
"""
GLaDOS Lisp Tester - Compare results with official Lisp implementations
"""

import subprocess
import sys
import os
from typing import Tuple, Optional
import tempfile

class Colors:
    GREEN = '\033[92m'
    RED = '\033[91m'
    YELLOW = '\033[93m'
    BLUE = '\033[94m'
    RESET = '\033[0m'
    BOLD = '\033[1m'

def run_glados(code: str) -> Tuple[Optional[str], Optional[str], int]:
    """Run code with GLaDOS compiler"""
    try:
        with tempfile.NamedTemporaryFile(mode='w', suffix='.lisp', delete=False) as f:
            f.write(code)
            f.flush()
            temp_file = f.name

        result = subprocess.run(
            ['./glados', temp_file],
            capture_output=True,
            text=True,
            timeout=5
        )

        os.unlink(temp_file)

        # Extract result from "Result: X" format
        stdout = result.stdout.strip()
        if stdout.startswith("Result: "):
            output = stdout.replace("Result: ", "")
        else:
            output = stdout

        return output, result.stderr, result.returncode
    except subprocess.TimeoutExpired:
        return None, "TIMEOUT", -1
    except Exception as e:
        return None, str(e), -1

def run_reference_lisp(code: str) -> Tuple[Optional[str], Optional[str], int]:
    """Run code with reference Lisp implementation (tries multiple)"""
    # Try different Lisp implementations
    interpreters = [
        (['sbcl', '--script'], 'sbcl'),
        (['clisp', '-q', '-x'], 'clisp'),
        (['guile', '-c'], 'guile'),
        (['racket', '-e'], 'racket'),
    ]

    for cmd, name in interpreters:
        try:
            # Adapt code for the interpreter
            adapted_code = adapt_code_for_interpreter(code, name)

            if name in ['guile', 'racket']:
                result = subprocess.run(
                    cmd + [adapted_code],
                    capture_output=True,
                    text=True,
                    timeout=5
                )
            else:
                with tempfile.NamedTemporaryFile(mode='w', suffix='.lisp', delete=False) as f:
                    f.write(adapted_code)
                    f.flush()
                    temp_file = f.name

                result = subprocess.run(
                    cmd + [temp_file],
                    capture_output=True,
                    text=True,
                    timeout=5
                )

                os.unlink(temp_file)

            if result.returncode == 0:
                output = result.stdout.strip()
                return output, result.stderr, result.returncode
        except (subprocess.TimeoutExpired, FileNotFoundError, Exception):
            continue

    return None, "NO_LISP_FOUND", -1

def adapt_code_for_interpreter(code: str, interpreter: str) -> str:
    """Adapt GLaDOS Lisp code for standard Lisp interpreters"""

    if interpreter == 'guile':
        # Guile is Scheme
        lines = code.split('\n')
        result_lines = []
        for line in lines:
            if 'define' in line:
                result_lines.append(line)
            else:
                # Wrap final expression in display
                if line.strip() and not line.strip().startswith('(define'):
                    result_lines.append(f"(display {line.strip()})")
        return '\n'.join(result_lines)

    elif interpreter == 'racket':
        # Racket needs #lang
        return f"#lang racket\n{code}\n"

    elif interpreter in ['sbcl', 'clisp']:
        # Common Lisp - wrap final expression in print
        lines = code.split('\n')
        result_lines = []
        for line in lines:
            if line.strip() and not line.strip().startswith('(define'):
                if 'define' not in line:
                    result_lines.append(f"(print {line.strip()})")
                else:
                    result_lines.append(line)
            else:
                result_lines.append(line)
        return '\n'.join(result_lines)

    return code

class TestCase:
    def __init__(self, name: str, code: str, expected: Optional[str] = None):
        self.name = name
        self.code = code
        self.expected = expected

def create_test_suite() -> list:
    """Create comprehensive test suite"""
    return [
        TestCase("Simple Addition", "(+ 2 3)", "5"),
        TestCase("Simple Subtraction", "(- 10 4)", "6"),
        TestCase("Simple Multiplication", "(* 3 4)", "12"),
        TestCase("Division", "(div 20 5)", "4"),
        TestCase("Modulo", "(mod 17 5)", "2"),

        TestCase("Nested Addition", "(+ 1 (+ 2 3))", "6"),
        TestCase("Nested Multiplication", "(* 2 (* 3 4))", "24"),
        TestCase("Mixed Operations", "(+ (* 2 3) (* 4 5))", "26"),

        TestCase("Simple If True", "(if (eq? 1 1) 10 20)", "10"),
        TestCase("Simple If False", "(if (eq? 1 2) 10 20)", "20"),
        TestCase("Less Than True", "(if (< 5 10) 1 0)", "1"),
        TestCase("Less Than False", "(if (< 10 5) 1 0)", "0"),
        TestCase("Greater Than", "(if (> 10 5) 1 0)", "1"),

        TestCase("Factorial 0", """(define (fact n)
  (if (eq? n 0)
      1
      (* n (fact (- n 1)))))
(fact 0)""", "1"),

        TestCase("Factorial 1", """(define (fact n)
  (if (eq? n 0)
      1
      (* n (fact (- n 1)))))
(fact 1)""", "1"),

        TestCase("Factorial 3", """(define (fact n)
  (if (eq? n 0)
      1
      (* n (fact (- n 1)))))
(fact 3)""", "6"),

        TestCase("Factorial 4", """(define (fact n)
  (if (eq? n 0)
      1
      (* n (fact (- n 1)))))
(fact 4)""", "24"),

        TestCase("Factorial 5", """(define (fact n)
  (if (eq? n 0)
      1
      (* n (fact (- n 1)))))
(fact 5)""", "120"),

        TestCase("Power 2^3", """(define (power base exp)
  (if (eq? exp 0)
      1
      (* base (power base (- exp 1)))))
(power 2 3)""", "8"),

        TestCase("Power 3^4", """(define (power base exp)
  (if (eq? exp 0)
      1
      (* base (power base (- exp 1)))))
(power 3 4)""", "81"),

        TestCase("GCD 48 18", """(define (gcd a b)
  (if (eq? b 0)
      a
      (gcd b (mod a b))))
(gcd 48 18)""", "6"),

        TestCase("GCD 100 45", """(define (gcd a b)
  (if (eq? b 0)
      a
      (gcd b (mod a b))))
(gcd 100 45)""", "5"),

        TestCase("Sum to N (5)", """(define (sum-to n)
  (if (eq? n 0)
      0
      (+ n (sum-to (- n 1)))))
(sum-to 5)""", "15"),

        TestCase("Sum to N (10)", """(define (sum-to n)
  (if (eq? n 0)
      0
      (+ n (sum-to (- n 1)))))
(sum-to 10)""", "55"),

        TestCase("Is Even (0)", """(define (is-even n)
  (if (eq? n 0)
      1
      (is-odd (- n 1))))
(define (is-odd n)
  (if (eq? n 0)
      0
      (is-even (- n 1))))
(is-even 0)""", "1"),

        TestCase("Is Even (10)", """(define (is-even n)
  (if (eq? n 0)
      1
      (is-odd (- n 1))))
(define (is-odd n)
  (if (eq? n 0)
      0
      (is-even (- n 1))))
(is-even 10)""", "1"),

        TestCase("Is Odd (7)", """(define (is-even n)
  (if (eq? n 0)
      1
      (is-odd (- n 1))))
(define (is-odd n)
  (if (eq? n 0)
      0
      (is-even (- n 1))))
(is-odd 7)""", "1"),

        TestCase("Fibonacci 0", """(define (fib n)
  (if (eq? n 0)
      0
      (if (eq? n 1)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))
(fib 0)""", "0"),

        TestCase("Fibonacci 1", """(define (fib n)
  (if (eq? n 0)
      0
      (if (eq? n 1)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))
(fib 1)""", "1"),

        TestCase("Fibonacci 2", """(define (fib n)
  (if (eq? n 0)
      0
      (if (eq? n 1)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))
(fib 2)""", "1"),

        TestCase("Fibonacci 3", """(define (fib n)
  (if (eq? n 0)
      0
      (if (eq? n 1)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))
(fib 3)""", "2"),

        TestCase("Fibonacci 4", """(define (fib n)
  (if (eq? n 0)
      0
      (if (eq? n 1)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))
(fib 4)""", "3"),

        TestCase("Fibonacci 5", """(define (fib n)
  (if (eq? n 0)
      0
      (if (eq? n 1)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))
(fib 5)""", "5"),

        TestCase("Fibonacci 6", """(define (fib n)
  (if (eq? n 0)
      0
      (if (eq? n 1)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))
(fib 6)""", "8"),

        TestCase("Simple Function Call", """(define (double x) (* x 2))
(double 5)""", "10"),

        TestCase("Nested Function Call", """(define (double x) (* x 2))
(define (quadruple x) (double (double x)))
(quadruple 3)""", "12"),
    ]

def run_tests(verbose: bool = False):
    """Run all tests and compare results"""
    tests = create_test_suite()

    passed = 0
    failed = 0
    errors = 0

    print(f"\n{Colors.BOLD}GLaDOS Lisp Tester{Colors.RESET}")
    print(f"{Colors.BLUE}={'=' * 60}{Colors.RESET}\n")

    for i, test in enumerate(tests, 1):
        print(f"[{i}/{len(tests)}] {test.name}...", end=" ")

        glados_output, glados_err, glados_code = run_glados(test.code)

        if glados_err and "TIMEOUT" in glados_err:
            print(f"{Colors.YELLOW}TIMEOUT{Colors.RESET}")
            errors += 1
            continue

        if glados_code != 0 and glados_err:
            print(f"{Colors.RED}ERROR{Colors.RESET}")
            if verbose:
                print(f"  GLaDOS error: {glados_err}")
            errors += 1
            continue

        # Check against expected value if provided
        if test.expected:
            if glados_output == test.expected:
                print(f"{Colors.GREEN}✓ PASS{Colors.RESET}")
                passed += 1
            else:
                print(f"{Colors.RED}✗ FAIL{Colors.RESET}")
                print(f"  Expected: {test.expected}")
                print(f"  Got:      {glados_output}")
                failed += 1
        else:
            # Just show the result
            print(f"{Colors.BLUE}Result: {glados_output}{Colors.RESET}")
            passed += 1

    # Summary
    print(f"\n{Colors.BLUE}{'=' * 60}{Colors.RESET}")
    print(f"{Colors.BOLD}Summary:{Colors.RESET}")
    print(f"  {Colors.GREEN}Passed:{Colors.RESET}  {passed}/{len(tests)}")
    print(f"  {Colors.RED}Failed:{Colors.RESET}  {failed}/{len(tests)}")
    print(f"  {Colors.YELLOW}Errors:{Colors.RESET}  {errors}/{len(tests)}")

    success_rate = (passed / len(tests)) * 100
    print(f"\n  Success Rate: {success_rate:.1f}%")

    if failed == 0 and errors == 0:
        print(f"\n{Colors.GREEN}{Colors.BOLD}🎉 All tests passed!{Colors.RESET}")
    elif success_rate >= 80:
        print(f"\n{Colors.YELLOW}Good progress! Keep fixing bugs.{Colors.RESET}")
    else:
        print(f"\n{Colors.RED}Many tests failing. Debug needed.{Colors.RESET}")

if __name__ == "__main__":
    verbose = "-v" in sys.argv or "--verbose" in sys.argv
    run_tests(verbose)
