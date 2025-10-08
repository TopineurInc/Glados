#!/usr/bin/env python3
"""
Convert the textual (or JSON) AST emitted by `./glados --ast` into a Mermaid flowchart.

Usage:
  ./glados --ast examples/input_showcase.lisp | ./scripts/ast_to_mermaid.py

The script reads from STDIN and writes the Mermaid diagram to STDOUT.
"""

from __future__ import annotations

import argparse
import json
import html
import sys
from dataclasses import dataclass, field
from typing import List, Optional, Sequence


# ---------------------------------------------------------------------------
# Tokenizer
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class Token:
    typ: str
    value: Optional[str] = None


SPECIAL_CHARS = {
    "(": "LPAREN",
    ")": "RPAREN",
    "[": "LBRACK",
    "]": "RBRACK",
    ",": "COMMA",
}


def tokenize(text: str) -> List[Token]:
    tokens: List[Token] = []
    i = 0
    length = len(text)
    while i < length:
        ch = text[i]
        if ch.isspace():
            i += 1
            continue
        if ch in SPECIAL_CHARS:
            tokens.append(Token(SPECIAL_CHARS[ch]))
            i += 1
            continue
        if ch == '"':
            i += 1
            start = i
            buffer: List[str] = []
            while i < length:
                c = text[i]
                if c == "\\" and i + 1 < length:
                    buffer.append(text[i + 1])
                    i += 2
                    continue
                if c == '"':
                    break
                buffer.append(c)
                i += 1
            else:
                raise ValueError("Unterminated string literal")
            tokens.append(Token("STRING", "".join(buffer)))
            i += 1  # consume closing quote
            continue
        if ch.isdigit() or (ch == "-" and i + 1 < length and text[i + 1].isdigit()):
            start = i
            i += 1
            while i < length and text[i].isdigit():
                i += 1
            tokens.append(Token("NUMBER", text[start:i]))
            continue
        # identifiers / keywords (bools, constructors)
        start = i
        while (
            i < length
            and not text[i].isspace()
            and text[i] not in SPECIAL_CHARS
            and text[i] != '"'
        ):
            i += 1
        ident = text[start:i]
        if ident in {"True", "False"}:
            tokens.append(Token("BOOL", ident))
        else:
            tokens.append(Token("IDENT", ident))
    return tokens


# ---------------------------------------------------------------------------
# AST representation
# ---------------------------------------------------------------------------


@dataclass
class Node:
    tag: str
    value: Optional[str] = None
    children: List["Node"] = field(default_factory=list)
    edge_labels: List[Optional[str]] = field(default_factory=list)


CLASS_MAP = {
    "Program": "root",
    "EApp": "application",
    "EIf": "control",
    "EDefine": "definition",
    "ELambda": "lambda",
    "EList": "collection",
    "EQuote": "special",
    "EVar": "identifier",
    "Param": "parameter",
    "EString": "literal",
    "EInt": "literal",
    "EBool": "literal",
    "Literal": "literal",
}

CLASS_DEFS = {
    "root": "fill:#f0f4ff,stroke:#2f4f8f,stroke-width:1px,color:#0b1f3a",
    "application": "fill:#e0f7fa,stroke:#006064,stroke-width:1px,color:#00363a",
    "control": "fill:#fdece4,stroke:#c62828,stroke-width:1px,color:#4a0404",
    "definition": "fill:#fff3cd,stroke:#ff8f00,stroke-width:1px,color:#3a1d00",
    "lambda": "fill:#ede7f6,stroke:#4527a0,stroke-width:1px,color:#1b0745",
    "collection": "fill:#e8f5e9,stroke:#2e7d32,stroke-width:1px,color:#0b1c0d",
    "special": "fill:#f3e5f5,stroke:#6a1b9a,stroke-width:1px,color:#230734",
    "identifier": "fill:#fff,stroke:#1565c0,stroke-width:1px,color:#0d2740",
    "parameter": "fill:#fafafa,stroke:#616161,stroke-width:1px,color:#1f1f1f",
    "literal": "fill:#fff8e1,stroke:#ef6c00,stroke-width:1px,color:#3a1c00",
    "other": "fill:#eceff1,stroke:#455a64,stroke-width:1px,color:#263238",
}


# ---------------------------------------------------------------------------
# Recursive descent parser
# ---------------------------------------------------------------------------


class Parser:
    def __init__(self, tokens: Sequence[Token]):
        self.tokens = list(tokens)
        self.pos = 0

    def parse(self) -> Node:
        node = self.parse_expr()
        self.expect_end()
        return node

    # Utilities -----------------------------------------------------------------
    def peek(self) -> Optional[Token]:
        if self.pos < len(self.tokens):
            return self.tokens[self.pos]
        return None

    def consume(self, typ: str | None = None) -> Token:
        token = self.peek()
        if token is None:
            raise ValueError("Unexpected end of input")
        if typ and token.typ != typ:
            raise ValueError(f"Expected {typ}, got {token.typ}")
        self.pos += 1
        return token

    def expect(self, typ: str) -> Token:
        return self.consume(typ)

    def expect_end(self) -> None:
        if self.peek() is not None:
            raise ValueError("Unexpected trailing tokens")

    # Grammar --------------------------------------------------------------------
    def parse_expr(self) -> Node:
        token = self.peek()
        if token is None:
            raise ValueError("Unexpected end while parsing expression")
        if token.typ == "LPAREN":
            self.consume("LPAREN")
            expr = self.parse_expr()
            self.expect("RPAREN")
            return expr
        if token.typ == "IDENT":
            constructor = self.consume("IDENT").value
            assert constructor is not None
            return self.parse_constructor(constructor)
        if token.typ == "STRING":
            value = self.consume("STRING").value
            return Node("Literal", value=value)
        raise ValueError(f"Cannot parse token {token.typ}")

    def parse_constructor(self, name: str) -> Node:
        if name == "EApp":
            func = self.parse_parenthesized_expr()
            args = self.parse_bracket_expr_list()
            labels = ["fn"] + [f"arg{i}" for i in range(1, len(args) + 1)]
            return Node("EApp", children=[func] + args, edge_labels=labels)
        if name == "EDefine":
            identifier = self.expect("STRING").value
            body = self.parse_expr()
            return Node("EDefine", value=identifier, children=[body], edge_labels=["body"])
        if name == "EVar":
            identifier = self.expect("STRING").value
            return Node("EVar", value=identifier)
        if name == "EString":
            literal = self.expect("STRING").value
            return Node("EString", value=literal)
        if name == "EInt":
            literal = self.expect("NUMBER").value
            return Node("EInt", value=literal)
        if name == "EBool":
            literal = self.expect("BOOL").value
            return Node("EBool", value=literal)
        if name == "EList":
            elements = self.parse_bracket_expr_list()
            labels = [f"elem{i}" for i in range(1, len(elements) + 1)]
            return Node("EList", children=elements, edge_labels=labels)
        if name == "ELambda":
            params = self.parse_string_list()
            body = self.parse_parenthesized_expr()
            param_nodes = [Node("Param", value=p) for p in params]
            labels = [f"param{i}" for i in range(1, len(param_nodes) + 1)] + ["body"]
            return Node("ELambda", children=param_nodes + [body], edge_labels=labels)
        if name == "EIf":
            cond = self.parse_expr()
            then = self.parse_expr()
            other = self.parse_expr()
            return Node("EIf", children=[cond, then, other], edge_labels=["cond", "then", "else"])
        if name == "EQuote":
            quoted = self.parse_parenthesized_expr()
            return Node("EQuote", children=[quoted], edge_labels=["expr"])
        # Fallback: treat as opaque constructor
        operands: List[Node] = []
        while True:
            next_token = self.peek()
            if next_token is None:
                break
            if next_token.typ in {"RPAREN", "RBRACK", "COMMA"}:
                break
            operands.append(self.parse_expr())
        labels = [None] * len(operands)
        return Node(name, children=operands, edge_labels=labels)

    def parse_parenthesized_expr(self) -> Node:
        self.expect("LPAREN")
        expr = self.parse_expr()
        self.expect("RPAREN")
        return expr

    def parse_bracket_expr_list(self) -> List[Node]:
        self.expect("LBRACK")
        items = self.parse_expr_list_until("RBRACK")
        self.expect("RBRACK")
        return items

    def parse_expr_list_until(self, closing: str) -> List[Node]:
        items: List[Node] = []
        if self.peek() and self.peek().typ == closing:
            return items
        while True:
            items.append(self.parse_expr())
            token = self.peek()
            if token is None:
                break
            if token.typ == "COMMA":
                self.consume("COMMA")
                continue
            if token.typ == closing:
                break
            raise ValueError(f"Unexpected token {token.typ} in list")
        return items

    def parse_string_list(self) -> List[str]:
        self.expect("LBRACK")
        items: List[str] = []
        token = self.peek()
        if token and token.typ == "RBRACK":
            self.consume("RBRACK")
            return items
        while True:
            items.append(self.expect("STRING").value or "")
            token = self.peek()
            if token is None:
                raise ValueError("Unterminated string list")
            if token.typ == "COMMA":
                self.consume("COMMA")
                continue
            if token.typ == "RBRACK":
                self.consume("RBRACK")
                break
            raise ValueError(f"Unexpected token {token.typ} in string list")
        return items


# ---------------------------------------------------------------------------
# Mermaid renderer
# ---------------------------------------------------------------------------


class MermaidRenderer:
    def __init__(self, direction: str = "TD"):
        self.direction = direction
        self.body_lines: List[str] = []
        self.counter = 0
        self.class_assignments: List[str] = []
        self.used_classes: set[str] = set()
        self.recorded_nodes: set[tuple[str, str]] = set()

    def render(self, roots: Sequence[Node]) -> str:
        self.body_lines = []
        self.class_assignments = []
        self.used_classes = set()
        self.recorded_nodes = set()

        self.body_lines.append("    Program((Programme))")
        self._record_class("Program", self._class_for_tag("Program"))
        for node in roots:
            node_id = self._render_node(node)
            self.body_lines.append(f"    Program --> {node_id}")

        output = [f"flowchart {self.direction}"]
        output.extend(self._class_def_lines())
        output.extend(self.body_lines)
        output.extend(self.class_assignments)
        return "\n".join(output)

    def _render_node(self, node: Node) -> str:
        node_id = f"n{self.counter}"
        self.counter += 1
        label = self._format_label(node)
        self.body_lines.append(f"    {node_id}[\"{label}\"]")
        self._record_class(node_id, self._class_for_tag(node.tag))
        for idx, child in enumerate(node.children):
            child_id = self._render_node(child)
            edge_label = None
            if idx < len(node.edge_labels):
                edge_label = node.edge_labels[idx]
            if edge_label:
                escaped = html.escape(edge_label)
                self.body_lines.append(f"    {node_id} -->|{escaped}| {child_id}")
            else:
                self.body_lines.append(f"    {node_id} --> {child_id}")
        return node_id

    def _record_class(self, node_id: str, class_name: Optional[str]) -> None:
        if not class_name:
            return
        self.used_classes.add(class_name)
        key = (node_id, class_name)
        if key in self.recorded_nodes:
            return
        self.recorded_nodes.add(key)
        self.class_assignments.append(f"    class {node_id} {class_name}")

    def _class_def_lines(self) -> List[str]:
        lines: List[str] = []
        for class_name in sorted(self.used_classes):
            rule = CLASS_DEFS.get(class_name, CLASS_DEFS["other"])
            lines.append(f"    classDef {class_name} {rule};")
        return lines

    def _class_for_tag(self, tag: str) -> str:
        return CLASS_MAP.get(tag, "other")

    def _format_label(self, node: Node) -> str:
        base = node.tag
        if node.tag == "Literal":
            base = "Literal"
        if node.value is not None:
            display = shorten(node.value)
            display = html.escape(display)
            if node.tag == "EString":
                display = f'&quot;{display}&quot;'
            elif node.tag == "Literal":
                pass
            return f"{base}<br/>{display}"
        if node.tag == "EString":
            return f"{base}<br/>&quot;&quot;"
        return base


def shorten(value: str, limit: int = 40) -> str:
    if len(value) <= limit:
        return value
    return f"{value[:limit-3]}..."


# ---------------------------------------------------------------------------
# CLI handling
# ---------------------------------------------------------------------------


def extract_ast_lines(stdin_data: str) -> List[str]:
    if "=== AST ===" in stdin_data:
        _, tail = stdin_data.split("=== AST ===", 1)
    else:
        tail = stdin_data
    lines = []
    for line in tail.splitlines():
        stripped = line.strip()
        if not stripped:
            continue
        if stripped.startswith("==="):
            continue
        lines.append(stripped)
    return lines


def parse_lines_to_nodes(lines: Sequence[str]) -> List[Node]:
    nodes: List[Node] = []
    for line in lines:
        tokens = tokenize(line)
        parser = Parser(tokens)
        try:
            node = parser.parse()
        except Exception as exc:  # pragma: no cover - simple CLI script
            raise ValueError(f"Unable to parse line: {line}") from exc
        nodes.append(node)
    return nodes


def parse_json_payload(payload: object) -> List[Node]:
    if isinstance(payload, list):
        return [node_from_json(item) for item in payload]
    return [node_from_json(payload)]


def node_from_json(item: object) -> Node:
    if isinstance(item, Node):
        return item
    if isinstance(item, dict):
        tag = str(item.get("tag") or item.get("type") or "Unknown")
        value = item.get("value")
        if value is not None:
            value = str(value)
        children: List[Node] = []
        if "func" in item:
            children.append(node_from_json(item["func"]))
        if "args" in item:
            children.extend(node_from_json(arg) for arg in item["args"])
        if "children" in item:
            children.extend(node_from_json(child) for child in item["children"])
        return Node(tag, value=value, children=children)
    if isinstance(item, (str, int, float, bool)):
        return Node("Literal", value=str(item))
    return Node("Unknown", value=str(item))


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Generate a Mermaid flowchart from the textual AST output."
    )
    parser.add_argument(
        "--direction",
        choices=["TD", "LR", "BT", "RL"],
        default="TD",
        help="Flowchart direction (default: TD).",
    )
    return parser


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)

    data = sys.stdin.read()
    if not data.strip():
        print("No input received on stdin.", file=sys.stderr)
        return 1

    try:
        payload = json.loads(data)
    except json.JSONDecodeError:
        lines = extract_ast_lines(data)
        if not lines:
            print("Could not find AST lines in the provided input.", file=sys.stderr)
            return 1
        nodes = parse_lines_to_nodes(lines)
    else:
        nodes = parse_json_payload(payload)

    renderer = MermaidRenderer(direction=args.direction)
    diagram = renderer.render(nodes)
    print(diagram)
    return 0


if __name__ == "__main__":  # pragma: no cover - CLI entry point
    raise SystemExit(main())
