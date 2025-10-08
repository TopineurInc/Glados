#!/bin/bash

# Usage: ./scripts/generate_ast_png.sh <input.lisp> [output.png]

if [ $# -lt 1 ]; then
    echo "Usage: $0 <input.lisp> [output.png]"
    exit 1
fi

INPUT_FILE="$1"
OUTPUT_FILE="${2:-output.png}"
TEMP_MERMAID=$(mktemp).mmd

# Check if mmdc is installed
if ! command -v mmdc &> /dev/null; then
    echo "Error: mermaid-cli is not installed"
    echo "Install it with: npm install -g @mermaid-js/mermaid-cli"
    exit 1
fi

# Generate mermaid from AST
echo "Generating AST diagram for $INPUT_FILE..."
./glados --ast "$INPUT_FILE" | ./scripts/ast_to_mermaid.py > "$TEMP_MERMAID"

if [ $? -ne 0 ]; then
    echo "Error: Failed to generate mermaid diagram"
    rm -f "$TEMP_MERMAID"
    exit 1
fi

# Generate PNG from mermaid
echo "Creating PNG at $OUTPUT_FILE..."
mmdc -i "$TEMP_MERMAID" -o "$OUTPUT_FILE" -b transparent -w 9000

if [ $? -eq 0 ]; then
    echo "✓ Successfully generated $OUTPUT_FILE"
    rm -f "$TEMP_MERMAID"
else
    echo "Error: Failed to generate PNG"
    rm -f "$TEMP_MERMAID"
    exit 1
fi
