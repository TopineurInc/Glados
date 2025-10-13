for file in $(find src -name "*.hs"); do
  hlint "$file" --refactor --refactor-options="-i"
done
