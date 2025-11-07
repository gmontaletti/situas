#!/bin/bash
# Scan for valid SITUAS report IDs

echo "Scanning SITUAS API for valid report IDs..."
echo ""

for id in {1..150}; do
  result=$(curl -s "https://situas-servizi.istat.it/publish/reportspooljsoncount?pfun=$id&pdata=05/10/2025")

  if echo "$result" | grep -q "NUM_ROWS"; then
    count=$(echo "$result" | grep -o '"NUM_ROWS":[0-9]*' | cut -d: -f2)
    echo "✓ ID $id: $count rows"
  elif echo "$result" | grep -q "401"; then
    : # Skip 401 errors silently
  elif echo "$result" | grep -q "NotFound"; then
    : # Skip NotFound silently
  elif [ ! -z "$result" ]; then
    echo "? ID $id: $(echo "$result" | head -c 50)"
  fi
done
