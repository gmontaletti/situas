#!/bin/bash
# Comprehensive scan for all valid SITUAS report IDs using TODAY'S date

TODAY="05/10/2025"

echo "Scanning SITUAS API for valid report IDs with date: $TODAY"
echo "============================================"
echo ""

for id in {1..200}; do
  # Test with single date parameter (DATA type)
  result=$(curl -s "https://situas-servizi.istat.it/publish/reportspooljsoncount?pfun=$id&pdata=$TODAY" 2>&1)

  if echo "$result" | grep -q "NUM_ROWS"; then
    count=$(echo "$result" | grep -o '"NUM_ROWS":[0-9]*' | cut -d: -f2)
    echo "✓ ID $id: $count rows (DATA type)"
  elif echo "$result" | grep -q "401"; then
    # Skip 401 silently (unauthorized)
    :
  elif echo "$result" | grep -q "NotFound"; then
    # Skip NotFound silently
    :
  elif echo "$result" | grep -q "UserDefinedResourceError"; then
    # This might be PERIODO type - try with date range
    result2=$(curl -s "https://situas-servizi.istat.it/publish/reportspooljsoncount?pfun=$id&pdatada=01/01/2024&pdataa=$TODAY" 2>&1)
    if echo "$result2" | grep -q "NUM_ROWS"; then
      count=$(echo "$result2" | grep -o '"NUM_ROWS":[0-9]*' | cut -d: -f2)
      echo "✓ ID $id: $count rows (PERIODO type)"
    fi
  fi
done

echo ""
echo "Scan complete!"
