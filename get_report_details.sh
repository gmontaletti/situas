#!/bin/bash
# Get metadata for all valid report IDs

valid_ids="61 63 64 65 66 68 69 70 71 73 74 75 76 91 92 102 128 129 130"

for id in $valid_ids; do
  echo "============================================"
  echo "Report ID: $id"
  echo "============================================"

  # Get metadata
  metadata=$(curl -s "https://situas-servizi.istat.it/publish/anagrafica_report_metadato_web?pfun=$id&pdata=05/10/2025")

  # Extract report name
  name=$(echo "$metadata" | grep -o '"REPORT NAME":"[^"]*"' | cut -d'"' -f4)
  echo "Name: $name"

  # Extract description (first 200 chars)
  desc=$(echo "$metadata" | grep -o '"REPORT DESCRIZIONE":"[^"]*"' | cut -d'"' -f4 | head -c 200)
  echo "Description: $desc..."

  # Get count
  count=$(curl -s "https://situas-servizi.istat.it/publish/reportspooljsoncount?pfun=$id&pdata=05/10/2025" | grep -o '"NUM_ROWS":[0-9]*' | cut -d: -f2)
  echo "Rows: $count"

  echo ""
done
