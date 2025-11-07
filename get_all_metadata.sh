#!/bin/bash
# Get complete metadata for all valid reports

TODAY="05/10/2025"
PERIOD_START="01/01/2024"

# DATA type reports
data_reports="61 63 64 65 66 68 69 70 71 73 74 75 76 91 92 102 128 129 130"
# PERIODO type reports
periodo_reports="98 99 100 103 104 105 106 107 108 112 113 114"

echo "DATA TYPE REPORTS (use pdata parameter)"
echo "========================================"
for id in $data_reports; do
  echo ""
  echo "--- Report ID: $id ---"

  # Get count
  count=$(curl -s "https://situas-servizi.istat.it/publish/reportspooljsoncount?pfun=$id&pdata=$TODAY" | grep -o '"NUM_ROWS":[0-9]*' | cut -d: -f2)

  # Get metadata
  metadata=$(curl -s "https://situas-servizi.istat.it/publish/anagrafica_report_metadato_web?pfun=$id&pdata=$TODAY")

  name=$(echo "$metadata" | grep -o '"REPORT NAME":"[^"]*"' | sed 's/"REPORT NAME":"//;s/"$//')
  desc=$(echo "$metadata" | grep -o '"REPORT DESCRIZIONE":"[^"]*"' | sed 's/"REPORT DESCRIZIONE":"//;s/"$//' | head -c 150)

  echo "Name: $name"
  echo "Description: $desc..."
  echo "Rows: $count"
done

echo ""
echo ""
echo "PERIODO TYPE REPORTS (use pdatada + pdataa parameters)"
echo "======================================================="
for id in $periodo_reports; do
  echo ""
  echo "--- Report ID: $id ---"

  # Get count
  count=$(curl -s "https://situas-servizi.istat.it/publish/reportspooljsoncount?pfun=$id&pdatada=$PERIOD_START&pdataa=$TODAY" | grep -o '"NUM_ROWS":[0-9]*' | cut -d: -f2)

  # Get metadata
  metadata=$(curl -s "https://situas-servizi.istat.it/publish/anagrafica_report_metadato_web?pfun=$id&pdatada=$PERIOD_START&pdataa=$TODAY")

  name=$(echo "$metadata" | grep -o '"REPORT NAME":"[^"]*"' | sed 's/"REPORT NAME":"//;s/"$//')
  desc=$(echo "$metadata" | grep -o '"REPORT DESCRIZIONE":"[^"]*"' | sed 's/"REPORT DESCRIZIONE":"//;s/"$//' | head -c 150)

  echo "Name: $name"
  echo "Description: $desc..."
  echo "Rows: $count"
done
