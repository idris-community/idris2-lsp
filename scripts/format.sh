#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

while read -r line
do
  l=$(echo -n "$line" | sed "s+PATH_ROOT_PROJECT+$PWD+g")

  # xargs trims the whitespace from wc output
  count=$(echo -n "$l" | wc -c | xargs)

  if [ "$LINE_ENDINGS" = "windows" ]; then
    echo "Content-Length: $count" | awk 'sub("$", "\r")'
    echo "" | awk 'sub("$", "\r")'
    echo -n "$l" # no new line here
  elif [ "$LINE_ENDINGS" = "linux" ]; then
    echo "Content-Length: $count"
    echo ""
    echo -n "$l" # no new line here
  fi

done < "${1:-/dev/stdin}"
