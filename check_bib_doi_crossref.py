#!/usr/bin/env python3
"""
Validate DOIs in a BibTeX file using the Crossref API.

Usage
-----
    python check_bib_doi_crossref.py /path/to/bibliography.bib

The script will:
  - Parse all BibTeX entries.
  - Extract any `doi` fields (case-insensitive).
  - Normalise values like `https://doi.org/...` to bare DOIs.
  - Query the Crossref API for each DOI.
  - Print a per-entry report and a short summary.
"""

from __future__ import annotations

import json
import re
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from dataclasses import dataclass
from typing import Dict, List, Optional


@dataclass
class BibEntryDOI:
    key: str
    doi: str
    line_no: int


DOI_FIELD_RE = re.compile(
    r"""(?ix)          # ignore case, verbose
    \bdoi\s*=\s*       # field name
    [{"]               # opening { or "
    (?P<value>[^}"]+)  # everything up to closing brace/quote
    [}"]               # closing } or "
    ,?                 # optional trailing comma
    """
)

ENTRY_START_RE = re.compile(r"""^@\w+\s*{\s*([^,]+)""")


def normalise_doi(raw: str) -> str:
    """Normalise DOI strings (strip prefixes like https://doi.org/)."""
    value = raw.strip()

    # Strip common URL prefixes
    prefixes = [
        "https://doi.org/",
        "http://doi.org/",
        "https://dx.doi.org/",
        "http://dx.doi.org/",
    ]
    for p in prefixes:
        if value.lower().startswith(p):
            value = value[len(p) :]
            break

    # DOIs are case-insensitive; query using lower-case
    return value.strip()


def parse_bibtex_dois(path: str) -> List[BibEntryDOI]:
    entries: List[BibEntryDOI] = []
    current_key: Optional[str] = None

    with open(path, "r", encoding="utf-8") as f:
        for i, line in enumerate(f, start=1):
            # Detect new entry and remember its key
            m_entry = ENTRY_START_RE.search(line)
            if m_entry:
                current_key = m_entry.group(1).strip()

            # Look for a DOI field on this line
            m_doi = DOI_FIELD_RE.search(line)
            if m_doi and current_key:
                raw_val = m_doi.group("value")
                doi = normalise_doi(raw_val)
                if doi:
                    entries.append(BibEntryDOI(key=current_key, doi=doi, line_no=i))

    return entries


def check_doi_crossref(doi: str, timeout: float = 10.0) -> Dict[str, str]:
    """
    Query Crossref for a DOI.

    Returns a dict with:
      - status: "ok", "not_found", or "error"
      - http_status: HTTP status code as string (if available)
      - message: short description
    """
    encoded_doi = urllib.parse.quote(doi)
    url = f"https://api.crossref.org/works/{encoded_doi}"

    headers = {
        # Crossref recommends including a descriptive User-Agent
        "User-Agent": "doi-validator/0.1 (mailto:someone@example.com)",
    }

    req = urllib.request.Request(url, headers=headers, method="GET")

    try:
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            status_code = resp.getcode()
            if status_code == 200:
                # We could inspect the payload, but 200 is enough here
                try:
                    body = resp.read().decode("utf-8")
                    data = json.loads(body)
                    found_doi = (
                        data.get("message", {})
                        .get("DOI", "")
                        .strip()
                        .lower()
                    )
                    msg = (
                        f"found (Crossref DOI: {found_doi})"
                        if found_doi
                        else "found"
                    )
                except Exception:
                    msg = "found (could not parse JSON, but HTTP 200)"
                return {
                    "status": "ok",
                    "http_status": str(status_code),
                    "message": msg,
                }
            elif status_code == 404:
                return {
                    "status": "not_found",
                    "http_status": str(status_code),
                    "message": "not found in Crossref (may be from another DOI registry)",
                }
            else:
                return {
                    "status": "error",
                    "http_status": str(status_code),
                    "message": f"unexpected HTTP status {status_code}",
                }
    except urllib.error.HTTPError as e:
        if e.code == 404:
            return {
                "status": "not_found",
                "http_status": str(e.code),
                "message": "not found in Crossref (HTTP 404)",
            }
        return {
            "status": "error",
            "http_status": str(e.code),
            "message": f"HTTPError {e.code}: {e.reason}",
        }
    except urllib.error.URLError as e:
        return {
            "status": "error",
            "http_status": "",
            "message": f"URLError: {e.reason}",
        }
    except Exception as e:
        return {
            "status": "error",
            "http_status": "",
            "message": f"Exception: {e}",
        }


def main(argv: List[str]) -> int:
    if len(argv) != 2:
        print(
            "Usage: python check_bib_doi_crossref.py /path/to/bibliography.bib",
            file=sys.stderr,
        )
        return 1

    bib_path = argv[1]

    try:
        entries = parse_bibtex_dois(bib_path)
    except FileNotFoundError:
        print(f"File not found: {bib_path}", file=sys.stderr)
        return 1

    if not entries:
        print("No DOI fields found in the BibTeX file.")
        return 0

    print(f"Found {len(entries)} entries with DOIs in {bib_path}.\n")

    not_found: List[BibEntryDOI] = []
    errors: List[BibEntryDOI] = []

    for idx, entry in enumerate(entries, start=1):
        print(
            f"[{idx}/{len(entries)}] key='{entry.key}', line={entry.line_no}, doi={entry.doi}"
        )
        result = check_doi_crossref(entry.doi)

        status = result["status"]
        message = result["message"]
        http_status = result["http_status"]

        if status == "ok":
            print(f"  -> OK (HTTP {http_status}) - {message}")
        elif status == "not_found":
            print(f"  -> NOT FOUND in Crossref (HTTP {http_status}) - {message}")
            not_found.append(entry)
        else:
            print(f"  -> ERROR (HTTP {http_status}) - {message}")
            errors.append(entry)

        # Be polite to the API
        time.sleep(0.2)

    print("\nSummary")
    print("-------")
    print(f"Total entries with DOIs: {len(entries)}")
    print(f"  OK in Crossref:        {len(entries) - len(not_found) - len(errors)}")
    print(f"  Not found in Crossref: {len(not_found)}")
    print(f"  Errors:                {len(errors)}")

    if not_found:
        print("\nDOIs not found in Crossref:")
        for e in not_found:
            print(f"  - key='{e.key}', line={e.line_no}, doi={e.doi}")

    if errors:
        print("\nEntries with errors while querying Crossref:")
        for e in errors:
            print(f"  - key='{e.key}', line={e.line_no}, doi={e.doi}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))


