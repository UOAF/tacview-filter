# tacview-filter

Reads a (decompressed, use `funzip` or the like) ACMI file from stdin,
shrinks it, and writes it (uncompressed, zip it back up) to stdout.

This was originally designed for BMS, but can clean up Tacview files
from any source (DCS, IL-2, ...). While it uses a few tricks, its main
savings come from delta-encode lines and decimating updates to the rates
suggested on https://www.tacview.net/documentation/realtime/en/
