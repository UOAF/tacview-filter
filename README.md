# bms-tacview-filter

Reads a (decompressed, use `funzip` or the like) BMS ACMI file from stdin,
filters out crap we don't care for,
and writes it (uncompressed, zip it back up) to stdout.

The game itself should probably not omit the junk in the first place, but

1. It's easier to filter out post-hoc in some cases.

2. This dumb little program can be updated in less than 3-4 weeks.
