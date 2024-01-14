# tacview-filter

Reads a (decompressed, use `funzip` or the like) ACMI file from stdin,
shrinks it, and writes it (uncompressed, zip it back up) to stdout.

This was originally designed for BMS, but can clean up Tacview files
from any source (DCS, IL-2, ...). While it uses a few tricks, its main
savings come from delta-encode lines and decimating updates to the rates
suggested on https://www.tacview.net/documentation/realtime/en/

## Obvious Quesitons

- **Can you bulid the unzipping and rezipping in so I don't need to script multiple programs?**  
  That's a good idea. Soon™.

- **Can you just make BMS export better Tacview files?**  
  It's a work in progress. Some improvements should ship with 4.37U4, but it seems nice to also have
  a standalone tool since:
  1. We can use more CPU and RAM that we wouldn't want to add to BMS system requirements
     in order to compute more efficient output.
  2. It's easier to ship frequent updates for a small program than for a whole video game.
     (The BMS release cadence is often many months between releases.)

- **Can you ship this with BMS and have it run in the background when I exit 3D?**  
  That's a good idea. Soon™.

- **Opening and saving a file in Tacview still makes it smaller than this program. Why?**  
  Turns out the designer of a file format can be very good at optimizing it!
  I'll try to add more of these optimizations Soon™.
