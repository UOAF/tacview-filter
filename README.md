A collection of Tacview tools, including:

- **tacview-filter** — given some `example.zip.acmi` or `example.txt.acmi`,
  spits out a `example-filtered.zip.acmi`, shrunk by:
  - Delta-encoding all changes to each object
  - Decimating updates to rates suggested by https://www.tacview.net/documentation/realtime/en/
  - Remapping all object IDs to smaller numbers.

  This can also be used as a traditional Unix-style filter,
  reading from `stdin` and writing to `stdout`.

- **tacview-server** - does all of the above while serving the output in real-time with
  Tacview or other compatible clients, like [OpenRadar](https://github.com/UOAF/openradar).

- **tacview-replay** reads an ACMI file and writes it to `stdout` at the speed dictated
  by its timestamps. Mostly used as a test tool for tacview-server.

- **tacview-stats** gathers statistics about the counts and frequencies of different categories
  of objects in an ACMI file. It's useful to understand what entries take up the most space.

## Obvious Quesitons

- **Can you just make BMS export better Tacview files?**  
  It's a work in progress, but it also seems nice to have a standalone tool since:
  1. We can use more CPU and RAM that we wouldn't want to add to BMS system requirements
     in order to compute more efficient output.
  2. It's easier to ship frequent updates for a small program than for a whole video game.
     (The BMS release cadence is often many months between releases.)

- **Can you ship this with BMS and have it run in the background?**  
  That's a good idea. Soon™.

- **Opening and saving a file in Tacview still makes it smaller than this program. Why?**  
  Turns out the designer of a file format can be very good at optimizing it!
  I'll try to add more of these optimizations Soon™.
