# lofifonts 0.1.0.9003  2024-10-31

* [9000] Optimised the storage and rendering speed
* [9001] Convert bittermelon fonts to 'lofi' font
* [9002] Add `cumsum_cut()` code contributed by June Choe
* [9003] Use raw bytes for bitmap coordinate storage. Converting to integer
  on-the-fly has minimal impact on speed.  The raw bytes compress marginally
  better than integer data, but the biggest win is that it cuts the build
  time in half.

# lofifonts 0.1.0

* Initial release