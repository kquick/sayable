# Revision history for sayable

## 1.1.1.0 -- 2023-06-20

* Add Haddock documentation showing examples for operators.
* Support GHC 9.6

## 1.1.0.0 -- 2023-01-17

* Added `&<`, `&<*`, and `&<?` operators to generate a newline between expressing
  their second arguments.
* Added `&!?` for Prettyprinter-processed Maybe sayable.
* Added `&!$*` for Prettyprinter-processed list result sayable.
* Removed unneeded 'SayMessage' type with 'sayMsg' extractor.
* Change `&*` and `&!*` formatting to generate spaces or newlines as appropriate
  to the current output width.
* Miscellaneous documentation fixes and enhancements

## 1.0.2.0 -- 2022-12-23

* Specific GHC support range for GHC 8.8--9.4
* Small cabal file adjustments.

## 1.0.1.0 -- 2022-12-01

* Added Sayable instance for Char.

## 1.0.0.0 -- 2022-06-30

* First independent version.
