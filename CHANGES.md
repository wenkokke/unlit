### Version 0.3.0.0

- Added an argument to the program and to the `unlit` function which
  determines its behaviour w.r.t. whitespace.

### Version 0.2.0.2

- Now exporting `infer` as an additional style option (passing in `[]`
  for style inference will be deprecated in future releases).

### Version 0.2.0.1

- Fixed bug with occurrences of Bird tags in other code blocks. The
  behaviour of occurrences of other opening/closing braces is
  unchanged: this will still result in an error.

### Version 0.2.0.0

- Rewrote external API.

### Version 0.1.2.1

- Repaired previous version which was broken.

### Version 0.1.2.0

- Changed signatures of `unlit` and `relit` to be functions from Text
  to Text, instead of the clumsy [(Int, Text)] -> [(Int, Text)].


### Version 0.1.1.0

- Changed behaviour of `unlit` such that when it encounters a
  delimiter that it isn't supposed to detect, it doesn't recognise
  them.
  This means that using, e.g. `~~~` in a file using LaTeX-style
  delimiters won't result in an error any longer.

- Added 'haskell' and 'all' styles as valid source styles.

- Separated executable from a small `Unlit` library, which exposes
  the `unlit` and `relit` functions.

- Created a copy of the `Unlit` library, `Unlit.String`, which works
  for regular `String` values. For now, until Backpack becomes a
  thing. (Also renamed `Unlit` to `Unlit.Text`.)
