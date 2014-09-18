### Version 0.1.1.0

- Changed behaviour of `unlit` such that when it encounters a
  delimiter that it isn't supposed to detect, it doesn't recognise
  them.
  This means that using, e.g. `~~~` in a file using LaTeX-style
  delimiters won't result in an error any longer.
