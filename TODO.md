### TODO

- Change behaviour of `unlit` such that when it encounters delimiters
  that it isn't supposed to detect, it doesn't even recognise them.
  This can easily be done by passing the current source style to the
  `isDelim` function, and only having it perform the checks that are
  necessary for those particular delimiters.
