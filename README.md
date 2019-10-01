# Genkouyoushi Paper Generator

A command line program to generate various formats of genkoyoushi
paper for practicing your Kana/Kanji.

Currently fixed to generating a 7x18 grid of boxes with furigana boxes in a
column format for 8.5"x11" paper. Command line arguments will come soon,
allowing different layouts and paper sizes.


## Build / Run

You'll need
[stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
installed. Then you can build & run the program:

```sh 
stack build
stack run
gimp test.png
```

To install the executable to `~/.local/bin`, run `stack install`.


## License

GPL-v3.0+
