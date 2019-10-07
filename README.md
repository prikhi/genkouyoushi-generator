# Genkouyoushi Paper Generator

A command line program to generate various formats of genkoyoushi
paper for practicing your Kana/Kanji.

CLI parameters may be passed to customize the page size, margins, box
count(rows/columns), presence of Furigana boxes, etc. Output formats include
PNG, JPG, & PDF.


## Build / Run

You'll need
[stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
installed. Then you can build & run the program:

```sh 
stack build
stack run test.png
feh test.png
```

Run `stack run -- --help` to see a full list of arguments that can be used to
customize the generated paper.

Run `stack install` to build & copy the `genkouyoushi-gen` executable to
`~/.local/bin`, .


## License

GPL-v3.0+
