# Genkouyoushi Paper Generator

[![Genkouyoushi Generator Build Status](https://travis-ci.org/prikhi/genkouyoushi-generator.svg?branch=master)](https://travis-ci.org/prikhi/genkouyoushi-generator)


A library, command line program, and web server to generate various formats of
genkoyoushi paper for practicing your Kana/Kanji.

The CLI program may be passed arguments to customize the page size, margins,
box count(rows/columns), presence of Furigana boxes, etc. Output formats
include PNG, JPG, & PDF.

The API server accepts configuration parameters via JSON and replies with a
bytestring of the generated PDF file.


## Build / Run

You'll need
[stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
installed. Then you can build & run the CLI program:

```sh 
stack build
stack run genkouyoushi-gen test.png
feh test.png
```

Run `stack run genkouyoushi-gen -- --help` to see a full list of arguments that
can be used to customize the generated paper.

Run `stack install genkouyoushi-generator:exe:genkouyoushi-gen` to build & copy
the `genkouyoushi-gen` executable to `~/.local/bin`. Running `stack install`
without any arguments will copy both the CLI program and API server.


## API Server

A simple web service is provided with the `genkouyoushi-api` executable. This
accepts JSON POST requests for the `Genkouyoushi.Config` type and replies with
a bytestring containing a generated PDF. A webapp to interact with the API
server will also be added to this repository.


## License

GPL-v3.0+
