# gbomb
Giantbomb command line client, written in OCaml

## API Keys
To use this tool, you must provide your own API key. It can be stored in two places:
- The environment variable `GIANTBOMB_API_KEY`
- The file `$HOME/.giantbomb/api_key`

You can get an API key [here](https://www.giantbomb.com/api/).

## Usage
```shell
# Prints the URL where you can download a video
# Intended to actually download the video at some point
$ gbomb download --quality [low|high|hd] VIDEO_ID
```

## TODO
- [ ] Acutually download the video when asked
- [ ] Get a list of last `n` videos, filtering by show
