# gbomb
Giant Bomb command line client, written in OCaml

## API Keys
To use this tool, you must provide your own API key. It can be stored in two places:
- The environment variable `GIANTBOMB_API_KEY`
- The file `$HOME/.giantbomb/api_key`

You can get an API key [here](https://www.giantbomb.com/api/).

Note that you must have a Giant Bomb premium subscription to download videos. Otherwise, you might get a *sinister error*.

## Usage
```shell
# Downloads a video at the specified quality
$ gbomb download --quality [low|high|hd] VIDEO_ID
```

## TODO
- [x] Actually download the video when asked
- [ ] Get a list of last `n` videos, filtering by show
