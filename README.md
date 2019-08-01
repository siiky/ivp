# `ivp`

Search and play videos from [Invidious][invidious], using the [Invidious API][invidious_api].

## Dependencies

Written in and tested with CHICKEN 5. Depends on various eggs,
[`invidious.scm`][invidious.scm] and [`cling`][cling] (which are not yet eggs).
Currently assumes `mpv` is installed and in your `$PATH`, and plays the videos
by calling `mpv` with the URLs as arguments. `mpv` depends on
[`youtube-dl`][youtube_dl] to stream.

## Usage

```sh
ivp rick astley
```

```
0                  3m33s              dQw4w9WgXcQ        Rick Astley - Never Gonna Give You Up (Video)
1                  3m25s              yPYZpwSpKmA        Rick Astley - Together Forever (Official Music Video)
2                  3m26s              cc91EfoBh8A        Rick Astley - Angels On My Side (Official Music Video)
3                  4m54s              C2xel6q0yao        Rick Astley - Cry for Help (Official Video)
4                  3m36s              elp76GYNhbI        Rick Astley - It Would Take a Strong Strong Man
5                  5m14s              WUEdsXgBrDA        Rick Astley - Uptown Funk - RTL LATE NIGHT
6                  3m26s              BeyEGebJ1l4        Rick Astley - Whenever You Need Somebody (Official Music Video)
7                  3m39s              AC3Ejf7vPEY        Rick Astley - Keep Singing (Official Music Video)
8                  17m45s             pT0eqOAqhdg        Rick Astley - Greatest Hits 80's Mix
9                  4m6s               Y1FWJJAKz30        Rick Astley's Daughter Taught Him About Rickrolling | The Graham Norton Show | BBC America
10                 2m58s              EPUVstUH22k        Rick Astley - Beautiful Life (Official Video)
11                 1h2m31s            AgxtWzTe9ek        The Best Of Rick Astley Greatest Hits|Rick Astley Together Forever|Rick Astley Mix 2017-2018
12                 3m17s              2nKsnQsi0EQ        Rick Astley - She Wants To Dance With Me
13                 4m39s              IdkCEioCp24        Foo Fighters With Rick Astley - Never Gonna Give You Up  - London O2 Arena 19 September 2017
14                 3m29s              5o7WEv5Q5ZE        Rick Astley - Dance (Official Video)
15                 4m10s              X6uzlX51kbo        Rick Astley - Don't Say Goodbye (Audio)
16                 3m8s               rAUkXDhrk5k        Rick Astley - Try (Official Video)
17                 7m42s              p1tMZ50s2Oo        Rick Astley - Never Gonna Give You Up (Live @ V Festival 2016 + Interview, HD)
18                 4m24s              DqZS89jFCFg        Rick Astley - Hold Me In Your Arms (Official Music Video)
19                 3m15s              m3wzpC2o42I        Lights Out - Rick Astley
```

Choose what to play: a single video (`0`), a list (`0,3`) or a range (`0-3`)

## Shortcomings

Configuration is currently limited.

[cling]: https://github.com/siiky/cling
[invidious.scm]: https://github.com/siiky/invidious.scm
[invidious]: https://invidio.us
[invidious_api]: https://github.com/omarroth/invidious/wiki/API
[youtube_dl]: https://github.com/ytdl-org/youtube-dl
