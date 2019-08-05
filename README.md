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
0  V dQw4w9WgXcQ 3m33s   Rick Astley - Never Gonna Give You Up (Video)
1  V yPYZpwSpKmA 3m25s   Rick Astley - Together Forever (Official Music Video)
2  V cc91EfoBh8A 3m26s   Rick Astley - Angels On My Side (Official Music Video)
3  V C2xel6q0yao 4m54s   Rick Astley - Cry for Help (Official Video)
4  V elp76GYNhbI 3m36s   Rick Astley - It Would Take a Strong Strong Man
5  V BeyEGebJ1l4 3m26s   Rick Astley - Whenever You Need Somebody (Official Music Video)
6  V WUEdsXgBrDA 5m14s   Rick Astley - Uptown Funk - RTL LATE NIGHT
7  V EPUVstUH22k 2m58s   Rick Astley - Beautiful Life (Official Video)
8  V BU1PaLmUcKg 6m47s   Rick Astley - My Arms Keep Missing You
9  V pT0eqOAqhdg 17m45s  Rick Astley - Greatest Hits 80's Mix
10 V rAUkXDhrk5k 3m8s    Rick Astley - Try (Official Video)
11 V tTb7xbuZYYs 53m52s  RICK ASTLEY  Live Full Concert 2018
12 V 2nKsnQsi0EQ 3m17s   Rick Astley - She Wants To Dance With Me
13 V AgxtWzTe9ek 1h2m31s The Best Of Rick Astley Greatest Hits|Rick Astley Together Forever|Rick Astley Mix 2017-2018
14 V Lf5jK46Cwe8 4m2s    Rick Astley - Giant (Tour Video)
15 V Y1FWJJAKz30 4m6s    Rick Astley's Daughter Taught Him About Rickrolling | The Graham Norton Show | BBC America
16 V p1tMZ50s2Oo 7m42s   Rick Astley - Never Gonna Give You Up (Live @ V Festival 2016 + Interview, HD)
17 V AC3Ejf7vPEY 3m39s   Rick Astley - Keep Singing (Official Music Video)
18 V dIxQ8X963mQ 3m23s   Rick Astley - She Makes Me (Official Video)
19 V IdkCEioCp24 4m39s   Foo Fighters With Rick Astley - Never Gonna Give You Up  - London O2 Arena 19 September 2017
```

```sh
# Search for all types (videos, playlists and channels)
ivp --type all rick astley
```

```
0  V dQw4w9WgXcQ              3m33s   Rick Astley - Never Gonna Give You Up (Video)
1  V yPYZpwSpKmA              3m25s   Rick Astley - Together Forever (Official Music Video)
2  C UCuAXFkgsw1L7xaCfnd5JJOw         Official Rick Astley
3  V C2xel6q0yao              4m54s   Rick Astley - Cry for Help (Official Video)
4  V cc91EfoBh8A              3m26s   Rick Astley - Angels On My Side (Official Music Video)
5  V elp76GYNhbI              3m36s   Rick Astley - It Would Take a Strong Strong Man
6  V BeyEGebJ1l4              3m26s   Rick Astley - Whenever You Need Somebody (Official Music Video)
7  V djV11Xbc914              3m48s   a-ha - Take On Me (Official Music Video)
8  V a3MWmObR4Ow              3m42s   Dying Inside To Hold You - Timmy Thomas
9  V 4N1iwQxiHrs              4m14s   The Outfield - Your Love (Official Video)
10 V EPUVstUH22k              2m58s   Rick Astley - Beautiful Life (Official Video)
11 V WUEdsXgBrDA              5m14s   Rick Astley - Uptown Funk - RTL LATE NIGHT
12 V rAUkXDhrk5k              3m8s    Rick Astley - Try (Official Video)
13 V Lf5jK46Cwe8              4m2s    Rick Astley - Giant (Tour Video)
14 V pT0eqOAqhdg              17m45s  Rick Astley - Greatest Hits 80's Mix
15 V AgxtWzTe9ek              1h2m31s The Best Of Rick Astley Greatest Hits|Rick Astley Together Forever|Rick Astley Mix 2017-2018
16 V Y1FWJJAKz30              4m6s    Rick Astley's Daughter Taught Him About Rickrolling | The Graham Norton Show | BBC America
17 V AC3Ejf7vPEY              3m39s   Rick Astley - Keep Singing (Official Music Video)
18 P RDdQw4w9WgXcQ                    Mix - Rick Astley - Never Gonna Give You Up (Video)
19 V BU1PaLmUcKg              6m47s   Rick Astley - My Arms Keep Missing You
20 V IdkCEioCp24              4m39s   Foo Fighters With Rick Astley - Never Gonna Give You Up  - London O2 Arena 19 September 2017
21 V p1tMZ50s2Oo              7m42s   Rick Astley - Never Gonna Give You Up (Live @ V Festival 2016 + Interview, HD)
22 V dIxQ8X963mQ              3m23s   Rick Astley - She Makes Me (Official Video)
```

Choose what to play: a single video (`0`), a list (`0,3`) or a range (`0-3`)

## Shortcomings

Configuration is currently limited.

[cling]: https://github.com/siiky/cling
[invidious.scm]: https://github.com/siiky/invidious.scm
[invidious]: https://invidio.us
[invidious_api]: https://github.com/omarroth/invidious/wiki/API
[youtube_dl]: https://github.com/ytdl-org/youtube-dl
