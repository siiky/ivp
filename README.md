# `ivp`

Search and play videos from [Invidious][invidious], using the [Invidious API][invidious_api].

## Dependencies

Written in and tested with CHICKEN 5. Depends on various eggs and
[`invidious.scm`][invidious.scm] (which is not yet an egg). Currently assumes
`mpv` is installed and in your `$PATH`, and plays the videos by calling `mpv`
with the URLs as arguments. `mpv` depends on [`youtube-dl`][youtube_dl] to
stream.

## Usage

```sh
ivp rick astley
```

```
0	3m33s	dQw4w9WgXcQ	Rick Astley - Never Gonna Give You Up (Video)
1	3m25s	yPYZpwSpKmA	Rick Astley - Together Forever (Official Music Video)
2	3m26s	cc91EfoBh8A	Rick Astley - Angels On My Side (Official Music Video)
3	4m54s	C2xel6q0yao	Rick Astley - Cry for Help (Official Video)
4	3m36s	elp76GYNhbI	Rick Astley - It Would Take a Strong Strong Man
5	3m39s	AC3Ejf7vPEY	Rick Astley - Keep Singing (Official Music Video)
6	17m45s	pT0eqOAqhdg	Rick Astley - Greatest Hits 80's Mix
7	4m6s	Y1FWJJAKz30	Rick Astley's Daughter Taught Him About Rickrolling | The Graham Norton Show | BBC America
8	3m17s	2nKsnQsi0EQ	Rick Astley - She Wants To Dance With Me
9	4m24s	DqZS89jFCFg	Rick Astley - Hold Me In Your Arms (Official Music Video)
10	3m26s	BeyEGebJ1l4	Rick Astley - Whenever You Need Somebody (Official Music Video)
11	4m3s	uDI-PCDL2G4	Rick Astley Vs Mika - Never Gonna Give Up On A Happy Ending
12	4m39s	IdkCEioCp24	Foo Fighters With Rick Astley - Never Gonna Give You Up  - London O2 Arena 19 September 2017
13	16m	nDnph5lp_vE	Rick Astley covers songs from Cars, Foo fighters, AC-DC (House of Blues Boston Feb 18th 2017)
14	5m14s	WUEdsXgBrDA	Rick Astley - Uptown Funk - RTL LATE NIGHT
15	5m55s	cyMHZVT91Dw	Rick Astley Sings Live - Never Gonna Give You Up - This Morning
16	1h2m31s	AgxtWzTe9ek	The Best Of Rick Astley Greatest Hits|Rick Astley Together Forever|Rick Astley Mix 2017-2018
17	3m15s	m3wzpC2o42I	Lights Out - Rick Astley
18	2m58s	EPUVstUH22k	Rick Astley - Beautiful Life (Official Video)
19	7m42s	p1tMZ50s2Oo	Rick Astley - Never Gonna Give You Up (Live @ V Festival 2016 + Interview, HD)
```

Choose what to play: a single video (`0`), a list (`0,3`) or a range (`0-3`)

## Shortcomings

Configuration is currently limited (read: none).

[invidious]: https://invidio.us
[invidious.scm]: https://github.com/siiky/invidious.scm
[invidious_api]: https://github.com/omarroth/invidious/wiki/API
[youtube_dl]: https://github.com/ytdl-org/youtube-dl
