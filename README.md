weather
=====

The erlang backend of my weather station project.
The project was based on:
https://blog.kdubovikov.ml/articles/hardware/build-yourself-a-weather-station

However, i did not use Rust, i chose to do it in erlang.
Also, my device includes an 128x32 oled display,
and in my firmware i chose a slightly different approach.

The erlang backend contains the following parts:
- a database server process (write/read measurements from DETS table)
- a telegram bot interface (subscription + periodic messages) -> pe4kin https://github.com/seriyps/pe4kin
- an MQTT server process (handling the MQTT connection) -> emqtt https://github.com/emqx/emqtt
- REST+HTTP interface to query and display the measuerments -> cowboy https://github.com/ninenines/cowboy

Build
-----

    $ rebar3 compile
