## Build the Stake Pool Monitor

```
docker build -t nessusio/cardano-pool-monitor ./context
```

## Run the Stake Pool Monitor

The monitor is configurable via environment variables.
All of which (except BLOCKFROST_API_KEY) are optional with defaults shown below.

When ticker list is `UNDEFINED`, the monitor selects the first `MONITOR_TICKER_SIZE`
pools that are returned from Blockfrost. This is mainly for testing. Otherwise you
would want to define a space separated list of specific pool tickers.

```
docker run --detach \
  --name="monitor" \
  -e BLOCKFROST_API_KEY="$BLOCKFROST_API_KEY" \
  -e BLOCKFROST_NETWORK="mainnet" \
  -e MONITOR_TICKER_LIST="UNDEFINED" \
  -e MONITOR_TICKER_SIZE=100 \
  -e MONITOR_MAX_FIXED=350000000 \
  -e MONITOR_MAX_FIXED_INCREASE=0.0 \
  -e MONITOR_MAX_MARGIN=0.03 \
  -e MONITOR_MAX_MARGIN_INCREASE=0.0 \
  -e MONITOR_ENDLESS=true \
  -e MONITOR_SLEEP=2000 \
  nessusio/cardano-pool-monitor

docker logs -n 200 -f monitor
```

Typical output would look liks this ...

```
[11-10-2021 20:12:42] INFO Start monitoring 30 pools
[11-10-2021 20:12:43] INFO Loading metadata for 100 stake pools
...................20..........30
[11-10-2021 20:12:57] INFO Monitor 30 stake pools
[11-10-2021 20:12:57] INFO pool1q80jjs53w0fx836n8g38gtdwr8ck5zre3da90peuxn84sj3cu0r 000 [340000000/340000000, 0.005/0.005] - OK
[11-10-2021 20:13:00] INFO pool1qqq6qqa0hpzvumv5p87ynczfmdj557xuwlc3289ke42g72z7f74 AHL [340000000/340000000, 0.018/0.018] - OK
[11-10-2021 20:13:02] INFO pool1qqqqqdk4zhsjuxxd8jyvwncf5eucfskz0xjjj64fdmlgj735lr9 ATADA [340000000/340000000, 0.01/0.01] - OK
[11-10-2021 20:13:05] INFO pool1qzlw7z5mutmd39ldyjnp8n650weqe55z5p8dl3fagac3ge0nx8l BCSH [340000000/340000000, 0.02/0.02] - OK
[11-10-2021 20:13:07] INFO pool1qzlwlpcsgflr9z3f24fg836tyq45p0kf5cnrp20s8v0psp6tdkx BCSH0 [340000000/340000000, 0.02/0.02] - OK
[11-10-2021 20:13:09] WARN pool1m83drqwlugdt9jn7jkz8hx3pne53acfkd539d9cj8yr92dr4k9y BKIND [400000000/400000000, 0.04/0.04] - Bad Fixed Cost
[11-10-2021 20:13:12] INFO pool1dmqzwuql5mylffvn7ln3pr9j7kh4gdsssrmma5wgx56f6rtyf42 BNTY1 [340000000/340000000, 0.03/0.03] - OK
[11-10-2021 20:13:14] INFO pool1fh4sh2telea0tfdy39h0drx6uq566yt9gf24edpz7335sclx39z CANUK [340000000/340000000, 0.02/0.02] - OK
[11-10-2021 20:13:16] INFO pool13wzpwavanfpkr4r4j30plrlc6dn6p822lkt22zdllmy7q8y7s90 CARBO [340000000/340000000, 0/0.001] - OK
[11-10-2021 20:13:19] WARN pool1e556526sqwugxwnmeumt9lhj5jukklg8vv3ynk75xt9vs7adr5y CHEAP [500000000/500000000, 0.009/0.009] - Bad Fixed Cost
[11-10-2021 20:13:21] INFO pool1v4wgyctxyxlcf90qtgflznv5f4fcm6cy0z80mchctekt20d3cww CPX [340000000/340000000, 0.01/0.01] - OK
[11-10-2021 20:13:23] INFO pool1vx9tzlkgafernd9vpjpxkenutx2gncj4yn88fpq69823qlwcqrt CRDNS [340000000/340000000, 0.02/0.02] - OK
[11-10-2021 20:13:26] INFO pool12584mjtgz3fhgpx823qht56gycnfnezg6aqqthazv4qdxkd5c46 DOLCA [340000000/340000000, 0.02/0.02] - OK
[11-10-2021 20:13:28] INFO pool1g0hsnjnt0du3kp33gjua3npncngr3yg4ak2zfdhd4zvmy6r8hyd EGGS [340000000/340000000, 0.02/0.02] - OK
[11-10-2021 20:13:30] INFO pool1lagcrwhhy24grdxpetj5vwwey6z8ncp7gx9gdd5p7gnd5f6urcc GAMER [345000000/345000000, 0.03/0.03] - OK
[11-10-2021 20:13:33] WARN pool14kg79f3v5lqfvp5etqvmy6m9kuzpnd80w5cl44qrl8tpuk00quc HEX [340000000/340000000, 0.042/0.042] - Bad Margin Cost
[11-10-2021 20:13:35] INFO pool1u899yml96tf2cf4zkw4490mnhf832pk6fefnaluvt7487jmqzhg IWH [340000000/340000000, 0.02/0.02] - OK
[11-10-2021 20:13:37] INFO pool1p9sda64t6l9802tsu2fj6phvt9xfqgcpjucyr3kek8wzurmn8rz K8S [340000000/340000000, 0/0.001] - OK
[11-10-2021 20:13:39] INFO pool1krznhwu3h5pu2stuslr53z5rme50t6384macsmylcnljwc7yjky LEAF [340000000/340000000, 0.02/0.02] - OK
[11-10-2021 20:13:42] INFO pool1jhzf2mm6zdlhl6w89uhgx8nq8p6ykcc86qq58vjy0ejyxrrksfj LOVE [340000000/340000000, 0.03/0.03] - OK
[11-10-2021 20:13:44] INFO pool1q3xdrewrldt06rpjx6pzguzm847ujxnmajxrsr7dsthscz4htlv MICRO [340000000/340000000, 0/0.001] - OK
[11-10-2021 20:13:47] WARN pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy NUTS [340000000/340000000, 0.049/0.049] - Bad Margin Cost
[11-10-2021 20:13:49] INFO pool1z5uqdk7dzdxaae5633fqfcu2eqzy3a3rgtuvy087fdld7yws0xt OCTAS [340000000/340000000, 0.009/0.009] - OK
[11-10-2021 20:13:51] INFO pool1pk7cjkcdsawxf25dwml0ely8vhjx7wp7nqans3jwatanceyqae0 RIOT [340000000/340000000, 0.025/0.025] - OK
[11-10-2021 20:13:54] INFO pool124lm97s6f4satl7xz0ulzgg6tv30tskry3zcntwrz68n60v5yne SHARP [340000000/340000000, 0.025/0.025] - OK
[11-10-2021 20:13:56] INFO pool13n4jzw847sspllczxgnza7vkq80m8px7mpvwnsqthyy2790vmyc SPACE [340000000/340000000, 0.03/0.03] - OK
[11-10-2021 20:13:58] INFO pool16tcjctesjnks0p8sfrlf8f3d3vrp2fdn2msy80sgg3cdjtayu3z TAPSY [340000000/340000000, 0.01/0.01] - OK
[11-10-2021 20:14:00] INFO pool18xykgvvvxej2e5ap04ks72d8fdke4qsqeh30satznyrtj88634d TQWS [340000000/340000000, 0/0.001] - OK
[11-10-2021 20:14:03] INFO pool1df9rj4n0t3zlpak7xnh4ue6t3yh9zlw7a02w4l8askp77up25rt VEGAS [340000000/340000000, 0.02/0.02] - OK
[11-10-2021 20:14:05] INFO pool1c8k78ny3xvsfgenhf4yzvpzwgzxmz0t0um0h2xnn2q83vjdr5dj ZEN [340000000/340000000, 0/0.001] - OK
```

Enjoy
