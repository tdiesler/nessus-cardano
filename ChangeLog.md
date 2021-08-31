# v1.29.0-rev1

Released: 31-Aug-2021

This is a major feature update and bug fix release.

* Upgrade to cardano-node-1.29.0 ([#73][73])
* Additional RTS flags for optimization of memory usage ([#67][67])
* Add support for RTS options in docker run ([#68][68])
* cardano-cli cannot parse script-data-value with spaces ([#69][69])

[67]: https://github.com/tdiesler/nessus-cardano/issues/67
[68]: https://github.com/tdiesler/nessus-cardano/issues/68
[69]: https://github.com/tdiesler/nessus-cardano/issues/69
[73]: https://github.com/tdiesler/nessus-cardano/issues/73

For details see [v1.29.0-rev1](https://github.com/tdiesler/nessus-cardano/issues?q=milestone%3Av1.29.0-rev1)

# v1.28.0-rev1

Released: 21-Jul-2021

This is a major feature update and bug fix release.

* Add support for cncli on arm64 ([#61][61])
* Add support for Log Navigator ([#62][62])
* Upgrade to cardano-node-1.28.0 ([#63][63])
* Upgrade to gLiveView-1.20.10 ([#64][64])
* Upgrade to cncli-3.1.4 ([#65][65])

[61]: https://github.com/tdiesler/nessus-cardano/issues/61
[62]: https://github.com/tdiesler/nessus-cardano/issues/62
[63]: https://github.com/tdiesler/nessus-cardano/issues/63
[64]: https://github.com/tdiesler/nessus-cardano/issues/64
[65]: https://github.com/tdiesler/nessus-cardano/issues/65

For details see [v1.28.0-rev1](https://github.com/tdiesler/nessus-cardano/issues?q=milestone%3Av1.28.0-rev1)

# v1.27.0-rev3

Released: 15-Jun-2021

This is a minor feature update and bug fix release.

* Simplify stop signal handling by using SIGINT directly ([#49][49])
* Migrate monit to system service ([#60][60])

[49]: https://github.com/tdiesler/nessus-cardano/issues/49
[60]: https://github.com/tdiesler/nessus-cardano/issues/60

For details see [v1.27.0-rev3](https://github.com/tdiesler/nessus-cardano/issues?q=milestone%3Av1.27.0-rev3)

# v1.27.0-rev2

Released: 05-Jun-2021

This is a minor feature update and bug fix release.

* Client-side handling of custom peers ([#55][55])
* Review M/Monit license handling ([#57][57])
* Upgrade to cncli-2.1.1 ([#56][56])

[55]: https://github.com/tdiesler/nessus-cardano/issues/55
[56]: https://github.com/tdiesler/nessus-cardano/issues/56
[57]: https://github.com/tdiesler/nessus-cardano/issues/57

For details see [v1.27.0-rev2](https://github.com/tdiesler/nessus-cardano/issues?q=milestone%3Av1.27.0-rev2)

# v1.27.0-rev1

Released: 14-May-2021

This is a major feature update and bug fix release.

* Migrate k8s node configuration to StatefulSets ([#36][36])
* Add support for testnet configs ([#50][50])
* Upgrade to cardano-node-1.27.0 ([#53][53])

[36]: https://github.com/tdiesler/nessus-cardano/issues/36
[50]: https://github.com/tdiesler/nessus-cardano/issues/50
[53]: https://github.com/tdiesler/nessus-cardano/issues/53

For details see [v1.27.0-rev1](https://github.com/tdiesler/nessus-cardano/issues?q=milestone%3Av1.27.0-rev1)

# v1.26.2-rev2

Released: 22-Apr-2021

This is a minor feature update and bug fix release.

* Missing libselinux.so since 1.26.1 ([#46][46])
* Restart containers in docker compose script ([#47][47])
* Upgrade to cncli-2.0.3 ([#48][48])

[46]: https://github.com/tdiesler/nessus-cardano/issues/46
[47]: https://github.com/tdiesler/nessus-cardano/issues/47
[48]: https://github.com/tdiesler/nessus-cardano/issues/48

For details see [v1.26.2-rev2](https://github.com/tdiesler/nessus-cardano/issues?q=milestone%3Av1.26.2-rev2)

# v1.26.2-rev1

Released: 19-Apr-2021

This is a required system update.

* Upgrade to cardano-node-1.26.2 ([#45][45])
* Run monit/mmonit on debian ([#44][44])

[44]: https://github.com/tdiesler/nessus-cardano/issues/44
[45]: https://github.com/tdiesler/nessus-cardano/issues/45

For details see [v1.26.2-rev1](https://github.com/tdiesler/nessus-cardano/issues?q=milestone%3Av1.26.2-rev1)

# v1.26.1-rev1

Released: 08-Apr-2021

This is a required system update.

* Upgrade to cardano-node-1.26.1 ([#42][42])
* Migrate monit/mmonit build to Nix ([#41][41])
* Upgrade to gLiveView-1.20.3
* Upgrade to mmonit-3.7.7
* Upgrade to cncli-1.5.1

[41]: https://github.com/tdiesler/nessus-cardano/issues/41
[42]: https://github.com/tdiesler/nessus-cardano/issues/42

For details see [v1.26.1-rev1](https://github.com/tdiesler/nessus-cardano/issues?q=milestone%3Av1.26.1-rev1)

# v1.25.1-rev3

Released: 14-Mar-2021

This is a minor feature update and bug fix release.

* Align node socket name with upstream task ([#35][35])
* Add build instructions to readme task ([#32][32])
* Add support for cardano-db-sync feature ([#31][31])
* Update to Cabal-3.4.0.0 task ([#10][10])
* Upgrade to GHC-8.10.4 task ([#7][7])
* Initial sync may block with CPU maxed out bug ([#6][6])

[6]: https://github.com/tdiesler/nessus-cardano/issues/6
[7]: https://github.com/tdiesler/nessus-cardano/issues/7
[10]: https://github.com/tdiesler/nessus-cardano/issues/10
[31]: https://github.com/tdiesler/nessus-cardano/issues/31
[32]: https://github.com/tdiesler/nessus-cardano/issues/32
[35]: https://github.com/tdiesler/nessus-cardano/issues/35

For details see [v1.25.1-rev3](https://github.com/tdiesler/nessus-cardano/issues?q=milestone%3Av1.25.1-rev3)

# v1.25.1-rev2

Released: 05-Mar-2021

This is a major feature update.

* Provide high quality multiarch docker image and k8s support ([#28][28])
* Add support for docker compose ([#29][29])
* Split nessusio/cardano into cardano-node and cardano-tools ([#30][30])

[28]: https://github.com/tdiesler/nessus-cardano/issues/28
[29]: https://github.com/tdiesler/nessus-cardano/issues/29
[30]: https://github.com/tdiesler/nessus-cardano/issues/30

For details see [v1.25.1-rev2](https://github.com/tdiesler/nessus-cardano/issues?q=milestone%3Av1.25.1-rev2)

# v1.25.1-rev1

Released: 28-Jan-2021

This is a required system update.

* Upgrade to cardano-node-1.25.1 ([#25][25])

[25]: https://github.com/tdiesler/nessus-cardano/issues/25

For details see [v1.25.1-rev1](https://github.com/tdiesler/nessus-cardano/issues?q=milestone%3Av1.25.1-rev1)

# v1.24.2-rev7

Released: 19-Jan-2021

This is a minor bug fix release.

* Docker restart may leave topology cron unstarted ([#23][23])
* Prevent outgoing update connections to guild-operators ([#24][24])

[23]: https://github.com/tdiesler/nessus-cardano/issues/23
[24]: https://github.com/tdiesler/nessus-cardano/issues/24

For details see [v1.24.2-rev7](https://github.com/tdiesler/nessus-cardano/issues?q=milestone%3Av1.24.2-rev7)

# v1.24.2-rev6

Released: 12-Jan-2021

This is a minor bug fix release. However, it changes the naming of config parameers in an incompatible way.

* Shelley misspelled in various places as Shelly ([#22][22])

[22]: https://github.com/tdiesler/nessus-cardano/issues/22

For details see [v1.24.2-rev6](https://github.com/tdiesler/nessus-cardano/issues?q=milestone%3Av1.24.2-rev6)

# v1.24.2-rev5

Released: 10-Jan-2021

This is a feature update release

* Add support for slot leader logs ([#21][21])

[21]: https://github.com/tdiesler/nessus-cardano/issues/21

For details see [v1.24.2-rev5](https://github.com/tdiesler/nessus-cardano/issues?q=milestone%3Av1.24.2-rev5)

# v1.24.2-rev4

Released: 08-Jan-2021

This is a minor bug fix release

* gLiveView monitor does not respond to SIGHUP ([#19][19])

[19]: https://github.com/tdiesler/nessus-cardano/issues/19

For details see [v1.24.2-rev4](https://github.com/tdiesler/nessus-cardano/issues?q=milestone%3Av1.24.2-rev4)

# v1.24.2-rev3

Released: 07-Jan-2021

This is an important bug fix release

* No graceful shutdown on docker stop ([#11][11])

[11]: https://github.com/tdiesler/nessus-cardano/issues/11

For details see [v1.24.2-rev3](https://github.com/tdiesler/nessus-cardano/issues?q=milestone%3Av1.24.2-rev3)

# v1.24.2-rev2

Released: 04-Jan-2021

This is a minor feature update

* Use dynamic public IP in topology updater ([#8][8])
* Add rotating log file to default mainnet config ([#14][14])

[8]: https://github.com/tdiesler/nessus-cardano/issues/8
[14]: https://github.com/tdiesler/nessus-cardano/issues/14

For details see [v1.24.2-rev2](https://github.com/tdiesler/nessus-cardano/issues?q=milestone%3Av1.24.2-rev2)

# v1.24.2-rev1

Released: 30-Dec-2020

This is our Initial release. It comes with

* Multi arch docker image for amd64 & arm64 ([#2][2], [#3][3])
* Containerized M/Monit ([#1][1])
* Built-in topology updater ([#17][17])
* Built-in gLiveView ([#18][18])

For details see [v1.24.2-rev1](https://github.com/tdiesler/nessus-cardano/issues?q=milestone%3Av1.24.2-rev1)

[1]: https://github.com/tdiesler/nessus-cardano/issues/1
[2]: https://github.com/tdiesler/nessus-cardano/issues/2
[3]: https://github.com/tdiesler/nessus-cardano/issues/3
[17]: https://github.com/tdiesler/nessus-cardano/issues/17
[18]: https://github.com/tdiesler/nessus-cardano/issues/18
