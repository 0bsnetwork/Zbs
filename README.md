# Zbs [![Build Status](https://travis-ci.org/0bsnetwork/Zbs.svg?branch=master)](https://travis-ci.org/0bsnetwork/Zbs) [![](https://images.microbadger.com/badges/version/0bsnetwork/zbs-testnet.svg)]

In the master branch there is a code with functions that is under development. The latest release for each network can be found in the [Releases section](https://github.com/0bsnetwork/Zbs/releases), you can switch to the corresponding tag and build the application.


# Instructions for Running

Modify the zbs-testnet.conf file where neccesary

Install as a service on Ubuntu / Debian

```
sudo dpkg -i zbs_0.14.6_all.deb

systemctl start zbs.service

Monitor: journalctl -u zbs.service -f
```

Run as a Java file

```
java -jar zbs-all-0.14.6.jar zbs-testnet.conf
```
