# Zbs [![Build Status](https://travis-ci.org/0bsnetwork/Zbs.svg?branch=master)](https://travis-ci.org/0bsnetwork/Zbs) [![](https://images.microbadger.com/badges/version/0bsnetwork/zbs-testnet.svg)]

In the master branch there is a code with functions that is under development. The latest release for each network can be found in the [Releases section](https://github.com/0bsnetwork/Zbs/releases), you can switch to the corresponding tag and build the application.


# Instructions for Running

Download the zbs-testnet.conf file from this directory and modify it according to the comments present in the file itself (if unsure about what you are doing, it is sufficient to only change the Node name and wallet password, you can leave all else as it is).

Download the appropriate file from the [Releases section](https://github.com/0bsnetwork/Zbs/releases): 
If you are running a Debian-based GNU/Linux system (including Ubuntu or Linux Mint) and want to run the node as a service, download the latest version of the file with the name ending in all.deb and follow the instructions for Option 1 below.
If you are running any other OS, or want to run the node only temporarily, download the latest version of the file with the name ending in .jar and follow the instructions for Option 2 below.


Option 1: Install as a service on Ubuntu / Debian:

```
sudo dpkg -i zbs_0.*_all.deb
```

Start the service (it will in the future run automatically on boot):
```
systemctl start zbs.service
```

Monitor the service: 
```
journalctl -u zbs.service -f
```


Option 2: Run as a Java file

```
java -jar zbs-all-0.*.jar zbs-testnet.conf
```
