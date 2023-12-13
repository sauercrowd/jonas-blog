---
title: "Fixing Bluetooth on Fedora 35"
date: 2022-03-17T22:00:00+01:00
draft: false
---

After updating Fedora, I ended up with a dysfunctional bluetooth - (again?).

To avoid the same pain for everyone else - here's how to fix it.

## The fix

The issue was caused by a missing patch, so first check if you're running into a similiar issue as I was by running

```
dmesg | grep -i bluetooth
```

If you now see something like the following, that means you're covered!

```
Bluetooth: hci1: BCM: Patch brcm/BCM20702A1-0b05-17cb.hcd not found
```

Head over to [winterheart/broadcom-bt-firmware](https://github.com/winterheart/broadcom-bt-firmware) and pretty much just follow the instructions!

To be more specific:

- Find the file mentioned in dmesg in the `brcm` folder in the repo mentioned above.

- Download it into `/lib/firmware/brcm`
```
$ cd /lib/firmware/brcm
$ sudo curl -o BCM20702A1-13d3-3411.hcd https://github.com/winterheart/broadcom-bt-firmware/raw/master/brcm/BCM20702A1-13d3-3411.hcd
```

- Reboot
