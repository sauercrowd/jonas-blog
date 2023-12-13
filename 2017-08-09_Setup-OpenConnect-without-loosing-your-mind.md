---
title: "Openconnect without getting annoyed"
date: 2017-08-09T14:56:14+02:00
draft: false
---

# Intro

I need to use opennecect, an opensource implementation for Cisco Anyconnect VPN, for my work, and it doesn't really work that well.
Usually some sites are really slow or not load at all when using Ciscos Anyconnect, so why not make life easier by putting that into a docker container.

## The Scenario

There are a few Hosts I need to reach from my browser and some servers via SSH.

So why even send all net traffic to my employee?

## The Solution

The Solution to this issue is to put openconnect into a container, add a proxy server and only pass the stuff which needs to reach the Work VPN to the Container.

To add a certain level of security, the proxy should be password protected, and should only listen on the localhost.

### Container

Easy one, just install and add some custom scripts for startup.

I use squid3 as a proxy server because I got some experience with it, but every proxy server should be fine.

I already build a Dockerfile a few months ago [github.com/sauercrowd/openconnect-proxy-docker](https://github.com/sauercrowd/openconnect-proxy-docker).

### Usage

You need apach2-utils for password generation and corkscrew to proxy the ssh connections via an http proxy.
There's a script called `magic.sh`, but I would recommend only using it for starting the container and probably adding users to the proxy.

Before executing `./magic.sh start-container`, make sure that you replace the 3 variables in the bash script.

Okay, so now the container should be running.
Add a new user with `./magic.sh adduser jonas` and enter a password.

Everything should work now, so you should be able to use it as a http proxy in your browser (I'd recommend using a proxy switcher, so you only proxy the hosts which need to be forwarded).

### SSH

To use ssh, you need to update your `~/.ssh/config` which is probably empty or doesn't even exist. If not, create it.

Now add the lines for every wildcard pattern which should be matched:

```
Host 123.456.*
    ProxyCommand corkscrew 127.0.0.1 3128 %h %p ~/.ssh/proxyauth
```

and create a new file `~/.ssh/proxyauth` where you add your users/passwords in the following form

```
user:password
```

Your password needs to be in plaintext.

Now, if you do an

```
ssh user@123.456.1.1
```

ssh will use your proxy. Profit!