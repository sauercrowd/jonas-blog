---
title: "Automate deployments with Kubernetes using Kubeadm, LetsEncrypt and Traefik"
date: 2017-09-20T22:11:35+02:00
draft: false
---
## Introduction

I'm wanted to have a functional Kubernetes cluster since I first heard about Kubernetes (which might 1,5 years ago), but I never really had the prerequisities to start (server, domain).

But two days ago I bought a domain, wiped my server and started!

## Goals

- Access the kubernetes dashboard easily with a browser
- Metrics
- Creating new services should be easy, I don't want to change configurations everywhere just to be able to reach it.
- Let's Encrypt certificates should be acquired automatically, I don't want to do that for every subdomain.
- It should be secure

*Spoiler:* All Goals will be satisfied

## Prerequisites
I used the following setup:
- a server:
  - 2 cores
  - 6 GB memory
  - 500GB hdd
  - Ubuntu 16.04
- a domain

Obviously you could use multiple servers to get a real cluster, but I'm sure you need to adapt this guide in order to be successful.

Altough I only used one server I will call the kubernetes setup a cluster from now on.

## Setup Kubernetes

*Note:* The setup of the cluster is based on [this](https://kubernetes.io/docs/setup/independent/install-kubeadm/)

### Install docker

At the time of writing the docker version which is supported by `kubeadm` is luckily in the standard ubuntu repositories, so a

```
sudo apt-get update && sudo apt-get install -y docker.io
```

did the job pretty well.

Okay cool, Docker installed. Half of the work done.

### Set the DNS server for your kubernetes host

This is something I'm not sure everyone needs to do, but in my case my server provider set up a custom dns for it's dns server, which lead to some issues later on.

So you may wan't to save one or two hours debugging and replace the dns server now by editing `/etc/network/interfaces`.

Search for the line `dns-nameservers`, delete the IPs and replace it e.g. with `8.8.8.8`.

A server restart is a good idea to see if the network still operates properly.

### Install kubeadm

Switch to the root user.

First we need to add a custom repo to apt by executing the following

```
apt-get update && apt-get install -y apt-transport-https
curl -s https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add -
cat <<EOF >/etc/apt/sources.list.d/kubernetes.list
deb http://apt.kubernetes.io/ kubernetes-xenial main
EOF
apt-get update
apt-get install -y kubelet kubeadm kubectl
```

After that succeeded, we're ready to deploy Kubernetes

### Deciding which network to use

We need a CNI(Container Network Interface) network solution for the cluster, for example to be able to view your cluster as one big cluster, independent from how many nodes there are.

I decided to use flannel, because I've read about the project multiple times and just wanted to try it out.

I did a Google search why I should use one network provider over the other, but I didn't really find a useful ressource.

Since flannel works really good for me, I'll use it in this tutorial.

If you want to use another provider, check the description [here](https://kubernetes.io/docs/setup/independent/create-cluster-kubeadm/)(at section 3/4).

### Deploy the cluster with kubeadm

Now the moment which we all waited for, deploy the cluster.

It's important that you decided for a network provider before deploy it, because for some networks you need to pass an IP range to `kubeadm`

For flannel that's the case, so we need to invoke (as root):

```
kubeadm init --pod-network-cidr=10.244.0.0/16
```

Wait a few seconds until it's deployed.

Good. Now deploy a network so you're able to start using your cluster.

### Deploy a network

Use `kubectl` (pronounced *cube cuddle*) to deploy flannel.

```
kubectl apply -f https://raw.githubusercontent.com/coreos/flannel/master/Documentation/kube-flannel.yml
kubectl apply -f https://raw.githubusercontent.com/coreos/flannel/master/Documentation/kube-flannel-rbac.yml
```

The commands apply the configurations defined at the URLs to the cluster.
The first configuration creates the flannel deployment itself, the second creates roles so the deployment is able to access everything it needs.

This access configuration is called `rbac` in kubernetes, make sure you use the rbac version of configurations (if available) if you apply custom deployment to your cluster.


`kubectl` is your tool to interact with your cluster. You could deploy serivces, do rolling updates and check pod logs.

You could now check if everything works as intended by executing

```
kubectl get pods --all-namespaces
```

After a few minutes everything should be in a running state, if thats the case:

Congratulations, your cluster is ready to use.

## Now what

Your cluster works and you're able to interact with it via `kubectl`.

Things we will do next:

- Import user certificate to Firefox/Chrome
- Deploy the Kubernetes Dashboard
- Deploy Heapster for metrics (Nice Graphs for the dashboard, yay!)
- Deploy Traefik to be able to do basically everything automagically
- Deploy a Jupyter Notebook server as an example application (Optional)

Let's begin

## Import user certificate to Firefox/Chrome

This is not needed, but I'd recommend it to you, because it will make sure that you're able to reach your kubernetes dashboard without `kubectl proxy` and will even work if traefik stops working.

Now, log into your server and change to the directory `/etc/kubernetes/pki/`

There you'll find two files called `apiserver-kubelet-client.crt` and `apiserver-kubelet-client.key`

Copy these files to your home directory

```
$ sudo cp /etc/kubernetes/pki/apiserver-kubelet-client.* ~/
```

Own these files to you

```
sudo chown $USER:$USER ~/apiserver-kubelet-client.*
```

so you don't need root access from now on.

Firefox and Chrome need `.p12` files, which could be generated using `.crt` and `key` files, so let's do that:

```
openssl pkcs12 -export -in apiserver-kubelet-client.crt -inkey apiserver-kubelet-client.key -out kubernetes.p12
```

Copy this file to all of your devices which you wan't to use to access kubernetes.

You could import these files by

Chrome:
 - Settings->Manage Certificates->Import

 Firefox:
 - Tools->Tools > Options > Advanced > Certificates: View Certificates->Your Certificates

 Try if you're able to reach your cluster by heading to `https://SERVER_IP:6443`
 
 If everything worked out, you should be asked by your Browser which certificate you would wan't to use, select the one we just imported.

 And you should get a JSON with a bunch of API paths.

 ## Deploy the Kubernetes Dashboard
 
 As you might already know, deployment is easy.

 Execute

 ```
 kubectl create -f https://raw.githubusercontent.com/sauercrowd/kube-configs/master/kubernetes-dashboard/kubernetes-dashboard.yaml
 ```

 This will use the configuration on my repo, but I did that so it won't disappear, you might wan't to use the latest/official configuration from [here](https://github.com/kubernetes/dashboard)

 Wait a few seconds, and you should be able to see a kubernetes dashboard pod in the running state when you execute

 ```
 kubectl get pods --all-namespaces
 ```

 Now head over to your browser and go to the url

 ```
 https://SERVER_IP:6443/api/v1/proxy/namespaces/kube-system/services/kubernetes-dashboard/
 ```

 and you should see the dashboard!

 As of the time of writing, the path which should lead you there, namely  `https://SERVER_IP:6443/ui/`, does not work, it does a redirect to the wrong URL.

 It might be because of the fact that kubernetes 1.7 is not fully supported by the latest stable dashboard version, but I'm sure that will change in the future.

 ## Deploy Heapster for metrics

 A Dashboard needs graphs, obviously, so we need Heapster.
 Heapster will collect these metrics and the Kubernetes Dashboard will collect these and integrate some nice graphs, based on these metrics.

 The configuration from the Heapster repository didn't work for me (probably also due to the new Kubernetes version 1.7), but the standalone version from the [kops](https://github.com/kubernetes/kops/tree/master/addons/monitoring-standalone) repository worked pretty well for me, so just use this. (Again, I mirrored it in my own repo)

 ```
 kubectl create -f https://raw.githubusercontent.com/sauercrowd/kube-configs/master/monitoring/heapster-standalone.yaml
 ```

 It will take some time before the metrics appear, but you should able to see nice graphs soon.

 For some reason sometimes there is nothing, sometimes just CPU metrics, sometimes just memory metrics,...

 No idea why, but it's okay for me.

 ## Deploy Traefik as a load balancer

 Now we will deploy Traefik as a load balancer.
 It will make sure that the correct subdomains or paths will be routed to the correct deployments, and it will automatically create letsEncrypt SSL certificates for you, so you really don't need to do anything anoying when deploying new applications to your cluster.

 ### Download the configuration

 First download the configuration:

 ```
wget 'https://raw.githubusercontent.com/sauercrowd/kube-configs/master/traefik/traefik-deployment.yaml'
```

and replace the following lines according to your setup.

```
    email = "myemail@example.com"
    main = "mydomain.org"
        - 10.11.12.13
```

So, setup the email which you wan't to use for Letsencrypt, set the domain for which letsEncrypt should sign the certificates for, and replace `10.11.12.13` with the IP of your server.

### Deploy the RBAC configuration
Before we deploy traefik itself, deploy the roles so everything will work as intended ([original](https://docs.traefik.io/user-guide/kubernetes/))

```
kubectl create -f https://raw.githubusercontent.com/sauercrowd/kube-configs/master/traefik/traefik-rbac.yaml
```

### Deploy Traefik

```
kubectl create -f ./traefik-deployment.yaml

```

