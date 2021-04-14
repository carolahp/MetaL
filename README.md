# PharoBootstrapGeneric
A generic bootstrapper for languages different from Pharo. 

This is an alternative to the bootstrapper provided in the official Pharo repository, but it uses mostly the same backend originally provided by [@Guillep](https://github.com/guillep/).

# Requirements
<strong>Install only in a Pharo 8.0 image</strong>

# Installation
The next code will install the bootstrapper and its dependencies (including [Espell](https://github.com/carolahp/espell/) and therefore the [Virtual Machine Simulator libraries](https://github.com/OpenSmalltalk/opensmalltalk-vm)).
The VM Simulator is huge, so it will take some time to load.

```Smalltalk
[ Metacello new
    baseline: 'PharoBootstrapGeneric';
    repository: 'github://carolahp/PharoBootstrapGeneric';
    load ] on: Warning do: #resume.
```
