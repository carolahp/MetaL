# PharoBootstrapGeneric
This project contains a bootstrapper that generates images of small languages, which can be different from Pharo, but run on top of the Pharo Virtual Machine.
The bootstrapper is a Pharo application that takes the definition of a new language and generates a file with extension .image, ready to be executed by the Pharo VM.

Generated images are small, thus we call them kernels.


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
# Usage
## Extending the language meta-model
Extend the class LanguageModel to define the class representing your language, as follows
```Smalltalk
LanguageModel subclass: #MyLanguage
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'MyPackage'
```
## Creating the language model and generating the kernel
```Smalltalk
| myLanguage |
"instantiates the language model"
myLanguage := (MyLanguage 
	named: 'MyLanguage' 
	withEntryPoint: 'System log: ''hello world''. 
        System quit.').

"generates the kernel and executes it using an external VM"
myLanguage generateAndRun.
	
"loads the generated kernel file into the host (current image) and executes it using the VM simulator 
(useful for debugging VM code using the Pharo debugger"
myLanguage runImageLoadedFromFile.
```	
