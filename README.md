# PharoBootstrapGeneric
Bootstrapper to generate images of small languages, which can be different from Pharo but run on top of the Pharo Virtual Machine (only 32 bits for the moment).
Bootstrapper is a Pharo application that takes the definition of a new language and generates a file with extension .image, ready to be executed by the Pharo VM.

Generated images are small, thus we call them kernels.


This is an alternative to the bootstrapper provided in the official Pharo repository, but it uses part of [@Guillep](https://github.com/guillep/) backend.

# Requirements
<strong>Install only in a Pharo 8.0 image</strong>

# Installation
The next code will install the bootstrapper and its dependencies (including [Espell](https://github.com/carolahp/espell/) and therefore the [Virtual Machine Simulator libraries](https://github.com/OpenSmalltalk/opensmalltalk-vm)).
The VM Simulator is huge, so it will take a long time to load (around 20 minutes).

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
"instantiate the language model"
myLanguage := (MyLanguage 
	named: 'MyLanguage' 
	withEntryPoint: 'System log: ''hello world''. 
        System quit.').

"generate the kernel and execute it using the external Pharo VM"
myLanguage generateAndRun.
```
## Debugging the generated kernel
When the Pharo VM fails to execute the generated kernel, it is possible to debug the VM code using the Pharo debugger by loadinig the generated kernel into Pharo and executing the kernel using the Pharo VM simulator.
```Smalltalk
"load the generated kernel file into the host (current image) and executes it using the VM simulator 
(useful for debugging VM code using the Pharo debugger"
myLanguage runImageLoadedFromFile.
```	

# Examples
Example Languages are offered in the repository [LanguageMetamodels](https://github.com/carolahp/LanguageMetamodels).
