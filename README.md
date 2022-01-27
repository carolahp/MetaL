# MetaL 
Metal is an application to bootstrap images of small languages (kernels) which run on top of the Pharo8.0 Virtual Machine.
Bootstrapped languages can be different from Pharo.

MetaL takes the definition of a new language and generates the kernel as a file with extension .image, ready to be executed by the Pharo8.0 VM.

MetaL is an alternative to the bootstrapper provided in the official Pharo repository, but it uses part of [@Guillep](https://github.com/guillep/) backend.

# Installation
<strong>Install only in a Pharo 8.0 image</strong>

The next code will install the bootstrapper and its dependencies (including [Espell2](https://github.com/carolahp/espell2/) and therefore the [Virtual Machine Simulator libraries](https://github.com/OpenSmalltalk/opensmalltalk-vm)).

The VM Simulator is huge, so it will take a long time to load (around 15 minutes).
<strong>You may get an Iceberg error the first time you execute the code or the image may crash, if that happens, execute the code again</strong>

```Smalltalk
[ Metacello new
    baseline: 'MetaLBootstrap';
    repository: 'github://carolahp/MetaL';
    load ] on: Warning do: #resume.
```

# Examples
A good way to learn MetaL is checking existing examples. Check subclasses of ```LanguageTest``` to see how different kernels are generated.

# Generating your first custom Kernel
Example Languages [LanguageMetamodels](https://github.com/carolahp/LanguageMetamodels), such as ObjVLisp and Candle are included.
The simplest one is ObjVLisp.
Generate it and execute it in a Pharo VM using the following code.
The message 'Hello from ObjVLisp', which was logged by the kernel execution is shown in the Transcript.

```Smalltalk
objvlisp := (ObjVLispMinLanguage 
    newWithName: 'ObjVLisp' 
    withEntryPoint: 'System log: ''Hello from ObjVLisp''. 
        	     System quit.').
objvlisp generate.
objvlisp writeImage.
objvlisp imageInDisk executeInVM.

```

# Generating custom Kernels
To define your own languages, follow the next steps.

## Extending the language meta-model
Extend the class LanguageModel to define the class representing your language, as follows.
Implement corresponding hooks.
```Smalltalk
LanguageModel subclass: #MyLanguage
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'MyPackage'
```
## Creating the language model and generating the kernel
Execute the next code in the Playground to generate your own kernel.

```Smalltalk

"instantiate the language model"
language := (MyLanguage 
	newWithName: 'MyLanguage' 
	withEntryPoint: 'System log: ''hello world''. 
        System quit.').

"load the base language definition, or load one created by yourself (tonel format)"
language loadBaseLanguageModelDefinitions.
"OR"
language loadLanguageModelDefinitions: '/path/to/repo' asFileReference.

"you can browse, edit and export the language model from calypso"
language browse.

"generate the kernel in memory"
language generate.

"before writing the kernel to disk, you can execute code directly in your kernel, and even debug it!"
language evaluateCode: '#(1 2 3) size'.
language debugCode: '#(1 2 3) size'.

"write the kernel to disk"
language writeImage.

"execute it using the external Pharo VM"
language imageInDisk executeInVM.

```
