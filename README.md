# PharoBootstrapGeneric
A generic bootstrapper for Pharo like languages. 
This is an alternative to the bootstrapper provided in the official Pharo repository, but it uses mostly the same backend.

# Installation
The next code will install the bootstrapper and its dependencies (including Espell and therefore the VMMaker).
It takes time to load.
```Smalltalk
[ Metacello new
    baseline: 'DebuggableASTInterpreter';
    repository: 'github://carolahp/PharoBootstrapGeneric';
    load ] on: Warning do: #resume.
```
