Class {
	#name : #BaselineOfMetaLBootstrap,
	#superclass : #BaselineOf,
	#category : #BaselineOfMetaLBootstrap
}

{ #category : #baselines }
BaselineOfMetaLBootstrap >> baseline: spec [
	<baseline>
	spec
		for: #common
		do: [ spec postLoadDoIt: #postload:package:.
			spec
				baseline: 'Tonel'
				with: [ spec repository: 'github://pharo-vcs/tonel:v1.0.13' ].
			spec
				baseline: 'Hermes'
				with: [ spec
						repository: 'github://tesonep/hermes:v2.5.1';
						loads: 'core' ].
			spec
				baseline: 'Espell2'
				with: [ spec repository: 'github://carolahp/espell2:v2.0' ].
			spec
				baseline: 'LanguageMetamodels'
				with: [ spec repository: 'github://carolahp/LanguageMetamodels:main' ].
			spec
				baseline: 'PBManager'
				with: [ spec repository: 'github://carolahp/PBManager:v2.0' ].
			spec package: 'MetaLBootstrap' ]
]

{ #category : #baselines }
BaselineOfMetaLBootstrap >> postload: loader package: packageSpec [
	| url name |
	
	url := 'https://github.com/carolahp/PharoCandleSrc'.
	"https://github.com/carolahp/KernelExplicitMetaclasses.git" 
	name := 'PharoCandleSrc'.
	"KernelExplicitMetaclasses"
	PBBootstrapGeneric
		baseLanguageModelRepository:
			(IceRepository registry
				detect: [ :repo | repo name = name ]
				ifNone: [ (IceRepositoryCreator new
						addRemote: url;
						createRepository) register ]) 
]
