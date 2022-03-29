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
				with: [ spec repository: 'github://pharo-vcs/tonel:v1.0.17' ].
			spec
				baseline: 'Hermes'
				with: [ spec
						repository: 'github://tesonep/hermes:v2.6.0';
						loads: 'core' ].
			"spec
				baseline: 'Espell2'
				with: [ spec repository: 'github://carolahp/espell2:merge' ]."
			spec
				baseline: 'LanguageMetamodels'
				with: [ spec repository: 'github://carolahp/LanguageMetamodels:v1.0.0' ].
			spec
				baseline: 'PBManager'
				with: [ spec repository: 'github://carolahp/PBManager:v2.1' ].
			spec package: 'MetaLBootstrap' ]
]

{ #category : #baselines }
BaselineOfMetaLBootstrap >> postload: loader package: packageSpec [
	PBBootstrapGeneric
		baseLanguageModelRepository:
			(IceRepository registry
				detect: [ :repo | repo name = 'KernelExplicitMetaclasses' ]
				ifNone: [ (IceRepositoryCreator new
						addRemote: 'https://github.com/carolahp/KernelExplicitMetaclasses.git';
						createRepository) register ]) 
]
