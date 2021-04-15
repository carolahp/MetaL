Class {
	#name : #BaselineOfPharoBootstrapGeneric,
	#superclass : #BaselineOf,
	#category : #BaselineOfPharoBootstrapGeneric
}

{ #category : #baselines }
BaselineOfPharoBootstrapGeneric >> baseline: spec [
	<baseline>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec baseline: 'Tonel' with: [ spec
			repository: 'github://pharo-vcs/tonel:v1.0.13' ].
		spec baseline: 'Hermes' with: [ spec
			repository: 'github://tesonep/hermes:v2.5.1';
  			loads: 'core' ].
		spec baseline: 'Espell2' with: [ spec 
			repository: 'github://carolahp/espell2:generic-bootstrap' ].
		spec
			 package: 'PharoBootstrapGeneric'		
]
]
