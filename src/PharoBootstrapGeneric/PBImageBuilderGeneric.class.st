Class {
	#name : #PBImageBuilderGeneric,
	#superclass : #Object,
	#instVars : [
		'languageDefinition',
		'espellBackend'
	],
	#category : #PharoBootstrapGeneric
}

{ #category : #accessing }
PBImageBuilderGeneric >> espellBackend [
	^ espellBackend 
]

{ #category : #initialize }
PBImageBuilderGeneric >> initialize [ 
	super initialize.
	self initializeBackend
]

{ #category : #initialize }
PBImageBuilderGeneric >> initializeBackend [

	espellBackend := EPSimulatorBackend for32Bit forBootstrap
]

{ #category : #accessing }
PBImageBuilderGeneric >> systemDefinition: aPBLanguageDefinitionPharoCandle [ 
	languageDefinition := aPBLanguageDefinitionPharoCandle
]
