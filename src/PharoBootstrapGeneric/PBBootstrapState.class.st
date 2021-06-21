Class {
	#name : #PBBootstrapState,
	#superclass : #Object,
	#instVars : [
		'currentStage'
	],
	#classVars : [
		'ClassesStage',
		'InitImageStage',
		'InitializingImageStage',
		'MethodsStage',
		'ObjectsStage',
		'ProcessStage',
		'StubsStage',
		'WriteImageStage'
	],
	#category : #'PharoBootstrapGeneric-builders'
}

{ #category : #accessing }
PBBootstrapState class >> initializeStages [
	InitImageStage := #initImage.
	StubsStage := #stubs.
	ObjectsStage := #objects.
	ClassesStage := #classes.
	MethodsStage := #methods.
	ProcessStage := #process.
	WriteImageStage := #writeImage 
	
]

{ #category : #accessing }
PBBootstrapState class >> stages [
	{ InitImageStage.
	StubsStage.
	ObjectsStage.
	ClassesStage.
	MethodsStage.
	ProcessStage.
	WriteImageStage }
	
]

{ #category : #accessing }
PBBootstrapState >> imageIsInitialized [
	^ self stages indexOf: currentStage > InitImageStage
]

{ #category : #accessing }
PBBootstrapState >> initialize [ 
	super initialize.
	self class initializeStages.
]

{ #category : #accessing }
PBBootstrapState >> initializingImage [
	currentStage := InitImageStage
]

{ #category : #accessing }
PBBootstrapState >> installingStubs [
	currentStage := StubsStage
]

{ #category : #accessing }
PBBootstrapState >> stages [
	^ self class stages
]

{ #category : #accessing }
PBBootstrapState >> stubsAreInstalled [
	^ self stages indexOf: currentStage > StubsStage 
]
