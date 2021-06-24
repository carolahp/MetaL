Class {
	#name : #PBBootstrapGeneric,
	#superclass : #Object,
	#instVars : [
		'architecture',
		'pbRepository',
		'builder'
	],
	#classInstVars : [
		'BaseLanguageModelRepository'
	],
	#category : #'PharoBootstrapGeneric-core'
}

{ #category : #'instance creation' }
PBBootstrapGeneric class >> baseLanguageModelLocation [
	^ self baseLanguageModelRepository location
]

{ #category : #'instance creation' }
PBBootstrapGeneric class >> baseLanguageModelRepository [
	^ BaseLanguageModelRepository
]

{ #category : #'instance creation' }
PBBootstrapGeneric class >> baseLanguageModelRepository: anIceRepository [
	^ BaseLanguageModelRepository := anIceRepository
]

{ #category : #'instance creation' }
PBBootstrapGeneric class >> for32Bits [

	^ self forArchitecture: '32'
]

{ #category : #'instance creation' }
PBBootstrapGeneric class >> forArchitecture: architecture [

	^ super new
		architecture: architecture;
		yourself
]

{ #category : #accessing }
PBBootstrapGeneric >> architecture: aString [ 
	architecture := aString
]

{ #category : #accessing }
PBBootstrapGeneric >> builder [
	^ builder
]

{ #category : #destroy }
PBBootstrapGeneric >> destroy [
	builder := nil.
	pbRepository := nil.
]

{ #category : #accessing }
PBBootstrapGeneric >> environment [
	^ self pbRepository ringEnvironment
]

{ #category : #accessing }
PBBootstrapGeneric >> environment: anEnvironment [
	self halt: 'deprecated'
]

{ #category : #'bootstrap0-prepare' }
PBBootstrapGeneric >> initializeBuilder [
	builder := PBImageBuilderGeneric new.
	"builder versionInfo: versionInfo.
	builder imageFileReference: imageReference."
	builder systemDefinition: self environment.
	
]

{ #category : #'creating-classes' }
PBBootstrapGeneric >> installClasses [
	builder createClasses
]

{ #category : #'bootstrap3-methods' }
PBBootstrapGeneric >> installMethods [
	builder 
		installMethods;
		installExtensionMethods
		
]

{ #category : #'bootstrap4-install process' }
PBBootstrapGeneric >> installProcess [
	| association process processor languageDefinition |
	builder initializeSpecialObjectArray.
	languageDefinition := builder systemDefinition.
	processor := (languageDefinition globalWithRole: #Processor) initialValue.
	
	association := (builder remoteClass: #Association) basicNew
		asRemoteAssociation
		key: (#Processor asRemoteObjectInObjectSpace: builder objectSpace);
		value: processor;
		yourself.
	
	builder objectSpace backend processorAssociation: association.
	process := builder objectSpace
		createProcessWithPriority: 3
		doing:
			(languageDefinition entryPointReceiver,' ' , languageDefinition entryPointSelector) asString
		withTemps: #().
	builder objectSpace
		installAsActiveProcess: process
		withProcessor: processor.
		
	builder systemDefinition
		hookSetSpecialObjectsArray: builder objectSpace backend specialObjectsArray.
	
	builder followForwardingPointers.
	self inform: 'Image Generated successfully'
]

{ #category : #'bootstrap1-stubs' }
PBBootstrapGeneric >> installStubsAndInitialObjects [
	builder initializeBootstrapEnvironment.
	builder createVMStubs.
	builder flushNewSpace.
	builder createInitialObjects
]

{ #category : #accessing }
PBBootstrapGeneric >> pbRepository [
	^ pbRepository
]

{ #category : #accessing }
PBBootstrapGeneric >> pbRepository: aPBRepository [ 
	pbRepository := aPBRepository
]

{ #category : #'bootstrap0-prepare' }
PBBootstrapGeneric >> prepareBootstrap [
	self flag: 'implement this step later'
	"self
		exportKernelProtocols;
		exportKernelPackageNames;
		exportAllPackagesIntoMcz;
		exportMonticelloInStFile;
		
		prepareEnvironmentForHermes;
		generateHermesFiles;
		prepareEnvironmentForExport."
]

{ #category : #accessing }
PBBootstrapGeneric >> spurImage [
	^ self builder spurImage
]

{ #category : #'writing-image' }
PBBootstrapGeneric >> writeImageIn: aFileReference [ 
	^ builder writeImageIn: aFileReference
]
