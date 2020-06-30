Class {
	#name : #PBBootstrapGeneric,
	#superclass : #Object,
	#instVars : [
		'architecture',
		'pbRepository',
		'builder'
	],
	#category : #'PharoBootstrapGeneric-core'
}

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
		"installOnlyMarkedMethods: self installOnlyMarkedMethods;"
		installMethods;
		installExtensionMethods
		
]

{ #category : #'bootstrap4-install process' }
PBBootstrapGeneric >> installProcess [
	| association process |
	builder initializeSpecialObjectArray.
	
	self bootstrapInterpreter
		evaluateCode:  '
	Processor := PCProcessorScheduler basicNew.
	Processor initProcessLists.'
		withTemps: #().

	
	
	association := (builder classNamed: #Association) new
		key: builder objectSpace nilObject;
		value: (builder bootstrapEnvironmentAt: #Processor);
		yourself.
	
	builder objectSpace backend processorAssociation: association .
	process := builder objectSpace createProcessWithPriority: 3 doing: 'PCSystem start' withTemps: #().
 	builder objectSpace installAsActiveProcess: process withProcessor: (builder bootstrapEnvironment at: #Processor).
	
	self followForwardingPointers.

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
