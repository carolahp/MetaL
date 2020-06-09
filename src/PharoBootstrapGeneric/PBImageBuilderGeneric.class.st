Class {
	#name : #PBImageBuilderGeneric,
	#superclass : #Object,
	#instVars : [
		'languageDefinition',
		'espellBackend',
		'bootstrapEnvironment',
		'objectSpace',
		'bootstrapInterpreter',
		'debuggerInterpreter',
		'freeListsOop',
		'classLoader'
	],
	#category : #'PharoBootstrapGeneric-builders'
}

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> allocateClassTable [
	"Allocate the root of the classTable plus enough pages to accomodate all classes in
	 the classToIndex map.  Don't fill in the entries yet; the classes have yet to be cloned."
	| tableRoot page maxSize numPages newHeap newOop |
	newHeap := objectSpace backend simulator objectMemory.
	tableRoot := newHeap
					allocateSlots: newHeap classTableRootSlots + newHeap hiddenRootSlots
					format: newHeap arrayFormat
					classIndex: newHeap arrayClassIndexPun.
	newOop := tableRoot.
	"self haltIf: [ newOop = 4239888 ]."
	self assert: (newHeap numSlotsOf: tableRoot) = (newHeap classTableRootSlots + newHeap hiddenRootSlots).
	self assert: (newHeap formatOf: tableRoot) = newHeap arrayFormat.
	self assert: (newHeap classIndexOf: tableRoot) = newHeap arrayClassIndexPun.
	newHeap nilFieldsOf: tableRoot.
	"first page is strong"
	page := newHeap
					allocateSlots: newHeap classTablePageSize
					format: newHeap arrayFormat
					classIndex: newHeap arrayClassIndexPun.
	self assert: (newHeap numSlotsOf: page) = newHeap classTablePageSize.
	self assert: (newHeap formatOf: tableRoot) = newHeap arrayFormat.
	self assert: (newHeap classIndexOf: tableRoot) = newHeap arrayClassIndexPun.
	self assert: (newHeap objectAfter: tableRoot limit: newHeap freeStart) = page.
	"lastClassTablePage := page."
	newHeap nilFieldsOf: page.
	newHeap storePointer: 0 ofObject: tableRoot withValue: page.
	newHeap setHiddenRootsObj: tableRoot.
	
	"this should actually take into account special indexes and immediate classes.
	Skipped for the moment"
	maxSize := (1 << self objectMemory classTableMajorIndexShift) + (self systemDefinition allClasses size * 2).
	
	numPages := (maxSize + newHeap classTableMinorIndexMask / newHeap classTablePageSize) truncated.
	2 to: numPages do:
		[:i|
		page := newHeap
					allocateSlots: newHeap classTablePageSize
					format: newHeap arrayFormat
					classIndex: newHeap arrayClassIndexPun.
		self assert: (newHeap numSlotsOf: page) = newHeap classTablePageSize.
		self assert: (newHeap formatOf: page) = newHeap arrayFormat.
		self assert: (newHeap classIndexOf: page) = newHeap arrayClassIndexPun.
		newHeap fillObj: page numSlots: newHeap classTablePageSize with: newHeap nilObject.
		newHeap storePointer: i - 1 ofObject: tableRoot withValue: page.
		self assert: (newHeap objectAfter: (newHeap fetchPointer: i - 2 ofObject: tableRoot)  limit: newHeap freeStart) = page.
		"lastClassTablePage := page"].
	"and once again to recompute numClassTablePages post building the class table."
	newHeap instVarNamed: 'numClassTablePages' put: nil.
	newHeap setHiddenRootsObj: tableRoot
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> allocateFreeLists [
	"Allocate the freeLists array."
	| newOop |
	freeListsOop := self objectMemory
						allocateSlots: objectSpace backend simulator objectMemory numFreeLists
						format: objectSpace backend simulator objectMemory wordIndexableFormat
						classIndex: objectSpace backend simulator objectMemory wordSizeClassIndexPun.
	self assert: (self objectMemory objectAfter: objectSpace backend trueObject target address) = freeListsOop.
	newOop := freeListsOop.
	"self haltIf: [ newOop = 4239888 ]."
	0 to: objectSpace backend simulator objectMemory numFreeLists - 1 do:
		[:i|
		self objectMemory
			storePointerUnchecked: i
			ofObject: freeListsOop
			withValue: 0].
	self objectMemory initializeFreeSpacePostLoad: freeListsOop.
]

{ #category : #'accessing-constants' }
PBImageBuilderGeneric >> arrayClassIndex [
	
	^ self objectMemory arrayClassIndexPun
]

{ #category : #'bootstrap3-methods' }
PBImageBuilderGeneric >> at: aString ifAbsent: aBlockClosure [ 
	"returns nil because this method is used to get CompiledMethod for generating Compiled Method instances durin the bootstrap"
	^ aBlockClosure value
]

{ #category : #bindings }
PBImageBuilderGeneric >> bindingOf: aName [
	
"
	([ self systemDefinition classNamed: aName ] on: Error do: [ nil ]) isNotNil
		ifTrue: [  "(objectSpace classNamed: aName) mirror asClassMirror" 
			^ EPClassBinding new
				name: aName objectSpace: self;
				yourself ].
	(((self systemDefinition globalVariableNamed: aName) isNotNil) or: [(self bootstrapEnvironment at: aName ifAbsent: [nil]) isNotNil])
		ifTrue: [ ^
			EPGlobalBinding new
				name: aName objectSpace: self;
				yourself ].
	self bootstrapEnvironment do: [:class | |rgClass|
		class isClassMirror ifTrue: [ 
			rgClass := class asClassMirror asRGClass.
			(rgClass classVarNames contains: [:cn | cn = aName])
				ifTrue: [|index|
					index := rgClass classVarNames indexOf: aName.
					^ EPClassVariableBinding new
						name: aName
						ownerName: rgClass theNonMetaClass name asSymbol
						objectSpace: self;
						yourself ]
			 ]
		].
			
	^ nil"
]

{ #category : #accessing }
PBImageBuilderGeneric >> bootstrapEnvironment [
	^ bootstrapEnvironment
]

{ #category : #'accessing-environment' }
PBImageBuilderGeneric >> bootstrapEnvironmentAt: aString [ 
	| classDefinition |
	classDefinition := [self systemDefinition classByRoleOrName: aString] on: PBClassDefinitionNotFound  do: [ ^ nil ].
	^ self bootstrapEnvironment at: classDefinition name ifAbsent: [ nil ]
]

{ #category : #accessing }
PBImageBuilderGeneric >> bootstrapInterpreter [
	^ bootstrapInterpreter
]

{ #category : #'accessing-mapping-mirrors/definitions' }
PBImageBuilderGeneric >> classForClassMirror: anEPClassMirror [

	^ classLoader classForClassMirror: anEPClassMirror
]

{ #category : #accessing }
PBImageBuilderGeneric >> classLoader [
	^ classLoader
]

{ #category : #'remote-classes' }
PBImageBuilderGeneric >> classNamed: aString [ 
	^ self classLoader classNamed: aString
]

{ #category : #'bootstrap2-classes' }
PBImageBuilderGeneric >> createBehaviorFromDefinition: aRGBehavior [ 
	^ self classLoader createBehaviorFromDefinition: aRGBehavior
]

{ #category : #'bootstrap2-classes' }
PBImageBuilderGeneric >> createClasses [
	| definitions total |
	definitions := self systemDefinition allClassesAndTraits sorted: [ :a :b | a name < b name ].
	total := definitions size.
	definitions
		withIndexDo: [ :aClassDefinition :index | | time |
			time := [self createBehaviorFromDefinition: aClassDefinition.] timeToRun.
			
			PBSubStepFinishedSignal emit: ((index printPaddedWith: $0 to: 3), '/' , 
								total asString, ' - ', 
									time asString ,': Built behavior ', 
										aClassDefinition name). ].
	
	PBStepFinishedSignal emit: 'Classes created'
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> createFalse [

	| falseObject |
	
	falseObject := self classLoader 
		createStubInstanceOf: #False 
		classIndex: self classLoader newClassIndex.
	
	objectSpace falseObject: falseObject.
	^ objectSpace falseObject
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> createInitialObjects [
	| systemDictionary |
	self halt.
	
	" Initialize globals in the bootstrap environment "
	self initializeGlobals.
	
	classLoader initializeClassPools.
	
	systemDictionary := (self remoteClass: #SystemDictionary) new asRemoteDictionary.
	systemDictionary at: #asd put: 1.
	(PBHybridDebugger withCode: 'PCDictionary new' temps: {  } interpreter: self debuggerInterpreter interpreter) openWithSpec.
	(PBHybridDebugger withCode: 'sysDic new at: #asd put:1' temps: { 'systemDict' -> (self classLoader classNamed: #SystemDictionary) } interpreter: self debuggerInterpreter interpreter) openWithSpec.
	(self languageDefinition hookSystemDictionaryInitialize: systemDictionary) evaluate. 
	
	"smalltalk := self languageDefinition instructionNewSystemDictionary evaluate."
	"self bootstrapEnvironment at: #Smalltalk put: smalltalk."
	
	"self bootstrapInterpreter evaluateCode: 'Smalltalk globals at: #Smalltalk put: Smalltalk'.
	espellBackend systemDictionary: (smalltalk instanceVariableAtIndex: 1).
	"
	self languageDefinition instructionsToCreateInitialObjects do: [:instruction | instruction evaluate ].
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> createNil [

	| nilObject |
	
	nilObject := self classLoader 
		createStubInstanceOf: #UndefinedObject 
		classIndex: self classLoader newClassIndex.
	
	objectSpace nilObject: nilObject .
	^ objectSpace nilObject
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> createStubForClassNamed: aString [ 
	
	^ self classLoader createStubForClassNamed: aString
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> createStubInstanceOfArrayOfSize: anInteger [ 
	^ self classLoader createStubInstanceOfArrayOfSize: anInteger 
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> createTrue [

	| trueObject |
	
	trueObject := self classLoader 
		createStubInstanceOf: #True 
		classIndex: self classLoader newClassIndex.
	
	objectSpace trueObject: trueObject.
	^ objectSpace trueObject
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> createVMStubs [
	| specialObjectsArray |
	super trace: 'creating initial Objects needed by the VM'.
	
	self createNil.
	self createFalse.
	self createTrue.
	self test01FirstObjectsStubs.
	
	self allocateFreeLists.
	self allocateClassTable.
	
	specialObjectsArray := self createStubInstanceOfArrayOfSize: 60.
	objectSpace specialObjectsArray: specialObjectsArray.
	objectSpace nilObject: objectSpace nilObject.
	objectSpace falseObject: objectSpace falseObject.
	objectSpace trueObject: objectSpace trueObject.
	self classLoader resetClassIndex.
	objectSpace nilObject
		setClass: (self createStubForClassNamed: #UndefinedObject).
	objectSpace falseObject
		setClass: (self createStubForClassNamed: #False).
	objectSpace trueObject
		setClass: (self createStubForClassNamed: #True).
	objectSpace backend
		smallIntegerClass: (self createStubForClassNamed: #SmallInteger).
	objectSpace backend
		characterClass: (self createStubForClassNamed: #Character).
	objectSpace backend
		byteSymbolClass: (self createStubForClassNamed: #Symbol).
	objectSpace backend
		byteStringClass: (self createStubForClassNamed: #String).
	objectSpace backend
		byteArrayClass: (self createStubForClassNamed: #ByteArray).
	objectSpace backend
		associationClass: (self createStubForClassNamed: #Association).
	objectSpace backend
		arrayClass: (self createStubForClassNamed: #Array).
	objectSpace backend
		symbolTableClass: objectSpace backend byteSymbolClass.
	objectSpace backend
		largeNegativeIntegerClass: (self createStubForClassNamed: #LargeNegativeInteger).
	objectSpace backend
		largePositiveIntegerClass: (self createStubForClassNamed: #LargePositiveInteger).
	objectSpace backend
		methodClass: (self createStubForClassNamed: #CompiledMethod).
	objectSpace backend
		floatClass: (self createStubForClassNamed: #Float).
	objectSpace backend
		contextClass: (self createStubForClassNamed: #Context).
	objectSpace backend
		processClass: (self createStubForClassNamed: #Process).
	objectSpace backend
		blockClass: (self createStubForClassNamed: #BlockClosure).
	objectSpace backend
		messageClass: (self createStubForClassNamed: #Message).

	"Point is required in the special objects array because it is used to instantiate point objects faster.
	If we do not put it here, the VM will crash.
	Lots of tests in kernel use it."
	objectSpace backend
		pointClass: (self createStubForClassNamed: #Point).
	objectSpace backend
		processorAssociation: (self classNamed: #Association) basicNew.
	objectSpace backend processorAssociation
		value: (self createStubForClassNamed: #ProcessorScheduler) basicNew.
	objectSpace backend
		specialSelectors: ((self classNamed: #Array) basicNew: 64)
]

{ #category : #accessing }
PBImageBuilderGeneric >> debuggerInterpreter [
	^ debuggerInterpreter
]

{ #category : #accessing }
PBImageBuilderGeneric >> espellBackend [
	^ espellBackend 
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> flushNewSpace [

	| newHeap |
	newHeap := espellBackend objectMemory.
	newHeap initializePostBootstrap.
	
	espellBackend simulator initializeInterpreter: 0.
	espellBackend simulator instVarNamed: 'methodDictLinearSearchLimit' put: SmallInteger maxVal.
	
	"try to move nil, true and false to the new space"
	newHeap flushNewSpace.

	self
		assert: (espellBackend simulator objectMemory objectStartingAt: espellBackend simulator objectMemory oldSpaceStart)
		= objectSpace nilObject target address.

	self
		assert: (espellBackend simulator objectMemory objectAfter: objectSpace nilObject target address)
		= objectSpace falseObject target address.
		
	self
		assert: (espellBackend simulator objectMemory objectAfter: objectSpace falseObject target address)
		= objectSpace trueObject target address.
	
	objectSpace backend initializeExecutionContext.
	self prepareSimulatorForExecution.
]

{ #category : #utils }
PBImageBuilderGeneric >> formatOf: aClassRole [ 
	^ (self systemDefinitionAt: aClassRole) format
]

{ #category : #initializing }
PBImageBuilderGeneric >> initializeBootstrapEnvironment [
	
	espellBackend := EP2Backend for32Bit forBootstrap.
	espellBackend systemDefinition: self systemDefinition.
	
	bootstrapEnvironment := EP2Environment new.
	bootstrapEnvironment builder: self.
	
	"We initialize the object space with a boot symbol table and character table.
	We will later replace them by real ones."
	objectSpace := EP2ObjectSpace new.
	objectSpace backend: espellBackend.
	objectSpace characterTable: (EPExternalCharacterTable new objectSpace: objectSpace; yourself).
	objectSpace symbolTable: (EPExternalSymbolTable new objectSpace: objectSpace; yourself).
	

	classLoader := PBClassLoaderGeneric new.
	classLoader builder: self.
	
	bootstrapInterpreter := EPASTEvaluator new.
	bootstrapInterpreter codeProvider: self.
	bootstrapInterpreter environment: bootstrapEnvironment.
	bootstrapInterpreter objectSpace: objectSpace.
	 
	debuggerInterpreter := DASTEvaluator new.
	debuggerInterpreter codeProvider: self.
	debuggerInterpreter environment: bootstrapEnvironment.
	debuggerInterpreter objectSpace: objectSpace.
	
	objectSpace interpreter: bootstrapInterpreter.
	objectSpace debuggerInterpreter: debuggerInterpreter.
	espellBackend objectSpace: objectSpace
]

{ #category : #globals }
PBImageBuilderGeneric >> initializeGlobals [
	| nilObj |
	nilObj := objectSpace nilObject.
	(self languageDefinition globalVariables) do: [ :globalVarName |
		" Create one global for each global variable in the language definition "
		self bootstrapEnvironment at: globalVarName put: nilObj.
		self assert: (self bootstrapEnvironment at: globalVarName) = nilObj ]
]

{ #category : #'bootstrap3-methods' }
PBImageBuilderGeneric >> installMethods [
	
	| definitions total |
	definitions := self systemDefinition allClassesAndTraits sorted: [ :a :b | a name < b name ].
	total := definitions size.
	definitions withIndexDo: [ :aClassDefinition :index |
		self installMethodsInBehaviorDefinition: aClassDefinition prefixed: (index printPaddedWith: $0 to: 3), '/' , total asString.
	].
	PBStepFinishedSignal emit: 'installed_methods'.
	self checkpoint: 'installed_methods'.
]

{ #category : #'bootstrap3-methods' }
PBImageBuilderGeneric >> installMethodsInBehaviorDefinition: behaviorDefinition prefixed: aPrefix [
	| behaviorMirror  |
	behaviorMirror := bootstrapEnvironment at: behaviorDefinition name.
	self
		installMethodsInBehaviorMapping: behaviorDefinition
		using: self methodBuilder
		onMirror: behaviorMirror
		prefixed: aPrefix.
	self
		installMethodsInBehaviorMapping: behaviorDefinition classSide
		using: self methodBuilder
		onMirror: behaviorMirror basicClass
		prefixed: aPrefix.
]

{ #category : #'bootstrap3-methods' }
PBImageBuilderGeneric >> installMethodsInBehaviorMapping: aBehaviorMapping using: aMethodBuilder onMirror: mirror prefixed: aPrefix [
	| newMethods theClass |
	newMethods := (aMethodBuilder methodsFromDefinition: aBehaviorMapping) sorted: [ :a :b | a selector < b selector ].
	theClass := (self classNamed: aBehaviorMapping theNonMetaClass name) mirror.
	aBehaviorMapping isMeta ifTrue: [ theClass := theClass classSide ].
	newMethods doWithIndex: [ :method :index |
		"PBSubStepFinishedSignal emit: aPrefix, ' - ', index asString, '/', newMethods size asString, ': Installing method ', 
			aBehaviorMapping name, '>>', method selector."
		self installMethod: method inBehavior: theClass.
	].
]

{ #category : #accessing }
PBImageBuilderGeneric >> languageDefinition [
	^ languageDefinition
]

{ #category : #accessor }
PBImageBuilderGeneric >> methodBuilder [
	^ PBMethodCompilerGeneric new builder: self; yourself.	
]

{ #category : #accessing }
PBImageBuilderGeneric >> objectMemory [
	
	^ espellBackend simulator objectMemory
]

{ #category : #accessing }
PBImageBuilderGeneric >> objectSpace [
	^ objectSpace
]

{ #category : #initializing }
PBImageBuilderGeneric >> prepareSimulatorForExecution [

	| savedEndOfMemory |
	self objectMemory initializePostBootstrap.
	
	"savedEndOfMemory := self objectMemory endOfMemory.
	self objectMemory setEndOfMemory: self objectMemory freeOldSpaceStart.
	self objectMemory setEndOfMemory: savedEndOfMemory."
"	self objectMemory interpreter initializeInterpreter: 0.
"	self objectMemory
		initializeNewSpaceVariables;
		bootstrapping: false;
		assimilateNewSegment: (self objectMemory segmentManager segments at: 0);
		setCheckForLeaks: 0;
		runLeakCheckerForFullGC.
]

{ #category : #'accessing-classes-installed' }
PBImageBuilderGeneric >> remoteClass: aString [ 
	^ (self classLoader classNamed: aString) asRemoteClass
]

{ #category : #accessing }
PBImageBuilderGeneric >> simulator [
	^ objectSpace backend simulator objectMemory
]

{ #category : #'accessing-environment' }
PBImageBuilderGeneric >> spurImage [
	^ EP2SpurImage onFirstObject: espellBackend nilObject
]

{ #category : #accessing }
PBImageBuilderGeneric >> systemDefinition [
	^ languageDefinition
]

{ #category : #accessing }
PBImageBuilderGeneric >> systemDefinition: aPBLanguageDefinitionPharoCandle [ 
	languageDefinition := aPBLanguageDefinitionPharoCandle
]

{ #category : #'accessing-systemDefinition' }
PBImageBuilderGeneric >> systemDefinitionAt: aString [ 
	^ self systemDefinition classByRoleOrName: aString
]

{ #category : #'accessing-systemDefinition' }
PBImageBuilderGeneric >> systemDefinitionAt: aString ifAbsent: aBlockClosure [ 
	^ [ self systemDefinition classByRoleOrName: aString ] on: PBClassDefinitionNotFound do: aBlockClosure
]

{ #category : #tests }
PBImageBuilderGeneric >> test01FirstObjectsStubs [
	
	self assert: (self objectSpace backend simulator objectMemory formatOf: self objectSpace nilObject target address) = (self formatOf: #UndefinedObject).
	self assert: (self objectSpace backend simulator objectMemory formatOf: self objectSpace falseObject target address) = (self formatOf: #False).
	self assert: (self objectSpace backend simulator objectMemory formatOf: self objectSpace trueObject target address) = (self formatOf: #True).
]
