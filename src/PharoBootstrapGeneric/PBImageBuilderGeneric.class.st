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

{ #category : #'bootstrap1-stubs-objectMemory' }
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
	self haltIf: [ newOop = 4239888 ].
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

{ #category : #'bootstrap1-stubs-objectMemory' }
PBImageBuilderGeneric >> allocateFreeLists [
	"Allocate the freeLists array."
	| newOop |
	freeListsOop := self objectMemory
						allocateSlots: objectSpace backend simulator objectMemory numFreeLists
						format: objectSpace backend simulator objectMemory wordIndexableFormat
						classIndex: objectSpace backend simulator objectMemory wordSizeClassIndexPun.
	self assert: (self objectMemory objectAfter: objectSpace backend trueObject target address) = freeListsOop.
	newOop := freeListsOop.
	self haltIf: [ newOop = 4239888 ].
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

{ #category : #accessing }
PBImageBuilderGeneric >> bootstrapEnvironment [
	^ bootstrapEnvironment
]

{ #category : #'accessing-environment' }
PBImageBuilderGeneric >> bootstrapEnvironmentAt: aString [ 
	^ self bootstrapEnvironment at: (self systemDefinition classWithRole: aString) name ifAbsent: [ nil ]
]

{ #category : #accessing }
PBImageBuilderGeneric >> classLoader [
	^ classLoader
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
PBImageBuilderGeneric >> createNil [

	| nilObject |
	
	nilObject := self classLoader 
		createStubInstanceOf: #UndefinedObject 
		classIndex: self classLoader newClassIndex.
	
	objectSpace nilObject: nilObject.
	^ objectSpace nilObject
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> createStubForClass: aString [ 
	
	^ self classLoader createStubForClass: (self systemDefinitionAt:aString)
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
	self halt.
	self createNil.
	self createFalse.
	self createTrue.
	
	self test01FirstObjectsStubs.
	
	self allocateFreeLists.
	self allocateClassTable.
	
	specialObjectsArray := self classLoader createStubInstanceOfArrayOfSize: 60.
	objectSpace specialObjectsArray: specialObjectsArray.
	objectSpace nilObject: objectSpace nilObject.
	objectSpace falseObject: objectSpace falseObject.
	objectSpace trueObject: objectSpace trueObject.
	self classLoader resetClassIndex.
	objectSpace nilObject
		setClass: (self createStubForClass: #UndefinedObject).
	objectSpace falseObject
		setClass: (self createStubForClassNamed: #PCFalse).
	objectSpace trueObject
		setClass: (self createStubForClassNamed: #PCTrue).
	objectSpace backend
		smallIntegerClass: (self createStubForClassNamed: #PCSmallInteger).
	objectSpace backend
		characterClass: (self createStubForClassNamed: #PCCharacter).
	objectSpace backend
		byteSymbolClass: (self createStubForClassNamed: #PCSymbol).
	objectSpace backend
		byteStringClass: (self createStubForClassNamed: #PCString).
	objectSpace backend
		byteArrayClass: (self createStubForClassNamed: #PCByteArray).
	objectSpace backend
		associationClass: (self createStubForClassNamed: #PCAssociation).
	objectSpace backend
		arrayClass: (self createStubForClassNamed: #PCArray).
	objectSpace backend
		symbolTableClass: objectSpace backend byteSymbolClass.
	objectSpace backend
		largeNegativeIntegerClass: (self createStubForClassNamed: #PCLargeNegativeInteger).
	objectSpace backend
		largePositiveIntegerClass: (self createStubForClassNamed: #PCLargePositiveInteger).
	objectSpace backend
		methodClass: (self createStubForClassNamed: #PCCompiledMethod).
	objectSpace backend
		floatClass: (self createStubForClassNamed: #PCFloat).
	objectSpace backend
		contextClass: (self createStubForClassNamed: #PCMethodContext).
	objectSpace backend
		processClass: (self createStubForClassNamed: #PCProcess).
	objectSpace backend
		blockClass: (self createStubForClassNamed: #PCBlock).
	"objectSpace backend fullBlockClass: (self createStubForClassNamed: #PCFullBlockClosure)."
	objectSpace backend
		messageClass: (self createStubForClassNamed: #PCMessage).

	"Point is required in the special objects array because it is used to instantiate point objects faster.
	If we do not put it here, the VM will crash.
	Lots of tests in kernel use it."
	objectSpace backend
		pointClass: (self createStubForClassNamed: #PCPoint).
	objectSpace backend
		processorAssociation: (self classNamed: #PCAssociation) mirror basicNew.
	objectSpace backend processorAssociation
		instanceVariableAtIndex: 2
		put: (self createStubForClassNamed: #PCProcessorScheduler) basicNew.
	objectSpace backend
		specialSelectors: ((self classNamed: #PCArray) mirror basicNew: 64)
]

{ #category : #accessing }
PBImageBuilderGeneric >> espellBackend [
	^ espellBackend 
]

{ #category : #'bootstrap-utils-classes' }
PBImageBuilderGeneric >> formatOf: aClassRole [ 
	^ (self systemDefinitionAt: aClassRole) format
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> initializeBootstrapEnvironment [
	
	espellBackend := EPSimulatorBackend for32Bit forBootstrap.
	espellBackend instanceVariableMapping: self systemDefinition instanceVariableMapping.
	
	bootstrapEnvironment := Dictionary new.
	
	"We initialize the object space with a boot symbol table and character table.
	We will later replace them by real ones."
	objectSpace := EPObjectSpace new.
	objectSpace backend: espellBackend.
	objectSpace characterTable: (EPExternalCharacterTable new objectSpace: objectSpace; yourself).
	objectSpace symbolTable: (EPExternalSymbolTable new objectSpace: objectSpace; yourself).
	

	classLoader := PBClassLoaderGenericReflective new.
	classLoader bootstrapEnvironment: bootstrapEnvironment.
	classLoader builder: self.
	
	bootstrapInterpreter := EPASTEvaluator new.
	bootstrapInterpreter codeProvider: self.
	bootstrapInterpreter environment: self.
	bootstrapInterpreter objectSpace: objectSpace.
	 
	debuggerInterpreter := DASTEvaluator new.
	debuggerInterpreter codeProvider: self.
	debuggerInterpreter environment: self.
	debuggerInterpreter objectSpace: objectSpace.
	
	objectSpace interpreter: bootstrapInterpreter.
	objectSpace debuggerInterpreter: debuggerInterpreter
]

{ #category : #accessing }
PBImageBuilderGeneric >> languageDefinition [
	^ languageDefinition
]

{ #category : #accessing }
PBImageBuilderGeneric >> objectMemory [
	
	^ espellBackend simulator objectMemory
]

{ #category : #accessing }
PBImageBuilderGeneric >> objectSpace [
	^ objectSpace
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
	^ self systemDefinition classWithRole: aString
]

{ #category : #tests }
PBImageBuilderGeneric >> test01FirstObjectsStubs [
	
	self assert: (self objectSpace backend simulator objectMemory formatOf: self objectSpace nilObject target address) equals: (self formatOf: #UndefinedObject).
	self assert: (self objectSpace backend simulator objectMemory formatOf: self objectSpace falseObject target address) equals: (self formatOf: #False).
	self assert: (self objectSpace backend simulator objectMemory formatOf: self objectSpace trueObject target address) equals: (self formatOf: #True).
]
