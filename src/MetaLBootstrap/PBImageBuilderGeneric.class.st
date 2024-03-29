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
		'classLoader',
		'imageFileReference',
		'stage'
	],
	#category : #'MetaLBootstrap-builders'
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

{ #category : #'temporal for remove' }
PBImageBuilderGeneric >> at: aString ifAbsent: aBlockClosure [ 
	"returns nil because this method is used to get CompiledMethod for generating Compiled Method instances durin the bootstrap"
	^ aBlockClosure value
]

{ #category : #'accessing backend' }
PBImageBuilderGeneric >> backend [
	^ objectSpace backend
]

{ #category : #bindings }
PBImageBuilderGeneric >> bindingOf: aName [
	^ self bootstrapEnvironment bindingOf: aName 
]

{ #category : #accessing }
PBImageBuilderGeneric >> bootstrapEnvironment [
	^ bootstrapEnvironment
]

{ #category : #'accessing-environment' }
PBImageBuilderGeneric >> bootstrapEnvironmentAt: aString [
	| classDefinition |
	self deprecated: 'Use EP2Environment instead'.
	classDefinition := [ self systemDefinition classByRoleOrName: aString ]
		on: PBClassDefinitionNotFound
		do: [
			"global is not the name of a class" 
			^ self bootstrapEnvironment at: aString ifAbsent: [ nil ] ].
	"global is a class name"
	^ self bootstrapEnvironment
		at: classDefinition name
		ifAbsent: [ nil ]
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

{ #category : #'accessing model' }
PBImageBuilderGeneric >> classWithRole: aString [ 
	^ self systemDefinition classWithRole: aString 
]

{ #category : #'initializing image' }
PBImageBuilderGeneric >> compactImage [
	| firstFree lastUsed newHeap |
	newHeap := espellBackend objectMemory.
	newHeap allHeapEntitiesDo:
		[:o|
		(newHeap isFreeObject: o)
			ifTrue: [firstFree ifNil: [firstFree := o]]
			ifFalse: [lastUsed := o]].
	lastUsed < firstFree ifTrue: "nothing to do"
		[^self].
]

{ #category : #'bootstrap2-classes' }
PBImageBuilderGeneric >> createBehaviorFromDefinition: aRGBehavior [ 
	^ self classLoader createBehaviorFromDefinition: aRGBehavior
]

{ #category : #'bootstrap2-classes' }
PBImageBuilderGeneric >> createClasses [
	
	stage := #installingClasses.
	self systemDefinition installClasses
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> createFalse [

	| falseObject |
	
	falseObject := self classLoader 
		createStubInstanceOf: (self classWithRole: #False) 
		classIndex: self classLoader newClassIndex.
	
	objectSpace falseObject: falseObject.
	^ objectSpace falseObject
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> createFirstObjectsStubs [
	| specialObjectsArray |
	"the first objects are created by hand, we don't use the language definition"
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
	self classLoader resetClassIndex
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> createInitialObjects [
	| remoteSystemDictionary |
	stage := #installingInitialObjects.
	" Initialize globals in the bootstrap environment "
	" create a temporary system dictionary to allow reflective instructions execution when creating the real system dictionary"
	
	remoteSystemDictionary := (EP2RemoteSystemDictionary
		newWithBackend: espellBackend).
	"set the objectSpace reference to the temporary systemDictionary"
	self objectSpace systemDictionary
		withSystemDictionary: remoteSystemDictionary.
	self languageDefinition globalVariablesSorted
		do: [ :glob | 
			glob installStub.
			glob setValue: glob initialValue ].

	" create the definitive remote systemDictionary by evaluating a hook. If the hook returns nil, keep the stub system dictionary "
	remoteSystemDictionary := (self systemDefinition
		createRemoteSystemDictionary ifNil: [ remoteSystemDictionary ])
		asRemoteSystemDictionary.

	"install globals with their definitive value"
	self systemDefinition globalVariables
		do: [ :glob | 
			glob remoteIsStub: false.
			glob register ].

	self systemDefinition createInitialObjects.
	
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> createNil [

	| nilObject |
	
	nilObject := self classLoader 
		createStubInstanceOf: (self classWithRole: #UndefinedObject) 
		classIndex: self classLoader newClassIndex.
	
	objectSpace nilObject: nilObject .
	^ objectSpace nilObject
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> createStubInstanceOfArrayOfSize: anInteger [ 
	^ self classLoader createStubInstanceOfArrayOfSize: anInteger 
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> createTrue [

	| trueObject |
	
	trueObject := self classLoader 
		createStubInstanceOf: (self classWithRole: #True) 
		classIndex: self classLoader newClassIndex.
	
	objectSpace trueObject: trueObject.
	^ objectSpace trueObject
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> createVMStubs [
	stage := #installingStubs.
	self createFirstObjectsStubs.
	self classLoader installStubs.
	self systemDefinition testStub.
	self setFirstObjectsStubClasses.
	self systemDefinition testStub.
	self fillSpecialObjectsArrayWithStubs.
	self systemDefinition testStub.
	
]

{ #category : #debugging }
PBImageBuilderGeneric >> debug: aString withTemps: aCollection [ 
	| temps |
	temps := aCollection collect: [ :assoc | assoc key -> (assoc value asRemoteObjectInObjectSpace: self objectSpace) ].
	(PBHybridDebugger 
		withCode: aString 
		temps: temps 
		interpreter: self debuggerInterpreter interpreter) openWithSpec.
]

{ #category : #accessing }
PBImageBuilderGeneric >> debuggerInterpreter [
	^ debuggerInterpreter
]

{ #category : #initializing }
PBImageBuilderGeneric >> destroy [
	bootstrapEnvironment := nil.
	
	bootstrapInterpreter
		ifNotNil: [ 
			bootstrapInterpreter destroy.
			bootstrapInterpreter := nil ].
	classLoader 
		ifNotNil: [ 
			classLoader destroy.
			classLoader := nil ].
	debuggerInterpreter
		ifNotNil: [ 
			debuggerInterpreter destroy.
			debuggerInterpreter := nil ].
	espellBackend := nil.
	
	languageDefinition := nil.
	objectSpace
		ifNotNil: [ 
			objectSpace destroy.
			objectSpace := nil ].
	
]

{ #category : #'accessing error messages' }
PBImageBuilderGeneric >> errorMessagesArray [

	"return objects for error messages - special objects array at index 52"
	
 	^ {nil "nil => generic error". 
		#'bad receiver'. #'bad argument'. #'bad index'.
		#'bad number of arguments'.
		#'inappropriate operation'.  #'unsupported operation'.
		#'no modification'. #'insufficient object memory'.
		#'insufficient C memory'. #'not found'. #'bad method'.
		#'internal error in named primitive machinery'.
		#'object may move'. #'resource limit exceeded'.
		#'object is pinned'. #'primitive write beyond end of object'.
		#'object moved'. #'object not pinned'. #'callback error'} asLiteralInObjectSpace: objectSpace
]

{ #category : #accessing }
PBImageBuilderGeneric >> espellBackend [
	^ espellBackend 
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> fillSpecialObjectsArrayWithStubs [
	
	objectSpace backend
		smallIntegerClass: (self remoteClass: #SmallInteger).
	objectSpace backend
		characterClass: (self remoteClass: #Character).
	objectSpace backend
		byteSymbolClass: (self remoteClass: #ByteSymbol).
	objectSpace backend
		byteStringClass: (self remoteClass: #ByteString).
	objectSpace backend
		byteArrayClass: (self remoteClass: #ByteArray).
	objectSpace backend
		associationClass: (self remoteClass: #Association).
	objectSpace backend
		arrayClass: (self remoteClass: #Array).
	"not sure why this is here so I comment it"
	"objectSpace backend
		symbolTableClass: objectSpace backend byteSymbolClass."
	objectSpace backend
		largeNegativeIntegerClass: (self remoteClass: #LargeNegativeInteger).
	objectSpace backend
		largePositiveIntegerClass: (self remoteClass: #LargePositiveInteger).
	objectSpace backend
		methodClass: (self remoteClass: #CompiledMethod).
	objectSpace backend
		floatClass: (self remoteClass: #Float).
	objectSpace backend
		contextClass: (self remoteClass: #Context).
	objectSpace backend
		processClass: (self remoteClass: #Process).
	objectSpace backend
		blockClass: (self remoteClass: #BlockClosure).
	objectSpace backend
		messageClass: (self remoteClass: #Message).

	"Point is required in the special objects array because it is used to instantiate point objects faster.
	If we do not put it here, the VM will crash.
	Lots of tests in kernel use it."
	objectSpace backend
		pointClass: (self remoteClass: #Point).
	objectSpace backend
		processorAssociation: (self remoteClass: #Association) basicNew.
	objectSpace backend processorAssociation
		value: (self remoteClass: #ProcessorScheduler) basicNew.
	objectSpace backend
		specialSelectors: ((self remoteClass: #Array) basicNew: 64).
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

{ #category : #'initializing image' }
PBImageBuilderGeneric >> followForwardingPointers [
	| newHeap |
	newHeap := espellBackend objectMemory.
	newHeap allObjectsDo: [:o|
		(newHeap isForwarded: o) ifFalse:
			[0 to: (newHeap numPointerSlotsOf: o) - 1 do:
				[:i| | field |
				field := newHeap fetchPointer: i ofObject: o.
				(newHeap isOopForwarded: field) ifTrue:
					[newHeap
						storePointer: i
						ofObject: o
						withValue: (newHeap followForwarded: field)]]]].
]

{ #category : #utils }
PBImageBuilderGeneric >> formatOf: aClassRole [ 
	^ (self classWithRole: aClassRole) format
]

{ #category : #'initializing image' }
PBImageBuilderGeneric >> freeForwarders [
	"Check that all forwarders have been followed.  Then free them."
	| numForwarders numFreed newHeap |
	numForwarders := numFreed := 0.
	newHeap := espellBackend objectMemory.
	newHeap allObjectsDo:
		[:o|
		(newHeap isForwarded: o)
			ifTrue: [numForwarders := numForwarders + 1]
			ifFalse:
				[0 to: (newHeap numPointerSlotsOf: o) - 1 do:
					[:i|
					self assert: (newHeap isOopForwarded: (newHeap fetchPointer: i ofObject: o)) not]]].
	Transcript cr;  nextPutAll: 'freeing '; print: numForwarders; nextPutAll: ' forwarders'; cr; flush.
	newHeap allObjectsDo:
		[:o|
		(newHeap isForwarded: o) ifTrue:
			[numFreed := numFreed + 1.
			 newHeap freeObject: o]].
	self assert: numFreed = numForwarders
]

{ #category : #initializing }
PBImageBuilderGeneric >> initializeBootstrapEnvironment [
	stage := #initializing.
	espellBackend := EP2Backend for32Bit forBootstrap.
	espellBackend systemDefinition: self systemDefinition.
	
	bootstrapEnvironment := EP2Environment new.
	bootstrapEnvironment builder: self.
	
	"We initialize the object space with a boot symbol table and character table.
	We will later replace them by real ones."
	objectSpace := EP2ObjectSpace new.
	objectSpace backend: espellBackend.
	objectSpace systemDictionary: (EP2SystemDictionary new objectSpace: objectSpace; yourself).
	objectSpace characterTable: (EP2ExternalCharacterTable new objectSpace: objectSpace; yourself).
	objectSpace symbolTable: (EP2ExternalSymbolTable new objectSpace: objectSpace; yourself).
	

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
	espellBackend objectSpace: objectSpace.
	
	 
]

{ #category : #'special objects array' }
PBImageBuilderGeneric >> initializeSpecialObjectArray [
	objectSpace specialObjectsArray: self newSpecialObjectsArray.
	self systemDefinition initializeSpecialObjectsArray
]

{ #category : #'stage checking' }
PBImageBuilderGeneric >> initializing [
	^ self stage = #initializing
]

{ #category : #'method installing' }
PBImageBuilderGeneric >> installExtensionMethods [
	self flag: 'do nothing for now. There is no extension methods in Candle'
]

{ #category : #'bootstrap3-methods' }
PBImageBuilderGeneric >> installMethods [
	stage := #installingMethods.
	self systemDefinition installMethods
	
	
]

{ #category : #'stage checking' }
PBImageBuilderGeneric >> installingClasses [
	^ self stage = #installingClasses
]

{ #category : #'stage checking' }
PBImageBuilderGeneric >> installingInitialObjects [
	^ self stage = #installingInitialObjects
]

{ #category : #'stage checking' }
PBImageBuilderGeneric >> installingMethods [
	^ self stage = #installingMethods
]

{ #category : #'stage checking' }
PBImageBuilderGeneric >> installingStubs [
	^ self stage = #installingStubs
]

{ #category : #accessing }
PBImageBuilderGeneric >> languageDefinition [
	^ languageDefinition
]

{ #category : #accessor }
PBImageBuilderGeneric >> methodBuilder [
	^ PBMethodCompilerGeneric new builder: self; yourself.	
]

{ #category : #'special objects array' }
PBImageBuilderGeneric >> newSpecialObjectsArray [
	| newSpecialObjectsArray |
	newSpecialObjectsArray := espellBackend arrayClass asClassMirror basicNew: 60.
	newSpecialObjectsArray at: 1 put: espellBackend nilObject.
	newSpecialObjectsArray at: 2 put: espellBackend falseObject.
	newSpecialObjectsArray at: 3 put: espellBackend trueObject.
	newSpecialObjectsArray
		at: 4
		put: espellBackend processorAssociation.
	newSpecialObjectsArray at: 5 put: espellBackend nilObject.
	newSpecialObjectsArray at: 6 put: espellBackend smallIntegerClass.
	newSpecialObjectsArray at: 7 put: espellBackend byteStringClass.
	newSpecialObjectsArray at: 8 put: espellBackend arrayClass.
	newSpecialObjectsArray at: 9 put: espellBackend smalltalkInstance.
	newSpecialObjectsArray at: 10 put: espellBackend floatClass.
	newSpecialObjectsArray at: 11 put: espellBackend contextClass.
	newSpecialObjectsArray at: 12 put: espellBackend nilObject.
	newSpecialObjectsArray at: 13 put: espellBackend pointClass.
	newSpecialObjectsArray at: 14 put: espellBackend largePositiveIntegerClass.
	newSpecialObjectsArray at: 15 put: espellBackend nilObject.
	newSpecialObjectsArray at: 16 put: espellBackend messageClass.
	newSpecialObjectsArray at: 17 put: espellBackend methodClass.
	newSpecialObjectsArray at: 18 put: espellBackend nilObject.
	newSpecialObjectsArray at: 19 put: espellBackend semaphoreClass.
	newSpecialObjectsArray at: 20 put: espellBackend characterClass.
	newSpecialObjectsArray at: 21 put: (#doesNotUnderstand: asLiteralInObjectSpace: objectSpace).
	newSpecialObjectsArray at: 22 put: (#cannotReturn: asLiteralInObjectSpace: objectSpace).
	newSpecialObjectsArray at: 23 put: espellBackend nilObject.
	newSpecialObjectsArray at: 24 put: (#(#+ 1 #- 1 #< 1 #> 1 #'<=' 1 #'>=' 1 #= 1 #'~=' 1 #* 1 #/ 1 #'\\' 1 #@ 1 #bitShift: 1 #'//' 1 #bitAnd: 1 #bitOr: 1 #at: 1 #at:put: 2 #size 0 #next 0 #nextPut: 1 #atEnd 0 #'==' 1 nil 0 #'~~' 1 #value 0 #value: 1 #do: 1 #new 0 #new: 1 #x 0 #y 0) asLiteralInObjectSpace: objectSpace).
	newSpecialObjectsArray
		at: 25
		put: espellBackend characterTable. "character table"
	newSpecialObjectsArray at: 26 put: (#mustBeBoolean asLiteralInObjectSpace: objectSpace).
	newSpecialObjectsArray at: 27 put: espellBackend byteArrayClass.
	newSpecialObjectsArray at: 28 put: espellBackend processClass.
	newSpecialObjectsArray at: 29 put: (espellBackend arrayClass basicNew: 0).
	30 to: 56 do: [ :idx | newSpecialObjectsArray at: idx put: espellBackend nilObject ].
	newSpecialObjectsArray at: 37 put: espellBackend blockClass.
	"External semaphore table"
	newSpecialObjectsArray at: 39 put: (espellBackend arrayClass asClassMirror basicNew: 20 ).
	newSpecialObjectsArray at: 43 put: espellBackend largeNegativeIntegerClass.
	newSpecialObjectsArray at: 49 put: (#aboutToReturn:through: asLiteralInObjectSpace: objectSpace).
	newSpecialObjectsArray at: 50 put: (#run:with:in: asLiteralInObjectSpace: objectSpace).
	self flag: #'add WeakFinalization list'.
	newSpecialObjectsArray at: 51 put: (#attemptToAssign:withIndex: asLiteralInObjectSpace: objectSpace).
	newSpecialObjectsArray at: 52 put: self errorMessagesArray.
	newSpecialObjectsArray at: 54 put: (#invokeCallbackContext: asLiteralInObjectSpace: objectSpace).
	newSpecialObjectsArray at: 58 put: (#unusedBytecode asLiteralInObjectSpace: objectSpace).
	newSpecialObjectsArray at: 59 put: (#conditionalBranchCounterTrippedOn: asLiteralInObjectSpace: objectSpace).
	newSpecialObjectsArray at: 60 put: (#classTrapFor: asLiteralInObjectSpace: objectSpace).

	^newSpecialObjectsArray.
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

{ #category : #registering }
PBImageBuilderGeneric >> registerGlobal: anEP2Remote definition: aGlobalModel fullyLoaded: aBoolean [ 
	| name value |
	name := aGlobalModel name.
	value := anEP2Remote.
	self objectSpace systemDictionary at: name put: value.
	self bootstrapEnvironment at: name put: value
]

{ #category : #'accessing-classes-installed' }
PBImageBuilderGeneric >> remoteClass: aString [ 

	^ (self classWithRole: aString) ensureRemote
]

{ #category : #'initializing image' }
PBImageBuilderGeneric >> reportSizes [
	| oldAvgBytes "newAvgBytes" newHeapSize newHeap |
	newHeapSize := 20 * 1024 * 1024.
	newHeap := espellBackend objectMemory.
	Transcript
		nextPutAll: 'done.'; cr;
		nextPutAll: 'initial heap size: '; nextPutAll: newHeapSize asStringWithCommas; cr;
"		nextPutAll: 'change: '; print: change * 100.0 maxDecimalPlaces: 2; nextPut: $%; cr;"
		flush.
	newHeapSize := newHeap endOfMemory
					- newHeap scavenger eden limit
					- newHeap totalFreeListBytes.
"	newAvgBytes := newHeapSize asFloat / newHeapNumObjs."
	Transcript
		nextPutAll: 'final new heap size: '; nextPutAll: newHeapSize asStringWithCommas; tab;
	"	nextPutAll: ' (avg obj bytes '; print: newAvgBytes maxDecimalPlaces: 2; nextPutAll: ' words '; print: newAvgBytes / self wordSize maxDecimalPlaces: 2; nextPut: $); cr;
""		nextPutAll: 'change: '; print: change * 100.0 maxDecimalPlaces: 2; nextPut: $%; cr;"
		flush
]

{ #category : #'bootstrap1-stubs' }
PBImageBuilderGeneric >> setFirstObjectsStubClasses [
	objectSpace nilObject
		setClass: (self remoteClass: #UndefinedObject).
	objectSpace falseObject
		setClass: (self remoteClass: #False).
	objectSpace trueObject
		setClass: (self remoteClass: #True).
	
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
PBImageBuilderGeneric >> stage [
	^ stage
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

{ #category : #'writing-image' }
PBImageBuilderGeneric >> writeImage [
	
	[[ espellBackend objectMemory flushNewSpace. ]
		on: Halt
		do: [:ex|
			(#(#fullGC #compactImage  #doScavenge:) includes: ex signalerContext sender selector)
				ifTrue: [ex resume]
				ifFalse: [ex pass]]] on: AssertionFailure do: [ :e |
				((e signalerContext sender method selector == #assert:)
					and: [ e signalerContext sender sender method selector = #mapStackPages ])
						ifTrue: [ e resume ]
						ifFalse: [ e pass ]].

	self freeForwarders.
	self compactImage.
	self reportSizes.

	self writeSnapshot: imageFileReference fullName ofTransformedImage: espellBackend simulator objectMemory headerFlags: 0 screenSize: 0.
	self inform: 'Image written in: ', imageFileReference fullName

]

{ #category : #'writing-image' }
PBImageBuilderGeneric >> writeImageIn: aFileReference [ 
	imageFileReference := aFileReference.	
	self writeImage
]

{ #category : #'writing image' }
PBImageBuilderGeneric >> writeSnapshot: imageFileName ofTransformedImage: spurHeap headerFlags: headerFlags screenSize: screenSizeInteger [
	"The bootstrapped image typically contains a few big free chunks and one huge free chunk.
	 Test snapshot writing and loading by turning the largest non-huge chunks into segment bridges
	 and saving."
	| penultimate ultimate sim |
"	[ spurHeap  flushNewSpace. ]
		on: Halt
		do: [:ex|"
			"suppress halts from the usual suspects (development time halts)"	
"			(#(#fullGC #compactImage  #doScavenge:) includes: ex signalerContext sender selector)
				ifTrue: [ex resume]
				ifFalse: [ex pass]]."
	sim := spurHeap coInterpreter.
	sim bootstrapping: true.
	spurHeap segmentManager prepareForSnapshot.
	spurHeap
		setEndOfMemory: spurHeap endOfMemory + spurHeap bridgeSize. "hack; initializeInterpreter: cuts it back by bridgeSize"
	sim initializeInterpreter: 0;
		setImageHeaderFlagsFrom: headerFlags;
		setSavedWindowSize: (800<<16)+600;
		setDisplayForm: nil.
	spurHeap allOldSpaceEntitiesDo: [:e| penultimate := ultimate. ultimate := e].
	"Check that we've left behind the old, pre-pigCompact segmented save"
	self assert: (spurHeap isFreeObject: penultimate) not.
	spurHeap checkFreeSpace.
	spurHeap runLeakCheckerForFullGC.
	sim bereaveAllMarriedContextsForSnapshotFlushingExternalPrimitivesIf: true.
	sim imageName: imageFileName.
	sim writeImageFileIO.
	Transcript cr; show: 'Done!'
]
