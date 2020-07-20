Class {
	#name : #PBClassLoaderGeneric,
	#superclass : #Object,
	#instVars : [
		'builder',
		'fullyLoaded',
		'addressDefinitionMap'
	],
	#classVars : [
		'nextClassIndex'
	],
	#category : #'PharoBootstrapGeneric-builders'
}

{ #category : #'accessing-mapping-mirrors/definitions' }
PBClassLoaderGeneric >> addressDefinitionMap [
	^ addressDefinitionMap
]

{ #category : #accessing }
PBClassLoaderGeneric >> backend [ 
	^ self builder objectSpace backend
]

{ #category : #accessing }
PBClassLoaderGeneric >> bootstrapEnvironment [
	^ self builder bootstrapEnvironment
]

{ #category : #accessing }
PBClassLoaderGeneric >> bootstrapEnvironment: aDictionary [ 
	self shouldNotImplement 
]

{ #category : #'accessing-classes-installed' }
PBClassLoaderGeneric >> bootstrapEnvironmentAt: aString [ 
	^ self builder bootstrapEnvironmentAt: aString
]

{ #category : #accessing }
PBClassLoaderGeneric >> builder [
	^ builder
]

{ #category : #accessing }
PBClassLoaderGeneric >> builder: aPBImageBuilderGeneric [ 
	builder := aPBImageBuilderGeneric
]

{ #category : #'accessing-mapping-mirrors/definitions' }
PBClassLoaderGeneric >> classForClassMirror: anEPClassMirror [ 
	self assert: anEPClassMirror isRemoteClass.
	^ self addressDefinitionMap at: anEPClassMirror target address 
		ifAbsent: [ self systemDefinitionAt: #Class ]
]

{ #category : #'accessing-classes-installed' }
PBClassLoaderGeneric >> classNamed: aString [ 
	
	^ ((self bootstrapEnvironmentAt: aString) 
		ifNil: [ self loadClassNamed: aString ]) asRemoteClass
]

{ #category : #'class-creation' }
PBClassLoaderGeneric >> createBehaviorFromDefinition: aClassDefinition [

	| newBehavior |
	(self isFullyLoadedClassNamed: aClassDefinition name) ifTrue: [ ^ self ].
	aClassDefinition isBehavior ifTrue: [
		"We ensure a class with that name already exists"
		self bootstrapEnvironment at: aClassDefinition name ifAbsent: [ self createStubForClassNamed: aClassDefinition name ]].
	
	newBehavior := (self systemDefinition hookInstallClass: aClassDefinition) evaluate asRemoteClass.
	
	aClassDefinition isBehavior ifTrue: [ 
		self registerClass: newBehavior asRemoteClass named: aClassDefinition name fullyLoaded: true.
	] ifFalse: [
		self registerTrait: newBehavior asRemoteTrait named: aClassDefinition name fullyLoaded: true.
	].
	
	(self systemDefinition hookInitializeMethodDictionaryFor: newBehavior) evaluate.
	"we register the class by hand in the Smalltalk globals"
	(self systemDefinition instructionRegisterInstalledBehaviorInGlobals: newBehavior) evaluate
]

{ #category : #'stubs-class-creation' }
PBClassLoaderGeneric >> createClassStubForDefinition: aRGBehavior [
	
	| class metaclass |	
	metaclass := self metaclassClass basicNew asRemoteClass.
	metaclass format: aRGBehavior classSide format.
	
	class := metaclass basicNew asRemoteClass.
	class format: aRGBehavior format.
	^ class
]

{ #category : #'stubs-class-creation' }
PBClassLoaderGeneric >> createStubForClass: aRGBehavior [ 

	| stub |
	^ aRGBehavior isBehavior
		ifTrue: [
			stub := self createClassStubForDefinition: aRGBehavior.
			self registerClass: stub named: aRGBehavior name.
			
			stub ]
		ifFalse: [ self error: 'Impossible to create class stub! The argument is not a behavior' ]
]

{ #category : #'stubs-class-creation' }
PBClassLoaderGeneric >> createStubForClassNamed: aString [ 

	^ self createStubForClass: (self systemDefinitionAt: aString)
]

{ #category : #'stubs-instance-creation' }
PBClassLoaderGeneric >> createStubForClassNamed: aString metaclassIndex: anInteger newClassIndex: anotherInteger [
	" We install by hand (non reflectively) a stub that represents a class using as template its definition in the ring model (language definition) " 
	| stub classDef metaclassIndex newClassIndex |
	classDef := self systemDefinitionAt: aString.
	metaclassIndex := anInteger.
	newClassIndex := anotherInteger.
	" First we create a zero sized stub object that represents the class.
	We do so by creating an instance of its metaclass "
	stub := (self createStubInstanceOf: classDef metaclass name classIndex: metaclassIndex) asRemoteClass.
	
	" Then we set its format (this is saved in the third instance variable of classes)"
	stub format: classDef format.
	
	" Finally we set its identity hash. A class identityHash is its index "
	stub setHashBitsTo: newClassIndex.
	self registerClassInClassTable: stub atIndex: newClassIndex.
	
	^ stub
]

{ #category : #'stubs-instance-creation' }
PBClassLoaderGeneric >> createStubInstanceOf: aClassName classIndex: anInteger [
	" A basic stub is an object of size 0 and its class index saved in its header "
	| format stub classIndex |
	format := (self systemDefinitionAt: aClassName) format.
	classIndex := anInteger.
	stub := (self backend
			createObjectWithFormat: format 
			withSize: 0 
			classIndex: classIndex).
	self assert: stub classIndex = classIndex.
	^ stub
]

{ #category : #'stubs-instance-creation' }
PBClassLoaderGeneric >> createStubInstanceOfArrayOfSize: anInteger [ 
	^ self backend
			createObjectWithFormat: (self systemDefinitionAt: #Array) format 
			withSize: 60
			classIndex: self builder arrayClassIndex.
]

{ #category : #'interpret-code' }
PBClassLoaderGeneric >> evaluate: aString temps: aCollectionOfAssociations [
	^ self builder bootstrapInterpreter 
			evaluateCode:  aString 
			withTemps: aCollectionOfAssociations 
]

{ #category : #testing }
PBClassLoaderGeneric >> fullyLoaded [
	^ fullyLoaded
]

{ #category : #initialization }
PBClassLoaderGeneric >> initialize [
	super initialize.
	addressDefinitionMap := Dictionary new.
	fullyLoaded := Dictionary new
]

{ #category : #'initializing-classes' }
PBClassLoaderGeneric >> initializeClassPool: aClassName [

	| definition class classPool classVariableBindings |
	definition := self systemDefinitionAt: aClassName.
	classVariableBindings := definition classVarNames collect: [ :each | | classVar |
		classVar := (self builder remoteClass: #ClassVariable) basicNew asRemoteAssociation.
		classVar key: each ].
	class := self bootstrapEnvironmentAt: aClassName.
	classPool := self localArrayAsRemoteDictionary: classVariableBindings.
	
	"Force the creation of the symbol using the object space class table." 
	self flag: #bug. "If we do not use name:, we duplicate symbols"
	class name: aClassName.
	class classPool: classPool.
	^ class
]

{ #category : #'initializing-classes' }
PBClassLoaderGeneric >> initializeClassPools [

	"shouldInitializeClassPool := true."
	self bootstrapEnvironment keysDo: [ :name | | definition |
			definition := self systemDefinition at: name.
			(definition isNotNil and: [definition isBehavior and: [definition isMetaclass not]]) 
				ifTrue: [ self initializeClassPool: definition name ]	
		 ]
]

{ #category : #accessing }
PBClassLoaderGeneric >> instanceVariableMapping [
	^ builder languageDefinition instanceVariableMapping
]

{ #category : #testing }
PBClassLoaderGeneric >> isClassDefinition: aString [ 
	^ self systemDefinition isClassDefinition: aString 
]

{ #category : #testing }
PBClassLoaderGeneric >> isFullyLoadedClassNamed: aClassName [

	^ fullyLoaded at: aClassName ifAbsent: [ false ]
]

{ #category : #'class-creation' }
PBClassLoaderGeneric >> loadClassNamed: aString [
	| class |
	class := self createStubForClassNamed: aString.
	"shouldInitializeClassPool
		ifTrue: [ self initializeClassPool: aString ]."
	^ class
]

{ #category : #'instance-creation' }
PBClassLoaderGeneric >> localArrayAsRemoteDictionary: aCollection [ 
	^ builder bootstrapInterpreter
		evaluateCode: 'aClassVariableNamesCollection asDictionary'
		withTemps: { 
			'aClassVariableNamesCollection' -> aCollection
		}.
]

{ #category : #'accessing-mapping-mirrors/definitions' }
PBClassLoaderGeneric >> map: aClassMirror with: aDefinition [

	"We create a mapping between the real classes and their definitions for later"
	
	| classSideMirror |
	self assert: (aClassMirror target isKindOf: EP2Handle).
	(aClassMirror backend simulator objectMemory classTableRootObj) ifNotNil: [
		self assert: aClassMirror target asClassIndexHandle address notNil.
		aClassMirror target: (aClassMirror target asClassIndexHandle) ].

	addressDefinitionMap at: aClassMirror target address put: aDefinition.
	aDefinition propertyNamed: #address put: aClassMirror target address.

	classSideMirror := aClassMirror basicClass.
	"only valid for Spur simulator bootstrap by now"
	(aClassMirror backend simulator objectMemory classTableRootObj) ifNotNil: [
		self assert: classSideMirror target asClassIndexHandle address notNil.
		classSideMirror target: (classSideMirror target asClassIndexHandle) ].
	
	addressDefinitionMap at: classSideMirror target address put: aDefinition classSide.
	aDefinition classSide propertyNamed: #address put: classSideMirror target address
]

{ #category : #'stubs-class-creation' }
PBClassLoaderGeneric >> metaclassClass [
	
	"Base case for recursion on class creation.
	Ok, so here we really go down and create the metaclass from nothing. We will use this metaclass to create all the others"
	| aRGBehavior theNewMetaclass theNewMetaclassClass metaclassIndex classIndex |
	" If the metaclass was already created, it will be found in the bootstrapEnvironment dictionary "
	(self bootstrapEnvironmentAt: #Metaclass) ifNotNil: [ :aClass | ^ aClass ].
	
	" We need to create the metaclass and its respective metaclass (the metaclass class) "
	aRGBehavior := self systemDefinitionAt: #Metaclass.
	
	metaclassIndex := self newClassIndex.
	classIndex := self newClassIndex.
	
	"This is Metaclass"
	theNewMetaclass := self createStubForClassNamed: aRGBehavior name metaclassIndex: metaclassIndex newClassIndex: classIndex.
	
	"This is Metaclass class"
	theNewMetaclassClass := self createStubForClassNamed: aRGBehavior metaclass name metaclassIndex: classIndex newClassIndex: metaclassIndex.
	
	self registerClass: theNewMetaclass named: aRGBehavior name.
	^ theNewMetaclass
]

{ #category : #'class-registration' }
PBClassLoaderGeneric >> migrateOldClass: oldClass to: newClass [

	| oldClassMetaclass oldClassAddress oldMetaclassAddress |
	
	oldClassAddress := oldClass target address.
	oldMetaclassAddress := oldClass basicClass target address.		
	
	"We migrate the class pools"
	oldClass asRemoteClass classPool isNilObject ifFalse: [
		newClass asRemoteClass classPool: oldClass asRemoteClass classPool ].
	
	"We migrate the class instance variables"
	"A class has at least 14 instances variables (cf class description, behavior) (this is for full Pharo). Starting from 15, we get user defined class instances variables."
	((builder systemDefinition instanceVariableMapping classNumberOfVariables) + 1) to: oldClass fixedSize do: [ :index |
		newClass instanceVariableAtIndex: index put: (oldClass instanceVariableAtIndex: index).
	].

	"We become the old class and metaclass into the new ones"
	oldClassMetaclass := oldClass basicClass.
	oldClass becomeInto: newClass.
	oldClassMetaclass target == Metaclass
		ifFalse:[ oldClassMetaclass becomeInto: newClass basicClass ].
		
	addressDefinitionMap removeKey: oldClassAddress.
	addressDefinitionMap removeKey: oldMetaclassAddress.
			
]

{ #category : #'stubs-class-creation' }
PBClassLoaderGeneric >> newClassIndex [
	"The first page of the class table contains special classes, so we start in the second page"
	"This index is shared among all subclasses, do not override! "
	nextClassIndex ifNil: [ nextClassIndex := self builder objectMemory classTablePageSize ].
	^ nextClassIndex := nextClassIndex + 1
]

{ #category : #'class-registration' }
PBClassLoaderGeneric >> registerClass: aClassMirror named: aString [ 
	self registerClass: aClassMirror named: aString fullyLoaded: false
]

{ #category : #'class-registration' }
PBClassLoaderGeneric >> registerClass: aClassMirror named: className fullyLoaded: aBoolean [

	| definition metaclassName |
	definition := self systemDefinition classNamed: className.
	metaclassName := definition classSide name.
	
	"If it existed before, we migrate its state"
	((aClassMirror isStub not) and: (self bootstrapEnvironment includesKey: className))
		ifTrue: [ 			
			self migrateOldClass: (self bootstrapEnvironment at: className) to: aClassMirror.
			 ]. 

	self bootstrapEnvironment at: className put: aClassMirror.
	self bootstrapEnvironment at: metaclassName put: aClassMirror basicClass.

	"We register it and keep up to date our compact class index"	
	aClassMirror compactClassIndex = 0 ifFalse: [ 
		builder objectSpace backend compactClassAt: aClassMirror asClassMirror compactClassIndex put: aClassMirror ].
	
	"We create a mapping between the real classes and their definitions for later"
	self map: aClassMirror with: definition.
	
	"We say the simulator that this class is a root, possible becomes can come into it. THIS IS ONLY VALID FOR SPUR SIMULATOR BOOTSTRAP. REFACTOR ME".
	self flag: #refactorMe.
	builder objectSpace backend addExtraRoot: aClassMirror.
	builder objectSpace backend addExtraRoot: aClassMirror classSide.
	
	"We reset the class map of the ast interpreter"
	self fullyLoaded at: className put: aBoolean.
	aBoolean ifTrue: [ 
		"we register the installed class in the remote system dictionary only if it's the fully loaded version"
		self builder objectSpace systemDictionary at: definition name put: aClassMirror.
		self builder objectSpace systemDictionary at: definition metaclass name put: aClassMirror basicClass.
		 ]
]

{ #category : #'class-registration' }
PBClassLoaderGeneric >> registerClassInClassTable: aClassMirror atIndex: anInteger [ 
	self builder objectSpace backend objectMemory classAtIndex: anInteger put: aClassMirror target address.	
	
]

{ #category : #'stubs-class-creation' }
PBClassLoaderGeneric >> resetClassIndex [
	nextClassIndex := nil
]

{ #category : #accessing }
PBClassLoaderGeneric >> systemDefinition [
	^ self builder systemDefinition
]

{ #category : #'accessing-classes-definitions' }
PBClassLoaderGeneric >> systemDefinitionAt: aString [ 
	^ self builder systemDefinitionAt: aString 
]

{ #category : #'accessing-classes-definitions' }
PBClassLoaderGeneric >> systemDefinitionAt: aString ifAbsent: aBlockClosure [ 
	^ self builder systemDefinitionAt: aString ifAbsent: [ aBlockClosure value ]
]
