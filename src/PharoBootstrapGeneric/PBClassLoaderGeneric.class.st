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
	(self isFullyLoadedClassNamed: aClassDefinition name)
		ifTrue: [ ^ self ].
	
	aClassDefinition isBehavior
		ifTrue: [ "We ensure a class with that name already exists"
			self bootstrapEnvironment
				at: aClassDefinition name
				ifAbsent: [ 
					self createStubForClassNamed: aClassDefinition name] ].
	
	newBehavior := (self systemDefinition
		hookInstallClass: aClassDefinition) asRemoteClass.
	
	self systemDefinition
		hookRegisterClass: newBehavior
		fromDefinition: aClassDefinition
		withLoader: self.
	 "we register the installed class in the remote system dictionary only if it's the fully loaded version"
	(self systemDefinition
				hookRegisterClass: newBehavior 
				named: aClassDefinition name) evaluate. 
	(self systemDefinition hookInitializeMethodDictionaryFor: newBehavior)
		evaluate.
	"we register the class by hand in the Smalltalk globals"
	(self systemDefinition
		instructionRegisterInstalledBehaviorInGlobals: newBehavior) evaluate
]

{ #category : #'stubs-class-creation' }
PBClassLoaderGeneric >> createClassStubForDefinition: aRGBehavior [
	
	| class metaclass |	
	metaclass := self metaclassClass basicNew asRemoteMetaclass.
	metaclass format: aRGBehavior classSide format.
	self registerClass: metaclass definition: aRGBehavior metaclass fullyLoaded: false.
	
	class := metaclass basicNew asRemoteClass.
	class format: aRGBehavior format.
	self registerClass: class definition: aRGBehavior fullyLoaded: false.
	self assert: class basicClass = metaclass .
	
	^ class
]

{ #category : #'stubs-class-creation' }
PBClassLoaderGeneric >> createStubForClass: aRGBehavior [
	^ aRGBehavior isBehavior
		ifTrue: [ self createClassStubForDefinition: aRGBehavior ]
		ifFalse: [ self error: 'Error while creating the class stub!' ]
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
	
	classPool := (classVariableBindings asRemoteObjectInObjectSpace: builder objectSpace) asRemoteArray asDictionary.
	
	"Force the creation of the symbol using the object space class table." 
	self flag: #bug. "If we do not use name:, we duplicate symbols"
	class name: aClassName.
	class classPool: classPool.
	^ class
]

{ #category : #'initializing-classes' }
PBClassLoaderGeneric >> initializeClassPools [
	self deprecated: 'use methods in the metamodel behaviors'.
	"shouldInitializeClassPool := true."
	self bootstrapEnvironment keysDo: [ :name | | definition |
			definition := self systemDefinition classNamed: name.
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
	self deprecated: 'use EP2RemoteArray>>asDictionary instead'.
	^ builder bootstrapInterpreter
		evaluateCode: 'aClassVariableNamesCollection asDictionary'
		withTemps: { 
			'aClassVariableNamesCollection' -> aCollection
		}.
]

{ #category : #'accessing-mapping-mirrors/definitions' }
PBClassLoaderGeneric >> map: aClassMirror with: aDefinition [

	"We create a mapping between the real classes and their definitions for later"
	
	self assert: (aClassMirror target isKindOf: EP2Handle).
	(aClassMirror backend simulator objectMemory classTableRootObj) ifNotNil: [
		self assert: aClassMirror target asClassIndexHandle address notNil.
		aClassMirror target: (aClassMirror target asClassIndexHandle) ].

	addressDefinitionMap at: aClassMirror target address put: aDefinition.
	aDefinition propertyNamed: #address put: aClassMirror target address
]

{ #category : #'stubs-class-creation' }
PBClassLoaderGeneric >> metaclassClass [
	"Base case for recursion on class creation.
	Ok, so here we really go down and create the metaclass from nothing. We will use this metaclass to create all the others"

	| aRGBehavior  |
	" If the metaclass was already created, it will be found in the bootstrapEnvironment dictionary "
	(self bootstrapEnvironmentAt: #Metaclass)
		ifNotNil: [ :aClass | ^ aClass ].
	^ self systemDefinition hookCreateMetaclassStubWithClassLoader: self.
	
]

{ #category : #'class-registration' }
PBClassLoaderGeneric >> migrateOldClass: oldClass to: newClass [

	| oldClassAddress |
	
	oldClassAddress := oldClass target address.
	
	"We migrate the class pools"
	oldClass classPool isNilObject ifFalse: [
		newClass classPool: oldClass asRemoteClass classPool ].
	
	"We migrate the class instance variables"
	"A class has at least 14 instances variables (cf class description, behavior) (this is for full Pharo). Starting from 15, we get user defined class instances variables."
	((builder systemDefinition instanceVariableMapping classNumberOfVariables) + 1) to: oldClass fixedSize do: [ :index |
		newClass instanceVariableAtIndex: index put: (oldClass instanceVariableAtIndex: index).
	].

	"We become the old class into the new one"
	oldClass becomeInto: newClass.
		
	addressDefinitionMap removeKey: oldClassAddress		
]

{ #category : #'stubs-class-creation' }
PBClassLoaderGeneric >> newClassIndex [
	"The first page of the class table contains special classes, so we start in the second page"
	"This index is shared among all subclasses, do not override! "
	nextClassIndex ifNil: [ nextClassIndex := self builder objectMemory classTablePageSize ].
	^ nextClassIndex := nextClassIndex + 1
]

{ #category : #'class-registration' }
PBClassLoaderGeneric >> registerClass: aClassMirror definition: aRGObject fullyLoaded: aBoolean [
	^ self registerClass: aClassMirror named: aRGObject name fullyLoaded: aBoolean

]

{ #category : #'class-registration' }
PBClassLoaderGeneric >> registerClass: aClassMirror named: aString [ 
	self deprecated: 'use registerClass:definition:fullyLoaded:'.
	self registerClass: aClassMirror named: aString fullyLoaded: false
]

{ #category : #'class-registration' }
PBClassLoaderGeneric >> registerClass: aClassMirror named: className fullyLoaded: aBoolean [
	| definition |
	
	
	definition := self systemDefinition classNamed: className.
	
	"If it existed before, we migrate its state"
	(aClassMirror isStub not
		and: (self bootstrapEnvironment includesKey: className))
		ifTrue: [ self
				migrateOldClass: (self bootstrapEnvironment at: className)
				to: aClassMirror ].
	self bootstrapEnvironment at: className put: aClassMirror.

	"We register it and keep up to date our compact class index"
	aClassMirror compactClassIndex = 0
		ifFalse: [ builder objectSpace backend
				compactClassAt: aClassMirror asClassMirror compactClassIndex
				put: aClassMirror ].

	"We create a mapping between the real classes and their definitions for later"
	self map: aClassMirror with: definition.
	"self map: aClassMirror basicClass with: definition metaclass."

	"We say the simulator that this class is a root, possible becomes can come into it. THIS IS ONLY VALID FOR SPUR SIMULATOR BOOTSTRAP. REFACTOR ME"
	self flag: #refactorMe.
	builder objectSpace backend addExtraRoot: aClassMirror.

	"We reset the class map of the ast interpreter"
	self fullyLoaded at: className put: aBoolean.
	
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
