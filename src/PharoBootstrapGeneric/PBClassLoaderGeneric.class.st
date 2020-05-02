Class {
	#name : #PBClassLoaderGeneric,
	#superclass : #Object,
	#instVars : [
		'builder',
		'fullyLoaded',
		'mirrorDefinitionMap',
		'definitionMirrorMap'
	],
	#classVars : [
		'nextClassIndex'
	],
	#category : #'PharoBootstrapGeneric-builders'
}

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

{ #category : #'accessing-installedClasses' }
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

{ #category : #'mapping-mirrors-definitions' }
PBClassLoaderGeneric >> classForClassMirror: anEPClassMirror [ 
	
	^ self mirrorDefinitionMap at: anEPClassMirror ifAbsent: [ self systemDefinitionAt: #Class ]
]

{ #category : #'accessing-installedClasses' }
PBClassLoaderGeneric >> classNamed: aString [ 
	
	^ (self bootstrapEnvironmentAt: aString) 
		ifNil: [ self loadClassNamed: aString ]
]

{ #category : #'class-creation' }
PBClassLoaderGeneric >> createBehaviorFromDefinition: aClassDefinition [

	| newBehavior code |
	(self isFullyLoadedClassNamed: aClassDefinition name) ifTrue: [ ^ self ].
	aClassDefinition isBehavior ifTrue: [
		"We ensure a class with that name already exists"
		self bootstrapEnvironment at: aClassDefinition name ifAbsent: [ self createStubForClassNamed: aClassDefinition name ]].
	
	newBehavior := (self systemDefinition instructionInstallClass: aClassDefinition) evaluate.
	(self systemDefinition instructionInitializeMethodDictionaryFor: newBehavior) evaluate.
	
	aClassDefinition isBehavior ifTrue: [ 
		self registerClass: newBehavior asClassMirror named: aClassDefinition name fullyLoaded: true.
	] ifFalse: [
		self registerTrait: newBehavior asTraitMirror named: aClassDefinition name fullyLoaded: true.
	].
	"we register the class by hand in the Smalltalk globals"
	(self systemDefinition instructionRegisterInstalledBehaviorInGlobals: newBehavior) evaluate
]

{ #category : #'class-creation-stubs' }
PBClassLoaderGeneric >> createClassStubForDefinition: aRGBehavior [
	
	| class metaclass |	
	metaclass := self metaclassClass basicNew asClassMirror.
	metaclass format: aRGBehavior classSide format.
	
	class := metaclass basicNew asClassMirror.
	class format: aRGBehavior format.
	^ class
]

{ #category : #'class-creation-stubs' }
PBClassLoaderGeneric >> createStubForClass: aRGBehavior [ 

	| stub |
	^ aRGBehavior isBehavior
		ifTrue: [
			stub := self createClassStubForDefinition: aRGBehavior.
			self registerClass: stub named: aRGBehavior name.
			
			stub ]
		ifFalse: [ self error: 'Impossible to create class stub! The argument is not a behavior' ]
]

{ #category : #'class-creation-stubs' }
PBClassLoaderGeneric >> createStubForClassNamed: aString [ 

	^ self createStubForClass: (self systemDefinitionAt: aString)
]

{ #category : #'instance-creation-stubs' }
PBClassLoaderGeneric >> createStubForClassNamed: aString metaclassIndex: anInteger newClassIndex: anotherInteger [
	" We install by hand (non reflectively) a stub that represents a class using as template its definition in the ring model (language definition) " 
	| stub classDef metaclassIndex newClassIndex |
	classDef := self systemDefinitionAt: aString.
	metaclassIndex := anInteger.
	newClassIndex := anotherInteger.
	" First we create a zero sized stub object that represents the class.
	We do so by creating an instance of its metaclass "
	stub := (self createStubInstanceOf: classDef metaclass name classIndex: metaclassIndex) asClassMirror.
	" Then we set its format (this is saved in the third instance variable of classes)"
	stub format: classDef format.
	" Finally we set its identity hash. A class identityHash is its index "
	stub setHashBitsTo: newClassIndex.
	self registerClassInClassTable: stub atIndex: newClassIndex.
	
	^ stub
]

{ #category : #'instance-creation-stubs' }
PBClassLoaderGeneric >> createStubInstanceOf: aClassName classIndex: anInteger [
	" A basic stub is an object of size 0 and its class index saved in its header "
	| format stub classIndex |
	format := (self systemDefinition classWithRole: aClassName) format.
	classIndex := anInteger.
	stub := (self backend
			createObjectWithFormat: format 
			withSize: 0 
			classIndex: classIndex).
	self assert: stub classIndex = classIndex.
	^ stub
]

{ #category : #'instance-creation-stubs' }
PBClassLoaderGeneric >> createStubInstanceOfArrayOfSize: anInteger [ 
	^ self backend
			createObjectWithFormat: (self systemDefinitionAt: #Array) format 
			withSize: 60
			classIndex: self builder arrayClassIndex.
]

{ #category : #testing }
PBClassLoaderGeneric >> fullyLoaded [
	^ fullyLoaded
]

{ #category : #initialization }
PBClassLoaderGeneric >> initialize [
	super initialize.
	mirrorDefinitionMap := EPDictionary new.
	definitionMirrorMap := EPDictionary new.
	fullyLoaded := Dictionary new
]

{ #category : #'initializing-classes' }
PBClassLoaderGeneric >> initializeClassPool: aClassName [

	| definition class classPool classVariableBindings |
	definition := self systemDefinition classNamed: aClassName.
	classVariableBindings := definition classVarNames collect: [ :each | | classVar |
		classVar := (self classNamed: #ClassVariable) basicNew.
		classVar instanceVariableAtIndex: (self instanceVariableMapping classVariableKeyIndex) put: (each asLiteralInObjectSpace: builder objectSpace) ].
	
	class := self bootstrapEnvironmentAt: aClassName.
	classPool := builder bootstrapInterpreter
		evaluateCode: 'aClassVariableNamesCollection asDictionary'
		withTemps: { 
			'aClassVariableNamesCollection' -> (builder objectSpace newArrayWithAll: classVariableBindings)
		}.
	
	"Force the creation of the symbol using the object space class table." 
	self flag: #bug. "If we do not use basicSetClassName, we duplicate symbols"
	class basicSetClassName: (aClassName asLiteralInObjectSpace: builder objectSpace).
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

{ #category : #'class-registration' }
PBClassLoaderGeneric >> map: aClassMirror with: aDefinition [

	"We create a mapping between the real classes and their definitions for later"
	
	| classSideMirror |
	self assert: (aClassMirror target isKindOf: EPSimulatorHandle).
	(aClassMirror backend simulator objectMemory classTableRootObj) ifNotNil: [
		self assert: aClassMirror target asClassIndexHandle address notNil.
		aClassMirror target: (aClassMirror target asClassIndexHandle) ].

	mirrorDefinitionMap at: aClassMirror put: aDefinition.
	definitionMirrorMap at: aDefinition put: aClassMirror.

	classSideMirror := aClassMirror basicClass.
	"only valid for Spur simulator bootstrap by now"
	(aClassMirror backend simulator objectMemory classTableRootObj) ifNotNil: [
		self assert: classSideMirror target asClassIndexHandle address notNil.
		classSideMirror target: (classSideMirror target asClassIndexHandle) ].
	
	mirrorDefinitionMap at: classSideMirror put: aDefinition classSide.
	definitionMirrorMap at: aDefinition classSide put: classSideMirror.
]

{ #category : #'class-creation-stubs' }
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

{ #category : #migrating }
PBClassLoaderGeneric >> migrateOldClass: oldClass to: newClass [

	| oldClassMetaclass |
	
	"We migrate the class pools"
	oldClass asClassMirror classPool isNilObject ifFalse: [
		newClass asClassMirror classPool: oldClass asClassMirror classPool ].
	
	"We migrate the class instance variables"
	"A class has at least 14 instances variables (cf class description, behavior). Starting from 15, we get user defined class instances variables."
	((newClass instanceVariableMapping classNumberOfVariables) + 1) to: oldClass fixedSize do: [ :index |
		newClass instanceVariableAtIndex: index put: (oldClass instanceVariableAtIndex: index).
	].

	"We become the old class and metaclass into the new ones"
	oldClassMetaclass := oldClass basicClass.
	oldClass becomeInto: newClass.
	oldClassMetaclass target == Metaclass
		ifFalse:[ oldClassMetaclass becomeInto: newClass basicClass ] 
]

{ #category : #'mapping-mirrors-definitions' }
PBClassLoaderGeneric >> mirrorDefinitionMap [
	^ mirrorDefinitionMap
]

{ #category : #'class-creation-stubs' }
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
	(self bootstrapEnvironment includesKey: className)
		ifTrue: [ self migrateOldClass: (self bootstrapEnvironment at: className) to: aClassMirror ].

	"We register it and keep up to date our compact class index"
	self bootstrapEnvironment at: className put: aClassMirror.
	self bootstrapEnvironment at: metaclassName put: aClassMirror basicClass.
	
	aClassMirror asClassMirror compactClassIndex = 0 ifFalse: [ 
		builder objectSpace backend compactClassAt: aClassMirror asClassMirror compactClassIndex put: aClassMirror ].
	
	"We create a mapping between the real classes and their definitions for later"
	self map: aClassMirror with: definition.
	
	"We say the simulator that this class is a root, possible becomes can come into it. THIS IS ONLY VALID FOR SPUR SIMULATOR BOOTSTRAP. REFACTOR ME".
	self flag: #refactorMe.
	builder objectSpace backend addExtraRoot: aClassMirror.
	builder objectSpace backend addExtraRoot: aClassMirror classSide.
	
	"We reset the class map of the ast interpreter"
	self fullyLoaded at: className put: aBoolean
]

{ #category : #'class-registration' }
PBClassLoaderGeneric >> registerClassInClassTable: aClassMirror atIndex: anInteger [ 
	self builder objectSpace backend objectMemory classAtIndex: anInteger put: aClassMirror target address.	
	
]

{ #category : #'class-creation-stubs' }
PBClassLoaderGeneric >> resetClassIndex [
	nextClassIndex := nil
]

{ #category : #accessing }
PBClassLoaderGeneric >> systemDefinition [
	^ self builder systemDefinition
]

{ #category : #'accessing-classesDefinitions' }
PBClassLoaderGeneric >> systemDefinitionAt: aString [ 
	^ self builder systemDefinitionAt: aString 
]

{ #category : #'accessing-classesDefinitions' }
PBClassLoaderGeneric >> systemDefinitionAt: aString ifAbsent: aBlockClosure [ 
	^ self builder systemDefinitionAt: aString ifAbsent: [ aBlockClosure value ]
]
