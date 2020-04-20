Class {
	#name : #PBClassLoaderGeneric,
	#superclass : #Object,
	#instVars : [
		'builder',
		'fullyLoaded'
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

{ #category : #'accessing-memory' }
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

{ #category : #'mapping-remote' }
PBClassLoaderGeneric >> classForClassMirror: anEPClassMirror [ 
	^ [anEPClassMirror asRGClass] on: Error do: [ self systemDefinition classWithRole: #Class ]
]

{ #category : #accessing }
PBClassLoaderGeneric >> classNamed: aString [ 
	
	^ (self bootstrapEnvironmentAt: aString) 
		ifNil: [ self loadClassNamed: aString ]
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
			self registerClass: stub basicClass named: aRGBehavior metaclass name.
			stub ]
		ifFalse: [ self error: 'Impossible to create class stub! The argument is not a behavior' ]
]

{ #category : #'class-creation-stubs' }
PBClassLoaderGeneric >> createStubForClassNamed: aString [ 

	^ self createStubForClass: (self systemDefinitionAt: aString)
]

{ #category : #'class-creation-stubs' }
PBClassLoaderGeneric >> createStubForClassNamed: aString metaclassIndex: anInteger newClassIndex: anotherInteger [
	" The way to install a stub depends on wether or not we are using reflective operations "
	self subclassResponsibility 
]

{ #category : #stubs }
PBClassLoaderGeneric >> createStubInstanceOf: aClassName classIndex: anInteger [
	self subclassResponsibility 
]

{ #category : #stubs }
PBClassLoaderGeneric >> createStubInstanceOfArrayOfSize: anInteger [ 
	self subclassResponsibility 
]

{ #category : #'accessing-private' }
PBClassLoaderGeneric >> fullyLoaded [
	^ fullyLoaded
]

{ #category : #accessing }
PBClassLoaderGeneric >> initialize [

	super initialize.	
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

{ #category : #'creating-classes' }
PBClassLoaderGeneric >> loadClassNamed: aString [
	| class |
	class := self createStubForClassNamed: aString.
	"shouldInitializeClassPool
		ifTrue: [ self initializeClassPool: aString ]."
	^ class
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
	self registerClass: theNewMetaclassClass named: aRGBehavior metaclass name.
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
PBClassLoaderGeneric >> registerClass: aClassMirror named: aName fullyLoaded: aBoolean [
	"If it existed before, we migrate its state"
	(self bootstrapEnvironment includesKey: aName)
		ifTrue: [ self migrateOldClass: (self bootstrapEnvironment at: aName) to: aClassMirror ].

	"We register it and keep up to date our compact class index"
	self bootstrapEnvironment at: aName put: aClassMirror.
	"self bootstrapEnvironment at: (aName,' class') asByteString put: aClassMirror basicClass."
	
	aClassMirror asClassMirror compactClassIndex = 0 ifFalse: [ 
		builder objectSpace backend compactClassAt: aClassMirror asClassMirror compactClassIndex put: aClassMirror ].
	
	"We say the simulator that this class is a root, possible becomes can come into it. THIS IS ONLY VALID FOR SPUR SIMULATOR BOOTSTRAP. REFACTOR ME".
	self flag: #refactorMe.
	builder objectSpace backend addExtraRoot: aClassMirror.
	
	"We reset the class map of the ast interpreter"
	self fullyLoaded at: aName put: aBoolean
]

{ #category : #'class-creation-stubs' }
PBClassLoaderGeneric >> resetClassIndex [
	nextClassIndex := nil
]

{ #category : #accessing }
PBClassLoaderGeneric >> systemDefinition [
	^ self builder systemDefinition
]

{ #category : #'accessing-systemDefinition' }
PBClassLoaderGeneric >> systemDefinitionAt: aString [ 
	^ self builder systemDefinitionAt: aString
]

{ #category : #accessing }
PBClassLoaderGeneric >> systemDefinitionAt: aString ifAbsent: aBlockClosure [ 
	^ [self systemDefinitionAt: aString] on: Error do: [aBlockClosure value]
]
