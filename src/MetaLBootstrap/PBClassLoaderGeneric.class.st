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
	#category : #'MetaLBootstrap-builders'
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
	"^ anEPClassMirror asRGClass"
	^ self addressDefinitionMap at: anEPClassMirror target address 
		ifAbsent: [ anEPClassMirror asRGClass ]
]

{ #category : #'accessing-classes-installed' }
PBClassLoaderGeneric >> classNamed: aString [ 
	self deprecated: 'use (LanguageDefinition >> classNamed) and then ensure the remote is installed'.
	^ ((self bootstrapEnvironmentAt: aString) 
		ifNil: [ self loadClassNamed: aString ]) asRemoteClass
]

{ #category : #'stubs-instance-creation' }
PBClassLoaderGeneric >> classWithRole: aString [
	^ self systemDefinition classWithRole: aString
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
				ifAbsentPut: [ 
					"self createStubForClassNamed: aClassDefinition name"
					aClassDefinition installStub ] ].
	
	newBehavior := (self systemDefinition
		installClass: aClassDefinition) asRemoteClass.
	
	"register class in remote globals"
	self systemDefinition
		registerClass: newBehavior
		fromDefinition: aClassDefinition.
	 "we register the installed class in the remote system dictionary only if it's the fully loaded version"
	(self systemDefinition
				hookRegisterClass: newBehavior 
				named: aClassDefinition name) evaluate. 
	(self systemDefinition hookInitializeMethodDictionaryFor: newBehavior)
		evaluate.
	
	self systemDictionary at: aClassDefinition name put: newBehavior
]

{ #category : #'stubs-instance-creation' }
PBClassLoaderGeneric >> createStubForClassNamed: aString metaclassIndex: anInteger newClassIndex: anotherInteger [
	" We install by hand (non reflectively) a stub that represents a class using as template its definition in the ring model (language definition) " 
	| stub classDef metaclassIndex newClassIndex |
	classDef := self systemDefinition classNamed: aString.
	metaclassIndex := anInteger.
	newClassIndex := anotherInteger.
	" First we create a zero sized stub object that represents the class.
	We do so by creating an instance of its metaclass "
	stub := (self createStubInstanceOf: classDef metaclass classIndex: metaclassIndex) asRemoteClass.
	
	" Then we set its format (this is saved in the third instance variable of classes)"
	stub format: classDef format.
	
	" Finally we set its identity hash. A class identityHash is its index "
	stub setHashBitsTo: newClassIndex.
	self registerClass: stub definition: classDef fullyLoaded: false.
	self assert: stub classTableIndex = newClassIndex.
	
	^ stub
]

{ #category : #'stubs-instance-creation' }
PBClassLoaderGeneric >> createStubInstanceOf: aClassModel classIndex: anInteger [
	" A basic stub is an object of size 0 and its class index saved in its header "
	| format stub classIndex |
	format := aClassModel format.
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
			createObjectWithFormat: (self classWithRole: #Array) format 
			withSize: 60
			classIndex: self builder arrayClassIndex.
]

{ #category : #destroy }
PBClassLoaderGeneric >> destroy [
	addressDefinitionMap := nil.
	builder := nil
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
	fullyLoaded := Dictionary new.
	nextClassIndex := nil
]

{ #category : #'initializing-classes' }
PBClassLoaderGeneric >> initializeClassPool: aClassName [

	| definition class classPool classVariableBindings |
	self deprecated: 'implement this in your ClassModel subclass and call it from LanguageModel>>createInitialObjects'.
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

{ #category : #registering }
PBClassLoaderGeneric >> installClass: aClassMirror inClassTableIndex: anInteger inBootstrapEnvironment: className [ 
	self backend installClass: aClassMirror inClassTableIndex: anInteger.
	"If it existed before, we migrate its state"
	(aClassMirror isStub not
		and: [ self bootstrapEnvironment includesKey: className ])
		ifTrue: [ self
				migrateOldClass: (self bootstrapEnvironment at: className)
				to: aClassMirror ].
	"Register in the bootstrap environment if it doesn't have me"
	(self bootstrapEnvironment includesKey: className)
		ifFalse: [ self bootstrapEnvironment at: className put: aClassMirror ].

]

{ #category : #'stubs-class-creation' }
PBClassLoaderGeneric >> installMulticlassStubs: aCollection [
	| names classes |
	" ensure the metaclass of each class-model in the collection is present  "
	names := aCollection collect: #name.
	classes := #() asOrderedCollection.
	aCollection
		do: [ :class | 
			(names anySatisfy: [ :c | c = class metaclass name ])
				ifFalse: [ classes add: class metaclass ] ].
	classes addAll: aCollection.
	"calculate index in class table"
	classes
		do: [ :class | 
			class indexInClassTable
				ifNil: [ class indexInClassTable: self newClassIndex ] ].
	"create stubs for each class and register"
	classes
		do: [ :class | 
			class remoteIsInstalled
				ifFalse: [ 
					self installStubBasic: class ] ].
	classes do: [ :class | class testStub ]
]

{ #category : #'installing stubs' }
PBClassLoaderGeneric >> installStub: aClassModel [ 
	" I install the stub of my corresponding class in the kernel.
	I depend on the implementation of installStubCustom "

	aClassModel remoteIsInstalled
		ifTrue: [ ^ aClassModel remote ].
	((aClassModel parent firstMetaclasses includes: aClassModel)
		or: [ aClassModel metaclass = aClassModel or: [ aClassModel metaclass metaclass = aClassModel ] ])
		ifTrue: [ self
				error:
					'Use LanguageModel >> installMulticlassStubs to install required classes' ].
	aClassModel indexInClassTable
		ifNil: [ aClassModel indexInClassTable: self newClassIndex ].
	self installStub: aClassModel metaclass.
	self installStubBasic: aClassModel.
	aClassModel testStub.
	^ aClassModel remote
]

{ #category : #'installing stubs' }
PBClassLoaderGeneric >> installStubBasic: aClassModel [
	" I install the stub of aClassModel in the kernel "

	aClassModel
		remote:
			(self
				createStubForClassNamed: aClassModel name
				metaclassIndex: aClassModel metaclass indexInClassTable
				newClassIndex: aClassModel indexInClassTable).
	self
		registerClass: aClassModel remote
		named: aClassModel name
		fullyLoaded: false.
	^ aClassModel remote
]

{ #category : #'stubs-class-creation' }
PBClassLoaderGeneric >> installStubs [
	
	self installMulticlassStubs: self systemDefinition firstMetaclasses 
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

{ #category : #'stubs-class-creation' }
PBClassLoaderGeneric >> lastClassIndex [
	^ nextClassIndex
]

{ #category : #'class-creation' }
PBClassLoaderGeneric >> loadClassNamed: aString [
	| classDef |
	self deprecated: 'use ClassModel >> installStub'.
	classDef := self systemDefinitionAt: aString.
	classDef installStub.
	^ classDef remote
]

{ #category : #'accessing-mapping-mirrors/definitions' }
PBClassLoaderGeneric >> map: aClassMirror with: aDefinition [
	"We create a mapping between the real classes and their definitions for later"
	addressDefinitionMap at: aClassMirror target address put: aDefinition.
	aDefinition propertyNamed: #address put: aClassMirror target address
]

{ #category : #'class-registration' }
PBClassLoaderGeneric >> migrateOldClass: oldClass to: newClass [

	| oldClassAddress |
	
	oldClassAddress := oldClass target address.
	oldClassAddress == newClass target address
		ifTrue: [ 
			^ newClass ].
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
PBClassLoaderGeneric >> registerClass: aClassMirror named: className fullyLoaded: aBoolean [
	| definition |
	definition := self systemDefinition classNamed: className.
	self assert: aClassMirror format isNilObject not.
	self installClass: aClassMirror inClassTableIndex: aClassMirror classTableIndex inBootstrapEnvironment: className.
	
	"We register it and keep up to date our compact class index"
	aClassMirror compactClassIndex = 0
		ifFalse: [ self
				error: 'in spur all classes are compact. We should not have arrived here'
			".builder objectSpace backend
				compactClassAt: aClassMirror asClassMirror compactClassIndex
				put: aClassMirror" ].

	"We create a mapping between the real classes and their definitions for later"
	self map: aClassMirror with: definition.

	"We say the simulator that this class is a root, possible becomes can come into it. THIS IS ONLY VALID FOR SPUR SIMULATOR BOOTSTRAP. REFACTOR ME"
	self flag: #refactorMe.
	builder objectSpace backend addExtraRoot: aClassMirror.

	"We reset the class map of the ast interpreter"
	self fullyLoaded at: className put: aBoolean
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
	self deprecated: 'use classWithRole'.
	^ self builder systemDefinitionAt: aString 
]

{ #category : #'accessing-classes-definitions' }
PBClassLoaderGeneric >> systemDefinitionAt: aString ifAbsent: aBlockClosure [ 
	^ self builder systemDefinitionAt: aString ifAbsent: [ aBlockClosure value ]
]

{ #category : #accessing }
PBClassLoaderGeneric >> systemDictionary [
	^ self builder objectSpace systemDictionary
]
