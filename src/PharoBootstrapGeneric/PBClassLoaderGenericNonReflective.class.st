Class {
	#name : #PBClassLoaderGenericNonReflective,
	#superclass : #PBClassLoaderGeneric,
	#category : #'PharoBootstrapGeneric-builders'
}

{ #category : #stubs }
PBClassLoaderGenericNonReflective >> createStubForClassNamed: aString metaclassIndex: anInteger newClassIndex: anotherInteger [
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

{ #category : #stubs }
PBClassLoaderGenericNonReflective >> createStubInstanceOf: aClassName classIndex: anInteger [
	" A basic stub is an object of size 0 and its class index saved in its header "
	| format stub classIndex |
	format := (self systemDefinition classWithRole: aClassName) format.
	classIndex := anInteger.
	stub := (self backend
			createObjectWithFormat: format 
			withSize: 0 
			classIndex: classIndex).
	self assert: stub classIndex equals: classIndex.
	^ stub
]

{ #category : #stubs }
PBClassLoaderGenericNonReflective >> createStubInstanceOfArrayOfSize: anInteger [ 
	^ self backend
			createObjectWithFormat: (self systemDefinitionAt: #Array) format 
			withSize: 60
			classIndex: self builder arrayClassIndex.
]

{ #category : #'class-table' }
PBClassLoaderGenericNonReflective >> registerClassInClassTable: aClassMirror atIndex: anInteger [ 
	self builder objectSpace backend objectMemory classAtIndex: anInteger put: aClassMirror target address.	
	
]
