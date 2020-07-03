Class {
	#name : #PBMethodCompilerGeneric,
	#superclass : #Object,
	#instVars : [
		'builder'
	],
	#category : #'PharoBootstrapGeneric-builders'
}

{ #category : #accessors }
PBMethodCompilerGeneric >> builder: aPBImageBuilderGeneric [ 
	^ builder := aPBImageBuilderGeneric
]

{ #category : #'building-methods' }
PBMethodCompilerGeneric >> createDoit: aString withTemps: aCollection inObjectSpace: anObjectSpace [ 
	
	^  anObjectSpace crossCompiler compileDoIt: aString
]

{ #category : #'building-methods' }
PBMethodCompilerGeneric >> createNewMethod: aMethodDefinition [

"	| ownerClass |
""	ownerClass := builder behaviorFrom: aMethodDefinition methodClass theNonMetaClass."
"	aMethodDefinition methodClass isMeta ifTrue: [ ownerClass := ownerClass classSide ].
"
	| method timeToCompile |
	timeToCompile := [ method := builder objectSpace crossCompiler
		compile: aMethodDefinition sourceCode
		forClass: aMethodDefinition methodClass
		inEnvironment: builder] timeToRun.
	"builder statistics addCompilationTime: timeToCompile."
	^ method
]

{ #category : #'building-methods' }
PBMethodCompilerGeneric >> methodsFromDefinition: aBehaviorDefinition [
	"Get all the methods we consider we want from the current environment and return a copy, binded to the new class"
	^ aBehaviorDefinition localMethods 
		collect: [ :m | self createNewMethod: m ].
]
