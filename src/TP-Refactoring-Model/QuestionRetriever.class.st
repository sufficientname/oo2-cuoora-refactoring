Class {
	#name : #QuestionRetriever,
	#superclass : #Object,
	#instVars : [
		'cuoora'
	],
	#category : #'TP-Refactoring-Model'
}

{ #category : #'instance creation' }
QuestionRetriever class >> newForCuOOra: cuoora [
	^ self new cuoora: cuoora; yourself 
]

{ #category : #accesing }
QuestionRetriever >> cuoora: aCuooraInstance [
	cuoora := aCuooraInstance 
]

{ #category : #private }
QuestionRetriever >> getQuestionsFor: aUser [
	self subclassResponsibility 
]

{ #category : #private }
QuestionRetriever >> limitAndSort: aQuestionCollection withoutQuestionsFrom: aUser [
	^ ((aQuestionCollection
		asSortedCollection: [ :a :b | a positiveVotes size > b positiveVotes size ])
		last: (self questionsLimitFor: aQuestionCollection))
		reject: [ :q | q user = aUser ]
]

{ #category : #private }
QuestionRetriever >> questionsLimit [
	^ 100
]

{ #category : #private }
QuestionRetriever >> questionsLimitFor: aQuestionCollection [
	^ self questionsLimit min: aQuestionCollection size
]

{ #category : #retrieving }
QuestionRetriever >> retrieveQuestions: aUser [
	^ self
		limitAndSort: (self getQuestionsFor: aUser)
		withoutQuestionsFrom: aUser
]
