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

{ #category : #retrieving }
QuestionRetriever >> limitQuestions: aQuestionCollection [
	^ aQuestionCollection
		last: (self questionsLimit min: aQuestionCollection size)
]

{ #category : #private }
QuestionRetriever >> questionsLimit [
	^ 100
]

{ #category : #retrieving }
QuestionRetriever >> retrieveQuestions: aUser [
	^ (self
		limitQuestions: (self sortQuestions: (self getQuestionsFor: aUser)))
		reject: [ :q | q user = aUser ]
]

{ #category : #retrieving }
QuestionRetriever >> sortQuestions: aQuestionCollection [
	^ aQuestionCollection asSortedCollection: [ :a :b | a positiveVotesCount > b positiveVotesCount ]
]
