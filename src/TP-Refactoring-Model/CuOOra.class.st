Class {
	#name : #CuOOra,
	#superclass : #Object,
	#instVars : [
		'users',
		'topics',
		'questions'
	],
	#category : #'TP-Refactoring-Model'
}

{ #category : #adding }
CuOOra >> addQuestion: aQuestion [
	questions add: aQuestion
]

{ #category : #adding }
CuOOra >> addQuestion: aQuestion forUser: aUser [
	aUser addQuestion: aQuestion.
	questions add: aQuestion.
]

{ #category : #adding }
CuOOra >> addTopic: aTopic [
	topics add: aTopic 

]

{ #category : #adding }
CuOOra >> addUser: aUser [
	users add: aUser 

]

{ #category : #retrieving }
CuOOra >> averageVotes: aNumber [
	^ (questions sum: [ :q | q positiveVotesCount ]) / aNumber
]

{ #category : #retrieving }
CuOOra >> getPopularTodayQuestions [
	^ self getTodayQuestions
		select: [ :q | 
			q positiveVotesCount
				>= (self averageVotes: self getTodayQuestions size) ]
]

{ #category : #retrieving }
CuOOra >> getTodayQuestions [
	^ self questions select: [ :q | q timestamp asDate = Date today ]
]

{ #category : #initialize }
CuOOra >> initialize [
	users := OrderedCollection new.
	topics := OrderedCollection new.
	questions := OrderedCollection new
]

{ #category : #accessing }
CuOOra >> questions [
	^ questions
]

{ #category : #accessing }
CuOOra >> topics [
	^ topics
]

{ #category : #accessing }
CuOOra >> users [
	^ users
]
