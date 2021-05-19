Class {
	#name : #User,
	#superclass : #Object,
	#instVars : [
		'questionRetriever',
		'questions',
		'answers',
		'username',
		'password',
		'topics',
		'following',
		'votes'
	],
	#category : #'TP-Refactoring-Model'
}

{ #category : #'instance-creation' }
User class >> username: aUsername password: aPassword questionRetriever: aQuestionRetriever [
	^ self new
		username: aUsername;
		password: aPassword;
		questionRetriever: aQuestionRetriever;
		yourself
]

{ #category : #adding }
User >> addAnswer: anAnswer [
	answers add: anAnswer 

]

{ #category : #adding }
User >> addQuestion: aQuestion [
	questions add: aQuestion
]

{ #category : #adding }
User >> addTopic: aTopic [
	topics add: aTopic 

]

{ #category : #adding }
User >> addVote: aVote [
	votes add: aVote
]

{ #category : #accessing }
User >> answers [
	^ answers
]

{ #category : #adding }
User >> follow: aUser [
	following add: aUser 
]

{ #category : #accessing }
User >> following [
	^ following
]

{ #category : #retrieving }
User >> getFollowingTopicsQuestions [
	^ topics flatCollect: [ :topic | topic questions ]
]

{ #category : #retrieving }
User >> getFollowingUsersQuestions [
	^ following flatCollect: [ :follow | follow questions ]
]

{ #category : #initialize }
User >> initialize [
	questions := OrderedCollection new.
	answers := OrderedCollection new.
	topics := OrderedCollection new.
	following := OrderedCollection new.
	votes := OrderedCollection new
]

{ #category : #accessing }
User >> password [
	^ password
]

{ #category : #private }
User >> password: anObject [
	password := anObject
]

{ #category : #accessing }
User >> questionRetriever [
	^ questionRetriever
]

{ #category : #private }
User >> questionRetriever: aQuestionRetriever [
	questionRetriever := aQuestionRetriever.
]

{ #category : #accessing }
User >> questions [
	^ questions
]

{ #category : #retrieving }
User >> questionsOfInterest [
	^ questionRetriever retrieveQuestions: self.
]

{ #category : #accessing }
User >> topics [
	^ topics
]

{ #category : #accessing }
User >> username [
	^ username
]

{ #category : #private }
User >> username: anObject [
	username := anObject
]

{ #category : #accessing }
User >> votes [
	^ votes
]
