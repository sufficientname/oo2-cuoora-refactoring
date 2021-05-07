Class {
	#name : #Question,
	#superclass : #Publication,
	#instVars : [
		'title',
		'answers',
		'topics'
	],
	#category : #'TP-Refactoring-Model'
}

{ #category : #'instance creation' }
Question class >> newWithTitle: aTitle description: aDescription user: aUser [
	^ self new
		title: aTitle;
		description: aDescription;
		user: aUser;
		yourself.
]

{ #category : #'instance creation' }
Question class >> newWithTitle: aTitle description: aDescription user: aUser topic: aTopic [
	^ self new
		title: aTitle;
		description: aDescription;
		addTopic: aTopic;
		user: aUser;
		yourself.
]

{ #category : #adding }
Question >> addTopic: aTopic [
	topics add: aTopic.
	aTopic addQuestion: self.

]

{ #category : #initialization }
Question >> initialize [
	super initialize.
	answers := OrderedCollection new.
	topics := OrderedCollection new
]

{ #category : #accessing }
Question >> title [
	^title 
]

{ #category : #accessing }
Question >> title: aTitle [
	title := aTitle 
]

{ #category : #accessing }
Question >> topics [
	^topics 
]
