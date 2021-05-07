Class {
	#name : #TopicsQuestionRetriever,
	#superclass : #QuestionRetriever,
	#category : #'TP-Refactoring-Model'
}

{ #category : #retrieving }
TopicsQuestionRetriever >> getQuestionsFor: aUser [
	^ aUser topics flatCollect: [ :topic | topic questions ]

]
