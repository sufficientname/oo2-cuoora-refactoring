Class {
	#name : #SocialQuestionRetriever,
	#superclass : #QuestionRetriever,
	#category : #'TP-Refactoring-Model'
}

{ #category : #retrieving }
SocialQuestionRetriever >> getQuestionsFor: aUser [
	^ aUser following flatCollect: [ :follow | follow questions ]
]
