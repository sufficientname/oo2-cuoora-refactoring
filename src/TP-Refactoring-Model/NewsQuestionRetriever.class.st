Class {
	#name : #NewsQuestionRetriever,
	#superclass : #QuestionRetriever,
	#category : #'TP-Refactoring-Model'
}

{ #category : #retrieving }
NewsQuestionRetriever >> getQuestionsFor: aUser [
	^ cuoora questions select: [ :q | q timestamp asDate = Date today ]
]
