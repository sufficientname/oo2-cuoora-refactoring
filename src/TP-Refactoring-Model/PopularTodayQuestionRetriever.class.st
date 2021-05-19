Class {
	#name : #PopularTodayQuestionRetriever,
	#superclass : #QuestionRetriever,
	#category : #'TP-Refactoring-Model'
}

{ #category : #retrieving }
PopularTodayQuestionRetriever >> getQuestionsFor: aUser [
	^ cuoora getPopularTodayQuestions
]
