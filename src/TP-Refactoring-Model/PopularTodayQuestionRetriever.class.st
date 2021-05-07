Class {
	#name : #PopularTodayQuestionRetriever,
	#superclass : #QuestionRetriever,
	#category : #'TP-Refactoring-Model'
}

{ #category : #retrieving }
PopularTodayQuestionRetriever >> getQuestionsFor: aUser [
	| popularTCol |
	popularTCol := cuoora questions
		select: [ :q | q timestamp asDate = Date today ].
	^ popularTCol
		select: [ :q | q positiveVotes size >= (cuoora averageVotes: popularTCol size) ]
]
