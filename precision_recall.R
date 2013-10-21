form_prediction_actual_matrix = function(actual,pred) { 
# create 2x2 matrix of actual class (1,0) and predicted class (1,0)
# 					       	actual class
#					          1 		0
# predicted class	1	tp		fp
# predicted class	0	fn		tn
  matrix(c(
           sum(actual==1 & pred==1), sum(actual>pred),
           sum(actual<pred),         sum(actual==0 & pred==0)),
         byrow=TRUE, nrow=2, ncol=2) 
}

precision = function(A) {
# precision = true positives / # predicted positives
# input A is 2x2 matrix of actual class (1,0) and predicted class (1,0)
# 					       	actual class
#					          1 		0
# predicted class	1	tp		fp
# predicted class	0	fn		tn
#
# where 
# 		tp = true positive
# 		fp = false positive
# 		fn = false negative
# 		tn = true negative

	ifelse( sum(A[1,]) != 0, A[1,1] / sum(A[1,]), 0 )
}

recall = function(A) {
# recall = true positives / # actual positives
# input A is 2x2 matrix of actual class (1,0) and predicted class (1,0)
# 					       	actual class
#					          1 		0
# predicted class	1	tp		fp
# predicted class	0	fn		tn
#
# where 
# 		tp = true positive
# 		fp = false positive
# 		fn = false negative
# 		tn = true negative

	ifelse( sum(A[,1]) != 0, A[1,1] / sum(A[,1]), 0 )
}

F1score = function(precision,recall) {
# computes F1 score for given precision and recall
# precision = true positives / # predicted positives
# recall = true positives / # actual positives
# F1 score is between zero and one where one is best and zero is worst
	ifelse( (precision + recall) != 0, 2 * precision * recall / (precision + recall), 0 )
}


F1score_from_actual_pred = function(actual,pred) {
# computes F1 score from 2 logical vectors of actual and predicted responses
# F1 score is between zero and one where one is best and zero is worst
  m_pred_actual = form_prediction_actual_matrix(actual,pred)
  prec = precision(m_pred_actual)
  rec = recall(m_pred_actual)
  return(F1score(prec,rec))
}
