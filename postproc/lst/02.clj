(defpolyfn stmt2str 'expressions.AssignmentExpression [ae]
  (str (stmt2str (eget ae :child)) " "
       (stmt2str (eget ae :assignmentOperator)) " "
       (stmt2str (eget ae :value))))
