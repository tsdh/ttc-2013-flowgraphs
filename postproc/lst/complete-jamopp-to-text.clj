(declare-polyfn stmt2str [elem])

(defn reduce-str [els]
  (reduce #(str %1 (stmt2str %2)) "" els))

(defpolyfn stmt2str 'members.ClassMethod [method]
  (str (eget method :name) "()"))

(defpolyfn stmt2str 'types.PrimitiveType [^org.eclipse.emf.ecore.EObject pt]
  (clojure.string/lower-case (.getName (.eClass pt))))

(defpolyfn stmt2str 'statements.LocalVariableStatement [lv]
  (let [v (eget lv :variable)]
    (str (stmt2str (eget v :typeReference)) " " (stmt2str v)
         (when-let [iv (eget v :initialValue)]
           (str " = " (stmt2str iv)))
         ";")))

(defpolyfn stmt2str 'references.IdentifierReference [ir]
  (stmt2str (eget ir :target)))

(defpolyfn stmt2str 'variables.Variable [v]
  (eget v :name))

(defpolyfn stmt2str 'expressions.MultiplicativeExpression [me]
  (let [[c1 c2] (eget me :children)]
    (str (stmt2str c1) " " (reduce-str (eget me :multiplicativeOperators))
         " " (stmt2str c2))))

(defpolyfn stmt2str 'expressions.EqualityExpression [ee]
  (let [[c1 c2] (eget ee :children)]
    (str (stmt2str c1) " " (reduce-str (eget ee :equalityOperators))
         " " (stmt2str c2))))

(defpolyfn stmt2str 'expressions.AdditiveExpression [ae]
  (let [[c1 c2] (eget ae :children)]
    (str (stmt2str c1) " " (reduce-str (eget ae :additiveOperators))
         " " (stmt2str c2))))

(defpolyfn stmt2str 'expressions.UnaryExpression [ue]
  (str (reduce-str (eget ue :operators))
       (stmt2str (eget ue :child))))

(defpolyfn stmt2str 'expressions.AssignmentExpression [ae]
  (str (stmt2str (eget ae :child)) " "
       (stmt2str (eget ae :assignmentOperator)) " "
       (stmt2str (eget ae :value))))

(defpolyfn stmt2str 'expressions.RelationExpression [re]
  (let [[c1 c2] (eget re :children)]
    (str (stmt2str c1) " "
         (reduce-str (eget re :relationOperators))
         " " (stmt2str c2))))

(defpolyfn stmt2str 'expressions.SuffixUnaryModificationExpression [se]
  (str (stmt2str (eget se :child))
       (stmt2str (eget se :operator))))

(defpolyfn stmt2str 'statements.Block [b]
  "\\{...\\}")

(defpolyfn stmt2str 'statements.Condition [c]
  "if")

(defpolyfn stmt2str 'statements.WhileLoop [c]
  "while")

(defpolyfn stmt2str 'statements.JumpLabel [l]
  (str (eget l :name) ":"))

(defpolyfn stmt2str 'statements.Break [b]
  (str "break"
       (when-let [l (eget b :target)]
         (str " " (eget l :name)))
       ";"))

(defpolyfn stmt2str 'statements.Continue [c]
  (str "continue"
       (when-let [l (eget c :target)]
         (str " " (eget l :name)))
       ";"))

(defpolyfn stmt2str 'statements.Return [r]
  (str "return" (when-let [rv (eget r :returnValue)]
                  (str " " (stmt2str rv)))
       ";"))

(defpolyfn stmt2str 'statements.ExpressionStatement [stmt]
  (str (stmt2str (eget stmt :expression)) ";"))

(defpolyfn stmt2str 'operators.Operator [op]
  (type-case op
    'operators.Multiplication "*"
    'operators.Subtraction    "-"
    'operators.Addition       "+"
    'operators.Division       "/"
    'operators.LessThan       "<"
    'operators.GreaterThan    ">"
    'operators.Assignment     "="
    'operators.MinusMinus     "--"
    'operators.PlusPlus       "++"
    'operators.AssignmentPlus "+="
    'operators.Equal          "=="))

(defpolyfn stmt2str 'literals.Literal [l]
  (type-case l
    'literals.DecimalIntegerLiteral (eget l :decimalValue)))
