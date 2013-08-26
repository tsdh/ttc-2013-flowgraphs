(make-test test-fg-transform-test0 "models/Test0.java.xmi"
           #{["testMethod()"   "int a = 1;"]     ;; expected cfNext links
             ;; more [cf-predecessor cf-successor] tuples
             ["return b * c;"  "Exit"]}
           #{["int a = 1;"     "int c = a + b;"] ;; expected dfNext links
             ;; more [df-predecessor df-successor] tuples
             ["b = a - b;"     "return b * c;"]})
