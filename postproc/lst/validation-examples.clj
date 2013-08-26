(make-test test-fg-transform-test4 "models/Test4.java.xmi"
           #{["testMethod()" "int i = 100;"]
             ["int i = 100;" "i > 0"]
             ["i > 0"        "Exit"]
             ["i > 0"        "i > 50"]
             ["i > 50"       "i--;"]
             ["i > 50"       "i = i - 10;"]
             ["i = i - 10;"  "i == 50"]
             ["i == 50"      "break;"]
             ["i == 50"      "i > 50"]
             ["break;"       "i--;"]
             ["i--;"         "i > 0"]}
           #{["int i = 100;" "i > 0"]
             ["int i = 100;" "i > 50"]
             ["int i = 100;" "i = i - 10;"]
             ["int i = 100;" "i--;"]
             ["i = i - 10;"  "i == 50"]
             ["i = i - 10;"  "i > 50"]
             ["i = i - 10;"  "i = i - 10;"]
             ["i = i - 10;"  "i--;"]
             ["i--;"         "i > 0"]
             ["i--;"         "i > 50"]
             ["i--;"         "i = i - 10;"]
             ["i--;"         "i--;"]})

;; For the large models, only the correct number of cfNext/dfNext links is asserted.
(make-test test-fg-transform-test9 "models/Test9.java.xmi" 14452 27202)
