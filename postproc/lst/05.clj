  (stmt2item [stmt]
      :generalizes [local-var-stmt2simple-stmt condition2if block2block
                    return2return while-loop2loop break2break continue2continue
                    label2label stmt2simple-stmt])
