Lam "x_1" (Base IntType) 
    (
        App 
            (App (Lam "x_2" (Base IntType) (Var "x_2")) (Var "x_1")) 
            (App (Lam "x_2" (Base IntType) (Var "x_2")) (Var "x_1"))
        )
        

(\x_1:IntType .
    (
        (
            (\x_2:IntType . x_2) x_1
        ) 
        (
            (\x_2:IntType . x_2) x_1
        )
    )
)

(\x:IntType -> ())

--The question is given a type
--can I construct ...no I can only make arrow types for sure

--so if the type to make in a lambda is not an arrow I must gen a symbol


so the rule is this. With a single constant I can reduce arrows and return Base.

I can reduce with lambdas

so the algo is simple

recurse until zero. Then cap

cap builds what ever type is necessary

if it is base -> constant
else if it is a lambda
