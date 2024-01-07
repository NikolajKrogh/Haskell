-- quango is parametric polymorphic because it can take any type of argument
quango :: a -> [a]
quango x = [x]

-- tango is ad-hoc polymorphic because it can take any type of argument, but p1 must be a Num
tango :: Num p1 => (a, b) -> p2 -> p1
tango (x, y) z = 42

-- Opg 2:
{-
(λx.xy)(λz.(λu.uu))}
(λx.xy)(λz.(λu.uu))
Apply first lambda abstraction:
(λx.xy)(λz.(λu.uu)) → (λz.(λu.uu))y
Apply second lambda abstraction:
(λz.(λu.uu))y → (λu.uu)
So, the terminating reduction sequence is:
(λx.xy)(λz.(λu.uu)) → (λu.uu)
The reduction terminates with (λu.uu)
-}

-- Opg 3 in class lecture
-- is polymorphic because it can take any type of argument
dingo ( x , y ) = [ x , y ]