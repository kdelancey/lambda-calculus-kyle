module LambdaLexer where
    import Text.Parsec
    import Text.Parsec.Combinator (between, sepBy1, chainr1)
    import Data.List (elemIndex)


    -- Simply Typed λ-Calculus Evaluator
    -- http://www.ling.ohio-state.edu/~pollard.4/type/books/pierce-tpl.pdf, page 113
    -- Grammar for λ-terms in our implementation:
    --      M, N ::= x | λx . M | M N

    