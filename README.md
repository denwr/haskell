type Operator = Double -> Double -> Double <br />
type OperatorStructure = (String, Operator) <br />
type Operators = [OperatorStructure] <br />
registredOperators :: Operators <br />
registredOperators = [
                ("+", (+)),
                ("-", (-)),
                ("/", (/)),
                ("*", (*))
            ] <br />
main = print $ calculateEvaluation "12 * 3 - 4 + 6" <br />

calculateEvaluation :: String -> Double <br />
calculateEvaluation = calcIteration registredOperators . words <br />
            
calcIteration :: Operators -> [String] -> Double <br />
calcIteration _ [number] = read number <br />
calcIteration ((operator, function):xs) unparsed = <br />
    case span (/=operator) unparsed of <br />
        (_, []) -> calcIteration xs unparsed <br />
        (beforeOperator, afterOperator) -> <br />
            function <br />
                (calcIteration registredOperators beforeOperator) <br />
                (calcIteration registredOperators $ drop 1 afterOperator) <br />
