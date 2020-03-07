namespace Integration_library

type Kovila77Solve() = 
    interface Knapsack.ISolver with
        member this.GetName() = "kovila77s solution, full search"
        member this.Solve(M:int, m:int[], c:int[]) = 
            let fst (a:List<bool>,_,_) = a
            let rec _solve max (m:List<int>) (c:List<int>) (taken:List<bool>) mass costs = 
                match m with
                    | [] -> (taken, mass, costs)
                    | h::t -> 
                        let (takenIfTake, sumMassIfTake, sumCostsIfTake) = _solve max t c.Tail (taken@[true]) (mass+h) (costs+c.Head)
                        let (takenIfNot, sumMassIfNot, sumCostsIfNot) = _solve max t c.Tail (taken@[false]) (mass) (costs)
                        if sumMassIfTake > max && sumCostsIfTake <= sumCostsIfNot then 
                            (takenIfNot, sumMassIfNot, sumCostsIfNot)
                        else 
                            (takenIfTake, sumMassIfTake, sumCostsIfTake)
            (Array.ofList (fst (_solve M (Array.toList m) (Array.toList c) [] 0 0)))
            
