
module die_hard {

    var small: int
    var big: int

    // a state initializer
    action init = all {
        small' = 0,
        big' = 0
    }

    val state = {
        small: small,
        big: big,
    }
    
    action FillSmall = all {
        small' = 3, 
        big'= big
    }

    action FillBig = all {
        big'   = 5, 
        small' = small
    }
    action EmptySmall = all {
        small' = 0,
        big'   = big
    }
    action EmptyBig = all {
        big' = 0,
        small' = small
    }
    action SmallToBig = 
        if (big + small <= 5 ) all{
            big'   = big + small,
            small' = 0
        } else all{
            big' = 5,
            small' = small - (5 - big)
        }
    
    action BigToSmall = 
        if (big + small <= 3 ) all{
            big'   = 0,
            small' = big + small
        } else all{
            big' = small - (3 - big),
            small' = 3
        }
    action step  = any{
        FillSmall,
        FillBig,
        EmptySmall,
        EmptyBig,
        SmallToBig,
        BigToSmall
    }
    // defines invariant
    // if broken it means four gallons of water could be poured in the big jug using 3 and 5 gallon jugs
    val bigJugNotEqualTo4: bool = big != 4

    // try invariant with 
    // quint run --invariant=bigJugNotEqualTo4 --verbosity=3 die_hard.qnt 
}
