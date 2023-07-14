module ctf_04 {

    type User = str


    var tot_supply: int
    var state_balances: str -> int
    var user_balances: str -> int
    var minted: str -> int
    var burned: str -> int
    var contr_balance: int
    var ratio: int
    var amt: int
    var opt_burn_amt: int
    var user: str
    var status: str

    pure val MAX_USER_AMOUNT = 10

    pure val USERS = Set("user1", "user2")

    def calculateSumOfMapValues(m: str -> int): int = {
        m.keys().fold(0, (res, it) => 
            res + m.get(it)
        )
    }

    def calculateMintAmount(amount: int, total_asset: int): int = {

        if(tot_supply == 0) {
            amount
        } 
        else {
            amount * tot_supply / total_asset
        }
    }

    def avoidDivByZeroBurn(bal: int): int = {

        if(bal == 0) {
            1
        } 
        else {
            bal
        }
    }

     val state= {
        tot_supply: tot_supply,
        state_balances: state_balances,
        user_balances: user_balances,
        contr_balance: contr_balance,
        minted: minted,
        burned: burned,
        opt_burn_amt: opt_burn_amt,
        amt: amt,
        status: status,
    }

    action init =  all{

        tot_supply' = 0,
        state_balances' = USERS.mapBy(a => 0),
        user_balances' = USERS.mapBy(a => oneOf(5.to(MAX_USER_AMOUNT))),
        contr_balance' = 0,
        minted' = USERS.mapBy(a => 0),
        burned' = USERS.mapBy(a => 0),
        ratio' = 1,
        amt' = 0,
        opt_burn_amt' = 0,
        user' = "/",
        status' = "INIT",
    }

    action mint(sender:User, amount: int) :bool =  {

        val mint_amt = calculateMintAmount(amount, contr_balance)

        all{
            amount <= user_balances.get(sender),
            mint_amt > 0,
            amount > 0,
            tot_supply' = tot_supply + mint_amt,    // increase total supply
            state_balances' = state_balances.set(sender, state_balances.get(sender) + mint_amt), // increase user state balance
            minted' = minted.set(sender, mint_amt + minted.get(sender)),
            burned' = burned,
            user_balances' = user_balances.set(sender, user_balances.get(sender) - amount), // apply user balance decrease when sending amount to contract
            contr_balance' = contr_balance + amount, // apply contract amount increase
            amt' = amount,
            opt_burn_amt' = opt_burn_amt,
            user' = sender,
            status' = "MINT",
        }
       
        
    }
    
    action burn(sender:User, shares: int, opt_amt: int) :bool =  {
        val total_supply = avoidDivByZeroBurn(tot_supply)
        val asset_to_return = shares * contr_balance / total_supply
    
        all{
            tot_supply > 0,
            asset_to_return > 0,
            tot_supply - shares >= 0,
            opt_amt <= user_balances.get(sender),
            tot_supply' = tot_supply - shares,   // decrease total supply
            state_balances' = state_balances.set(sender, state_balances.get(sender) - shares),
            user_balances' = user_balances.set(sender, user_balances.get(sender) + asset_to_return - opt_amt), // send asset_to_return to user account
            contr_balance' = contr_balance - asset_to_return + opt_amt, // from contract
            minted' = minted,
            burned' = burned.set(sender, asset_to_return + burned.get(sender)),
            amt' = shares,
            opt_burn_amt' = opt_amt,
            user' = sender,
            status' = "BURN",
        }
        
    }

    action step = {

        nondet sender = USERS.oneOf()
        nondet amount = (1).to(10).oneOf()
        nondet shares = (1).to(10).oneOf()
        nondet opt_amt = (0).to(2).oneOf()

       
        any {
                mint(sender,amount),
                burn(sender, shares, opt_amt)
            }
    }

     val UserCanNotWithdrawMoreThanDeposited: bool = {
         if (status == "BURN")
            val sum_of_minted = calculateSumOfMapValues(minted)
            val sum_of_burned = calculateSumOfMapValues(burned) 
            sum_of_burned <= sum_of_minted
        else
            true
    }

}