module ctf_04 {

    type User = str


    var tot_supply: int
    var state_balances: str -> int
    var user_balances: str -> int
    var contr_balance: int
    var status: str

    pure val MAX_USER_AMOUNT = 10

    pure val USERS = Set("user1", "user2")

    val state= {
        tot_supply: tot_supply,
        state_balances: state_balances,
        user_balances: user_balances,
        contr_balance: contr_balance,
        status: status,
        //history:history
    }

    //  type Ctf_04_step = {
    //     st: state
    // }

    var history: List[state]

    def userBalancesLessOrEqualThan (accs: a -> int, amt: int) : bool ={
        accs.keys().fold(true, (res, acc) => res and (accs.get(acc) <= amt))
    }

    action init =  all{

        tot_supply' = 0,
        state_balances' = USERS.mapBy(a => 0),
        user_balances' = USERS.mapBy(a => oneOf(5.to(MAX_USER_AMOUNT))),
        contr_balance' = 0,
        status' = "INIT",
        history' = []
    }

    action mint(sender:User, amount: int) :bool =  {

        val contr_bal = contr_balance + amount // send tokens to contract
        val user_bal = user_balances.get(sender) - amount // decrease user balance

        val total_asset = contr_bal - amount

        val mint_amt = if (tot_supply == 0) {amount} else amount * tot_supply / total_asset

        all{
            amount < user_balances.get(sender),
            mint_amt > 0,
            amount > 0,
            contr_bal > 0,
            total_asset > 0,
          
            tot_supply' = tot_supply + mint_amt,    // increase total supply
            state_balances' = state_balances.set(sender, user_bal + mint_amt), // increase user state balance

            user_balances' = user_balances.set(sender, user_bal), // apply user balance decrease when sending amount to contract
            contr_balance' = contr_bal, // apply contract amount increase
            status' = "MINT",
            
            history' = history.append({
                st: state,
                sender:sender
            }) 
        }
       
        
    }
    
    action burn(sender:User, shares: int, amount: int) :bool =  {

    
        val contr_bal = contr_balance + amount // send tokens to contract - if any, 
        val user_bal = user_balances.get(sender) - amount // decrease user balance

        val total_asset = contr_bal
        val asset_to_return = shares * total_asset / tot_supply


        all{
            amount < user_balances.get(sender),
            asset_to_return > 0,
            contr_balance - asset_to_return > 0,
            tot_supply - shares > 0,
          
            tot_supply' = tot_supply - shares,   // decrease total supply
            state_balances' = state_balances.set(sender, user_bal - shares),  // decrease user state balance


            user_balances' = user_balances.set(sender, user_bal + asset_to_return), // send asset_to_return to user account
            contr_balance' = contr_balance - asset_to_return, // from contract

            status' = "BURN",

            history' = history.append({
                st: state,
                sender:sender
            }) 
        }
        
    }

    action step = {

        nondet sender = USERS.oneOf()
        nondet amount = (1).to(10).oneOf()
        nondet shares = (1).to(10).oneOf()

       
        any {
                mint(sender,amount),
                burn(sender, shares, amount)
            }
    }

     val UserCanNotWithdrawMoreThanDeposited: bool = all{
        userBalancesLessOrEqualThan(user_balances,MAX_USER_AMOUNT)
    }

}