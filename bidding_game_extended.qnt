module bidding_game_extended {
    
    type Bidder = {
        id: int,
        bid: int,
        status: str,
        amount_of_money: int,
    }

    type Game = {
        id: int,
        bid1: Bidder,
        bid2: Bidder,
        bidWon: str,
        deadline: bool
    }
    
    // state variables --------
    var games: Set[Game]
    var bidders: List[Bidder]
    // ------------------------


    val MAX_BID = 100
    val MIN_BID = -100
    val MAX_AMOUNT = 1000

    def state = {
        games: games,
        bidders: bidders
    }

    // checking if both bidders have played a move
    def bothPlayersBidded(game: Game): bool = all {
        // not(bid1.bid == -1), // superfluous
        // not(bid2.bid == -1), // superfluous
        // check whether the bids are valid
        game.bid1.bid > 0,
        game.bid2.bid > 0
    }

    // checking if both bidders were selected
    def bothPlayersPicked(game: Game): bool = all {
        game.bid1.status == "PLAYING",
        game.bid2.status == "PLAYING"
    }

    // declares a winner
    def declareWinner(b1: int, b2: int): str = {
        if(b1 != b2) {
            if (b1 > b2)
                "bidder1"
            else
                "bidder2"
        } else {
            ""
        }
    }

    // returns a list of bidders who's status is "NOT_PLAYING"
    def availableBidders: List[Bidder] =
        bidders.select(b => b.status == "NOT_PLAYING")

    // picks two random numbers from a provided list
    pure def pickTwoNumbers(s: Set[int]): List[int] = {
        if (size(s) >= 2) {
            nondet idx = oneOf(s)
            nondet idx2 = oneOf(s.filter(num => num != idx))
            [idx, idx2]
        } else {
            []
        }
    }

    // defines chances for action to happen
    pure def chanceToPass: int =
        oneOf(0.to(1))

    // iterates through free-bidders list and appends their ids to a set
    def getNumbersForSelection: Set[int] =
        availableBidders.foldl(Set(), (s, b) => s.union(Set(b.id)))

    
    action init = all {
        bidders' = List({id: 0, bid: -1, status: "NOT_PLAYING", amount_of_money: oneOf(0.to(MAX_AMOUNT))}, {id: 1, bid: -1, status: "NOT_PLAYING", amount_of_money: oneOf(0.to(MAX_AMOUNT))}, {id: 2, bid: -1, status: "NOT_PLAYING", amount_of_money: oneOf(0.to(MAX_AMOUNT))}, {id: 3, bid: -1, status: "NOT_PLAYING", amount_of_money: oneOf(0.to(MAX_AMOUNT))}, {id: 4, bid: -1, status: "NOT_PLAYING", amount_of_money: oneOf(0.to(MAX_AMOUNT))}),
        /*val bid1 = (-1, -1, "", -1)
        val bid2 = (-1, -1, "", -1)
        val bidWon = ""
        val deadline = false*/
        games' = Set()
    }

    action createNewGame = all {
        val game = {id: size(games), bid1: {id: -1, bid: -1, status: "", amount_of_money: -1}, bid2: {id: -1, bid: -1, status: "", amount_of_money: -1}, deadline: false, bidWon: ""}
        games' = games.union(Set(game)),
        bidders' = bidders
    }

    action pickBidders(gameID: int): bool = all {
        val willPass = chanceToPass    // unreliable communication
        willPass == 1,
        val idxs = pickTwoNumbers(getNumbersForSelection)
        val game = games.filter(g => g.id == gameID).oneOf() // "uzmi neki" element iz seta (koji u sebi ima samo jedan element - onaj sa kojim se prosledjeni ID podudara)
        all {
            not(bothPlayersPicked(game)),
            length(idxs) >= 2,  // there has to be at least two players for game to be created
            bidders' = bidders.replaceAt(idxs[0], {id: idxs[0], bid: -1, status: "PLAYING", amount_of_money: bidders[idxs[0]].amount_of_money}).replaceAt(idxs[1], {id: idxs[1], bid: -1, status: "PLAYING", amount_of_money: bidders[idxs[1]].amount_of_money}),
            val idx = game.id
            val bi1 = {id: bidders[idxs[0]].id, bid: bidders[idxs[0]].bid, status: "PLAYING", amount_of_money: bidders[idxs[0]].amount_of_money}
            val bi2 = {id: bidders[idxs[1]].id, bid: bidders[idxs[1]].bid, status: "PLAYING", amount_of_money: bidders[idxs[1]].amount_of_money}
            val dl = game.deadline
            val bw = game.bidWon
            val gg = game.with("id", idx).with("bid1", bi1).with("bid2", bi2).with("deadline", dl).with("bidWon", bw)
            games' = games.filter(g => g != game).union(Set(gg))
        }
    }


    action makeBid1(gameID: int): bool = all {
        val willPass = chanceToPass
        willPass == 1,
        size(games.filter(g => g.id == gameID)) > 0,    // precondition - element with defined id must exist
        
        val game = oneOf(games.filter(g => g.id == gameID))
        nondet b1 = oneOf(MIN_BID.to(MAX_BID))
        all {
            bothPlayersPicked(game),
            game.deadline == false,
            game.bid1.amount_of_money >= b1,
            bidders' = bidders.replaceAt(game.bid1.id, {id: game.bid1.id, bid: b1, status: game.bid1.status, amount_of_money: game.bid1.amount_of_money}),
            val bi1 = {id: game.bid1.id, bid: b1, status: game.bid1.status, amount_of_money: game.bid1.amount_of_money}
            val bi2 = game.bid2
            val idx = game.id
            val dl = game.deadline
            val bw = game.bidWon
            val gg = game.with("id", idx).with("bid1", bi1).with("bid2", bi2).with("deadline", dl).with("bidWon", bw)
            games' = games.filter(g => g != game).union(Set(gg)),
        },
    }

    action makeBid2(gameID: int): bool = all {
        val willPass = chanceToPass
        willPass == 1,

        size(games.filter(g => g.id == gameID)) > 0,
        val game = oneOf(games.filter(g => g.id == gameID))
        nondet b2 = oneOf(MIN_BID.to(MAX_BID))
        all {
            bothPlayersPicked(game),
            game.deadline == false,
            game.bid2.amount_of_money >= b2,
            bidders' = bidders.replaceAt(game.bid2.id, {id: game.bid2.id, bid: b2, status: game.bid2.status, amount_of_money: game.bid2.amount_of_money}),
            val bi2 = {id: game.bid2.id, bid: b2, status: game.bid2.status, amount_of_money: game.bid2.amount_of_money}
            val bi1 = game.bid1
            val idx = game.id
            val dl = game.deadline
            val bw = game.bidWon
            val gg = game.with("id", idx).with("bid1", bi1).with("bid2", bi2).with("deadline", dl).with("bidWon", bw)
            games' = games.filter(g => g != game).union(Set(gg))
        },
    }

    // Should the money states be updated after the game is over?  It is not defined in a task
    action hitDeadline(gameID: int): bool = all {
        val willPass = chanceToPass
        willPass == 1,

        size(games.filter(g => g.id == gameID)) > 0,
        val game = oneOf(games.filter(g => g.id == gameID))

        all {
            bothPlayersBidded(game),
            game.deadline == false,

            val bi1 = game.bid1
            val bi2 = game.bid2
            val dl = true
            val bw = declareWinner(game.bid1.bid, game.bid2.bid)
            val idx = game.id
            val gg = game.with("id", idx).with("bid1", bi1).with("bid2", bi2).with("deadline", dl).with("bidWon", bw)
            
            games' = games.filter(g => g != game).union(Set(gg)),
            bidders' = bidders
        }
        
    }
    
    action startNewRound(gameID: int): bool = all {
        val willPass = chanceToPass
        willPass == 1,

        size(games.filter(g => g.id == gameID)) > 0,
        val game = oneOf(games.filter(g => g.id == gameID))

        all {
            game.deadline == true,

            // similar as init, but amount of money of each player remains from the past round
            bidders' = List({id: 0, bid: -1, status: "NOT_PLAYING", amount_of_money: bidders[0].amount_of_money}, {id: 1, bid: -1, status: "NOT_PLAYING", amount_of_money: bidders[1].amount_of_money}, {id: 2, bid: -1, status: "NOT_PLAYING", amount_of_money: bidders[2].amount_of_money}, {id: 3, bid: -1, status: "NOT_PLAYING", amount_of_money: bidders[3].amount_of_money}, {id: 4, bid: -1, status: "NOT_PLAYING", amount_of_money: bidders[4].amount_of_money}),
            games' = games.filter(g => g != game)
        }
        
    }

    /*action step = any {
        pickBidders,
        makeBid1,
        makeBid2,
        hitDeadline,
        startNewRound
    }*/
    
}

